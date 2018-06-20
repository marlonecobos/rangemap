#' Species distributional ranges based on a trend surface analysis
#'
#' @description rangemap_tsa generates species range polygons for a given species
#' using a trend surface analysis. An approach to the species extent of occurrence
#' (using convex hulls) and the area of occupancy according to the IUCN criteria
#' are also generated. Shape files can be saved in the working directory if it is needed.
#'
#' @param occurrences a data.frame containing species occurrences, columns must be:
#' Species, Longitude, and Latitude.
#' @param region_of_interest a SpatialPolygon object on which the trend surface analysis
#' will be performed. For instance, a country, an ecoregion, or a biogeogeographical region.
#' Projection must be Geographic (longitude, latitude).
#' @param resolution (numeric) resolution in kilometers in which the resultant surface will be created,
#' default = 5. Rsolution will depend on the size of the area in wich the species is distributed
#' and values lower than 1 are only recomended when the species is narrowly distributed.
#' @param threshold (numeric) percentage of occurrence records to be excluded when deciding
#' the minimum surface value to be considered part of the species range, default = 0.
#' @param save_shp (logical) if TRUE shapefiles of the species range, extent of occurrence and area of
#' occupancy will be written in the working directory. Default = FALSE.
#' @param save_tsmodel (logical) if TRUE a GeoTif file of the species tsa model will be written in
#' the working directory. Default = FALSE.
#' @param name (character) valid if save_shp TRUE. The name of the shapefile and GeoTif (if save_tsmodel
#' = TRUE) to be exported.
#'
#' @return A named list containing a data.frame with information about the species range, a
#' SpatialPolygon object of the species range in Geographic projection, and the same SpatialPolygon
#' object projected to the Azimuthal equal area projection.
#'
#' @details Trend surface analysis Is a method based on low-order polynomials of spatial coordinates
#' for estimating a regular grid of points from scattered observations. This method assumes that all
#' cells not occupied by occurrences are absences; hence its use depends on the quality of data and
#' the certainty of having or not a complete sampling of the regiong_of_interest.
#'
#' @examples
#' if(!require(rgbif)){
#' install.packages("rgbif")
#' library(rgbif)
#' }
#' if(!require(maps)){
#' install.packages("maps")
#' library(maps)
#' }
#' if(!require(maptools)){
#'  install.packages("maptools")
#'  library(maptools)
#' }
#'
#' # getting the data from GBIF
#' species <- name_lookup(query = "Peltophryne fustiger",
#'                        rank="species", return = "data") # information about the species
#'
#' occ_count(taxonKey = species$key[5], georeferenced = TRUE) # testing if keys return records
#'
#' key <- species$key[5] # using species key that return information
#'
#' occ <- occ_search(taxonKey = key, return = "data") # using the taxon key
#'
#' # keeping only georeferenced records
#' occ_g <- occ[!is.na(occ$decimalLatitude) & !is.na(occ$decimalLongitude),
#'              c("name", "decimalLongitude", "decimalLatitude")]
#'
#' # region of interest
#' WGS84 <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
#' w_map <- map(database = "world", regions = "Cuba", fill = TRUE, plot = FALSE) # map of the world
#' w_po <- sapply(strsplit(w_map$names, ":"), function(x) x[1]) # preparing data to create polygon
#' reg <- map2SpatialPolygons(w_map, IDs = w_po, proj4string = WGS84) # map to polygon
#'
#' # other data
#' res <- 1
#' thr <- 0
#' save <- TRUE
#' name <- "test"
#'
#' tsa <- rangemap_tsa(occurrences = occ_g, region_of_interest = reg,
#'                     threshold = thr, resolution = res, save_shp = save,
#'                     name = name)

# Dependencies: maps (map),
#               maptools (map2SpatialPolygons),
#               raster (area, rasterize, extent, raster),
#               rgdal (writeOGR),
#               rgeos (gIntersection, gCentroid, gBuffer),
#               sp (SpatialPointsDataFrame, spTransform, SpatialPolygonsDataFrame,
#                   CRS, over, Polygons, Polygon, SpatialPolygons, proj4string)
#               spatial(surf.ls, predict.trls)

rangemap_tsa <- function(occurrences, region_of_interest, resolution = 5, threshold = 0,
                         save_shp = FALSE, save_tsmodel = FALSE, name) {
  # testing potential issues
  if (missing(occurrences)) {
    stop("Argument occurrences is necessary to perform the analysis")
  }

  if (dim(occurrences)[2] != 3) {
    stop("occurrences data.frame must have the following columns: \nSpecies, Longitude, and Latitude")
  }

  if (missing(region_of_interest)) {
    stop("Argument region_of_interest is necessary to perform the analysis")
  }

  # erase duplicate records
  occ <- as.data.frame(unique(occurrences))[, 1:3]
  colnames(occ) <- c("Species", "Longitude", "Latitude")

  # making spatial points
  WGS84 <- sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  occ_sp <- sp::SpatialPointsDataFrame(coords = occ[, 2:3], data = occ,
                                       proj4string = WGS84)

  # region of interest
  region <- region_of_interest

  # keeping only records in land
  occ_sp <- occ_sp[!is.na(sp::over(occ_sp, region)), ]

  # project the points using their centriods as reference
  centroid <- rgeos::gCentroid(occ_sp, byid = FALSE)

  AEQD <- sp::CRS(paste("+proj=aeqd +lat_0=", centroid@coords[2], " +lon_0=", centroid@coords[1],
                        " +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs", sep = ""))

  occ_pr <- sp::spTransform(occ_sp, AEQD)

  # region of interest projected
  region <- sp::spTransform(region, AEQD)

  # preparing variables
  ## creating a grid
  grid <- raster::raster(raster::extent(region))

  ## grid resolution and values
  raster::res(grid) <- resolution * 1000
  raster::values(grid) <- 0

  ## grid projection
  sp::proj4string(grid) <- sp::proj4string(region)

  ## extract grid with region
  grid_reg <- raster::mask(grid, region)

  ## grid for region of interest
  grid_r_pol <- raster::rasterToPolygons(grid_reg)

  ## points for region of interest
  matrix_a <- raster::rasterToPoints(grid_reg)

  ## selecting grids with occurrences
  grid_pres <- grid_r_pol[!is.na(sp::over(grid_r_pol, as(occ_pr, "SpatialPoints"))), ]

  ## grid to points
  ras_grid <- raster::rasterize(grid_pres, grid_reg, "layer")
  point_pres <- raster::rasterToPoints(ras_grid)[, 1:2]

  ## asigning 1 to cells occupied by occurrenes
  matrix_pa <- matrix_a
  matrix_pa[, 3] <- ifelse(paste(matrix_pa[, 1], matrix_pa[, 2], sep = "_") %in%
                             paste(point_pres[, 1], point_pres[, 2], sep = "_"),  1, 0)

  ## data for models
  if (dim(matrix_pa)[1] > 10000 + dim(point_pres)[1]) {
    ma_p <- matrix_pa[matrix_pa[, 3] == 0, ]
    ma_p <- ma_p[sample(nrow(ma_p), 10000), ]
    ma_a <- matrix_pa[matrix_pa[, 3] == 1, ]

    matrix_spa <- rbind(ma_p, ma_a)
  }else {
    matrix_spa <- matrix_pa
  }

  ## variables
  longitude <- matrix_spa[, 1]
  latitude <- matrix_spa[, 2]
  pres_abs <- matrix_spa[, 3]

  project_matrix1 <- data.frame(matrix_pa[, 1:2], matrix_pa[, 1:2])
  names(project_matrix1) <- c("x", "y", "longitude", "latitude")
  sp::coordinates(project_matrix1) <- ~ x + y
  rast_r <- raster::raster(ncol = dim(grid_reg)[2],
                           nrow = dim(grid_reg)[1])
  raster::extent(rast_r) <- raster::extent(grid_reg)
  sp::proj4string(rast_r) <- sp::proj4string(region)

  # tsa
  ## tsa model
  tsa <- spatial::surf.ls(np = 3, x = longitude, y = latitude, z = pres_abs)

  # tsa prediction to region of insterest
  tsa_reg <- spatial::predict.trls(tsa, project_matrix1$longitude, project_matrix1$latitude) # try with raster stack x, y, ...

  tsa_model <- raster::rasterize(project_matrix1, rast_r, tsa_reg)

  # tsa thresholded
  tsa_t <- tsa_model
  occ_val <- na.omit(raster::extract(tsa_t, occ_pr@coords))
  val <- ceiling(length(occ[, 1]) * threshold / 100) + 1
  thres <- sort(occ_val)[val]

  raster::values(tsa_t)[raster::values(tsa_t) < thres] <- 0
  raster::values(tsa_t)[raster::values(tsa_t) >= thres] <- 1

  # only presence
  raster::values(tsa_t)[raster::values(tsa_t) == 0] <- NA

  # tsa to spatial polygon
  tsa_pol <- raster::rasterToPolygons(tsa_t)
  tsa_pol@data$union_field <- rep("Union", length(tsa_pol@data[, 1])) # new field for union
  tsa_pol <- rgeos::gUnaryUnion(tsa_pol, id = tsa_pol@data$union_field) # now dissolve

  # calculate areas in km2
  area <- raster::area(tsa_pol) / 1000000
  areakm2 <- sum(area) # total area of the species range

  ## extent of occurrence
  coord <- as.data.frame(occ[, 2:3]) # spatial point dataframe to data frame keeping only coordinates
  covexhull <- chull(coord) # convex hull from points
  coord_pol <- coord[c(covexhull, covexhull[1]),] # defining coordinates
  covexhull_polygon <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(coord_pol)), ID = 1))) # into SpatialPolygons
  sp::proj4string(covexhull_polygon) <- WGS84 # project
  covexhull_polygon_pr <- sp::spTransform(covexhull_polygon, AEQD) # reproject
  #c_hull_extent <- rgeos::gIntersection(polygons, covexhull_polygon_pr, byid = TRUE, drop_lower_td = TRUE) # area of interest

  eockm2 <- raster::area(covexhull_polygon_pr) / 1000000
  eocckm2 <- sum(eockm2) # total area of the species range

  ## area of occupancy
  grid <- raster::raster(ext = raster::extent(occ_pr) + 10000, res = c(2000, 2000), crs = AEQD)
  raster_sp <- raster::rasterize(occ_pr[, 2:3], grid)[[1]] # raster from points
  grid_sp <- as(raster_sp, "SpatialPolygonsDataFrame") # raster to polygon

  aockm2 <- raster::area(grid_sp) / 1000000
  aocckm2 <- sum(aockm2) # area calculation

  # adding characteristics to spatial polygons
  species <- as.character(occurrences[1, 1])
  clip_area <- sp::SpatialPolygonsDataFrame(tsa_pol, data = data.frame(species, areakm2, # species range
                                                                       eocckm2, aocckm2),
                                            match.ID = FALSE)

  extent_occurrence <- sp::SpatialPolygonsDataFrame(covexhull_polygon_pr, # extent of occurrence
                                                    data = data.frame(species,
                                                                      eockm2),
                                                    match.ID = FALSE)

  area_occupancy <- sp::SpatialPolygonsDataFrame(grid_sp, data = data.frame(species, # area of occupancy
                                                                            aockm2),
                                                 match.ID = FALSE)

  # exporting
  if (save_shp == TRUE) {
    cat("Writing shapefiles in the working directory.")
    rgdal::writeOGR(clip_area, ".", name, driver = "ESRI Shapefile")
    rgdal::writeOGR(extent_occurrence, ".", paste(name, "extent_occ", sep = "_"), driver = "ESRI Shapefile")
    rgdal::writeOGR(area_occupancy, ".", paste(name, "area_occ", sep = "_"), driver = "ESRI Shapefile")
    rgdal::writeOGR(occ_pr, ".", paste(name, "unique_records", sep = "_"), driver = "ESRI Shapefile")
  }

  if (save_tsmodel == TRUE) {
    cat("Writing trend surface model in the working directory.")
    raster::writeRaster(tsa_model, paste(name, "_tsa.tif", sep = ""), format = "GTiff")
  }

  # return results (list or a different object?)
  sp_dat <- data.frame(occ[1, 1], dim(occ_pr)[1], areakm2, eocckm2, aocckm2) # extent of occ = total area?
  colnames(sp_dat) <- c("Species", "Unique records", "Range area", "Extent of occurrence", "Area of occupancy")

  if (save_tsmodel == TRUE) {
    results <- list(sp_dat, occ_pr, clip_area, extent_occurrence, area_occupancy, tsa_model)
    names(results) <- c("Summary", "Species unique records", "Species range", "Extent of occurrence",
                        "Area of occupancy", "Trend surface model")
  }else {
    results <- list(sp_dat, occ_pr, clip_area, extent_occurrence, area_occupancy)
    names(results) <- c("Summary", "Species unique records", "Species range", "Extent of occurrence",
                        "Area of occupancy")
  }

  return(results)
}


