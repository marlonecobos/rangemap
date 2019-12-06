#' Species distributional ranges based on trend surface analyses
#'
#' @description rangemap_tsa generates a distributional range for a given species
#' using a trend surface analysis. An approach to the species extent of occurrence
#' (using convex hulls) and the area of occupancy according to the IUCN criteria
#' are also generated. Shapefiles can be saved in the working directory if it is needed.
#'
#' @param occurrences a data.frame containing geographic coordinates of species occurrences,
#' columns must be: Species, Longitude, and Latitude. Geographic coordinates must be in
#' decimal degrees.
#' @param region_of_interest a SpatialPolygon object on which the trend surface analysis
#' will be performed. For instance, a country, an ecoregion, or a biogeogeographical region.
#' Projection must be Geographic (longitude, latitude).
#' @param resolution (numeric) resolution in kilometers in which the resultant surface will
#' be created, default = 5. Rsolution will depend on the size of the area in wich the species
#' is distributed and values lower than 1 are only recomended when the species is narrowly
#' distributed.
#' @param threshold (numeric) percentage of occurrence records to be excluded when deciding
#' the minimum trend surface value to be considered as part of the species range. Default = 0.
#' @param simplify_level (numeric) tolerance at the moment of simplifying polygons created
#' with the trend surface model. Lower values will produce polygons more similar to the original
#' geometry. Default = 0. If simplify is needed, try numbers between 0 and 1 first.
#' @param final_projection (character) string of projection arguments for resulting Spatial objects.
#' Arguments must be as in the PROJ.4 documentation. See \code{\link[sp]{CRS-class}} for details.
#' Default = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" = WGS84.
#' @param save_shp (logical) if TRUE, shapefiles of the species range, occurrences, extent of
#' occurrence and area of occupancy will be written in the working directory. Default = FALSE.
#' @param save_tsmodel (logical) if TRUE, the species tsa model will be included as part of the
#' object (list) returned, and if other shapefiles are also saved, \code{save_shp} = TRUE, the
#' tsa model will be written in GeoTif format in the working directory. Default = FALSE.
#' @param name (character) valid if \code{save_shp} = TRUE. The name of the geographic files
#' to be exported. A suffix will be added to \code{name} depending on the object as follows:
#' species extent of occurrence = "_extent_occ", area of occupancy = "_area_occ", occurrences
#' = "_unique_records", and, if \code{save_tsmodel} = TRUE, trend surface model "_tsa".
#' Default = "range_tsa".
#'
#' @return A named list containing: (1) a data.frame with information about the species range,
#' and SpatialPolygon objects of (2) unique occurrences, (3) species range, (4) extent of
#' occurrence, and (5) area of occurpancy. If \code{save_tsmodel} = TRUE, the (6) tsa model
#' will be included as well. All Spatial objects will be in Azimuthal equal area projection.
#'
#' @details Trend surface analysis is a method based on low-order polynomials of spatial
#' coordinates for estimating a regular grid of points from scattered observations. This
#' method assumes that all cells not occupied by occurrences are absences; hence its use
#' depends on the quality of data and the certainty of having or not a complete sampling
#' of the \code{regiong_of_interest}.
#'
#' @usage
#' rangemap_tsa(occurrences, region_of_interest, resolution = 5, threshold = 0,
#'     simplify_level = 0, save_shp = FALSE, save_tsmodel = FALSE,
#'     name = "range_tsa")
#'
#' @export
#'
#' @importFrom sp CRS SpatialPointsDataFrame SpatialPolygonsDataFrame coordinates
#' @importFrom sp SpatialPolygons Polygons Polygon proj4string over spTransform
#' @importFrom raster disaggregate area extent rasterize res values mask
#' @importFrom raster writeRaster extract rasterToPolygons
#' @importFrom rgeos gCentroid gUnaryUnion gIntersection
#' @importFrom rgdal writeOGR
#' @importFrom spatial surf.ls predict.trls
#'
#' @examples
#' if(!require(rgbif)){
#'   install.packages("rgbif")
#'   library(rgbif)
#' }
#' if(!require(maps)){
#'   install.packages("maps")
#'   library(maps)
#' }
#' if(!require(maptools)){
#'  install.packages("maptools")
#'  library(maptools)
#' }
#'
#' # getting the data from GBIF
#' species <- name_lookup(query = "Peltophryne taladai",
#'                        rank="species", return = "data") # information about the species
#'
#' #occ_count(taxonKey = species$key[7], georeferenced = TRUE) # testing if keys return records
#'
#' key <- species$key[7] # using species key that return information
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
#' res <- 5
#' thr <- 0
#' save <- TRUE
#' name <- "test"
#'
#' tsa <- rangemap_tsa(occurrences = occ_g, region_of_interest = reg,
#'                     threshold = thr, resolution = res, save_shp = save,
#'                     name = name)
#'
#' # see the species range in a figure
#' extent <- TRUE
#' occ <- TRUE
#' legend <- TRUE
#' north <- TRUE
#'
#' # creating the species range figure
#' rangemap_fig(tsa, add_extent = extent, add_occurrences = occ,
#'              legend = legend, northarrow = north, legend_position = "bottomleft")

rangemap_tsa <- function(occurrences, region_of_interest, resolution = 5, threshold = 0,
                         simplify_level = 0, final_projection, save_shp = FALSE,
                         save_tsmodel = FALSE, name = "range_tsa") {
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
  if (threshold > 0) {
    warning("Since threshold > 0, some occurrences may be excluded form the species range.")
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

  tsa_pol <- suppressWarnings(rgeos::gSimplify(tsa_pol, tol = simplify_level)) # simplify polygons

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
  c_hull_extent <- rgeos::gIntersection(region, covexhull_polygon_pr, byid = TRUE, drop_lower_td = TRUE) # area of interest

  eockm2 <- raster::area(c_hull_extent) / 1000000
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

  extent_occurrence <- sp::SpatialPolygonsDataFrame(c_hull_extent, # extent of occurrence
                                                    data = data.frame(species,
                                                                      eockm2),
                                                    match.ID = FALSE)

  area_occupancy <- sp::SpatialPolygonsDataFrame(grid_sp, data = data.frame(species, # area of occupancy
                                                                            aockm2),
                                                 match.ID = FALSE)

  # reprojection
  if (missing(final_projection)) {
    final_projection <- WGS84
  } else {
    final_projection <- sp::CRS(final_projection) # character to projection
  }

  clip_area <- sp::spTransform(clip_area, final_projection)
  extent_occurrence <- sp::spTransform(extent_occurrence, final_projection)
  area_occupancy <- sp::spTransform(area_occupancy, final_projection)
  occ_pr <- sp::spTransform(occ_pr, final_projection)

  # exporting
  if (save_shp == TRUE) {
    cat("Writing shapefiles in the working directory.")
    rgdal::writeOGR(clip_area, ".", name, driver = "ESRI Shapefile")
    rgdal::writeOGR(extent_occurrence, ".", paste(name, "extent_occ", sep = "_"), driver = "ESRI Shapefile")
    rgdal::writeOGR(area_occupancy, ".", paste(name, "area_occ", sep = "_"), driver = "ESRI Shapefile")
    rgdal::writeOGR(occ_pr, ".", paste(name, "unique_records", sep = "_"), driver = "ESRI Shapefile")
  }

  if (save_tsmodel == TRUE & save_shp == TRUE) {
    cat("Writing trend surface model in the working directory.")
    raster::writeRaster(tsa_model, paste(name, "_tsa.tif", sep = ""), format = "GTiff")
  }

  # return results (list or a different object?)
  sp_dat <- data.frame(occ[1, 1], dim(occ_pr)[1], areakm2, eocckm2, aocckm2) # extent of occ = total area?
  colnames(sp_dat) <- c("Species", "Unique_records", "Range_area", "Extent_of_occurrence", "Area_of_occupancy")

  if (save_tsmodel == TRUE) {
    results <- list(sp_dat, occ_pr, clip_area, extent_occurrence, area_occupancy, tsa_model)
    names(results) <- c("Summary", "Species_unique_records", "Species_range", "Extent_of_occurrence",
                        "Area_of_occupancy", "Trend_surface_model")
  }else {
    results <- list(sp_dat, occ_pr, clip_area, extent_occurrence, area_occupancy)
    names(results) <- c("Summary", "Species_unique_records", "Species_range", "Extent_of_occurrence",
                        "Area_of_occupancy")
  }

  return(results)
}


