#' Species distributional ranges based on buffered occurrences
#'
#' @description rangemap_buff generates SpatialPolygonsDataFrame objects of a species range, and
#' extent of occurrence and area of occupancy according to the UICN criteria. The species
#' range is calculated by buffering provided occurrences using a defined distance. Shapefiles
#' of the results can be saved in the working directory if it is needed.
#'
#' @param occurrences a data.frame containing species occurrences, columns must be:
#' Species, Longitude, and Latitude. Geographic coordinates must be in decimal degrees
#' @param distance (numeric) distance, in meters, to be used for creating the buffer areas
#' around occurrences, default = 100000.
#' @param polygons (optional) a SpatialPolygon object that will be clipped with the buffer areas
#' to create species ranges based on actual limits. Projection must be Geographic (longitude, latitude).
#' If not defined, a default, simple world map will be used.
#' @param save_shp (logical) if TRUE shapefiles of the species range, extent of occurrence and area of
#' occupancy will be written in the working directory.
#' @param name (character) valid if save_shp TRUE. The name of the shapefile to be exported.
#'
#' @return A named list containing a data.frame with information about the species range and
#' SpatialPolygonDataFrame objects of the species range, extent of occurrence, and area of occupancy;
#' all in Azimuthal equal area projection. If save_shp = TRUE, written shapefiles' projections
#' will be the same as the SpatialPolygonDataFrame objects.
#'
#' @examples
#' if(!require(rgbif)){
#' install.packages("rgbif")
#' library(rgbif)
#' }
#'
#' # getting the data from GBIF
#' occ <- occ_search(taxonKey = 2422480, return = "data")
#'
#' # keeping only georeferenced records
#' occ_g <- occ[!is.na(occ$decimalLatitude) & !is.na(occ$decimalLongitude),
#'             c("name", "decimalLongitude", "decimalLatitude")]
#'
#' # buffer distance
#' dist <- 100000
#' save <- TRUE
#' name <- "test"
#'
#' buff_range <- rangemap_buff(occurrences = occ_g, distance = dist,
#'                             save_shp = save, name = name)

# Dependencies: maps (map),
#               maptools (map2SpatialPolygons),
#               raster (area, rasterize, extent),
#               rgdal (writeOGR),
#               rgeos (gIntersection, gCentroid, gBuffer),
#               sp (SpatialPointsDataFrame, spTransform, SpatialPolygonsDataFrame,
#                   CRS, over, Polygons, Polygon, SpatialPolygons, proj4string)

rangemap_buff <- function(occurrences, distance = 100000, polygons, save_shp = FALSE, name) {
  # testing potential issues
  if (missing(occurrences)) {
    stop("Argument occurrences is necessary to perform the analysis")
  }

  if (dim(occurrences)[2] != 3) {
    stop("occurrences data.frame must have the following columns: \nSpecies, Longitude, and Latitude")
  }

  # erase duplicate records
  occ <- as.data.frame(unique(occurrences))[, 1:3]
  colnames(occ) <- c("Species", "Longitude", "Latitude")

  # make a spatial object from coordinates
  WGS84 <- sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  occ_sp <- sp::SpatialPointsDataFrame(coords = occ[, 2:3], data = occ,
                                   proj4string = WGS84)

  # world map or user map fro creating species range
  if (missing(polygons)) {
    w_map <- maps::map(database = "world", fill = TRUE, plot = FALSE) # map of the world

    w_po <- sapply(strsplit(w_map$names, ":"), function(x) x[1]) # preparing data to create polygon
    polygons <- maptools::map2SpatialPolygons(w_map, IDs = w_po, proj4string = WGS84) # map to polygon
  }

  # keeping only records in land
  occ_sp <- occ_sp[!is.na(sp::over(occ_sp, polygons)), ]

  # project the points using their centriods as reference
  centroid <- rgeos::gCentroid(occ_sp, byid = FALSE)

  AEQD <- sp::CRS(paste("+proj=aeqd +lat_0=", centroid@coords[2], " +lon_0=", centroid@coords[1],
                        " +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs", sep = ""))

  occ_pr <- sp::spTransform(occ_sp, AEQD)

  # project polygons
  polygons <- sp::spTransform(polygons, AEQD)

  # create a buffer based on a user-defined distance
  buff_area <- rgeos::gBuffer(occ_pr, width = distance)

  # clip a world map based on the created buffer
  clip_area <- rgeos::gIntersection(polygons, buff_area, byid = TRUE, drop_lower_td = TRUE)

  # calculate areas in km2
  area_km2 <- sum(raster::area(clip_area) / 1000000) # total area of the species range

  ## extent of occurrence
  coord <- as.data.frame(occ[, 2:3]) # spatial point dataframe to data frame keeping only coordinates
  covexhull <- chull(coord) # convex hull from points
  coord_pol <- coord[c(covexhull, covexhull[1]),] # defining coordinates
  covexhull_polygon <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(coord_pol)), ID = 1))) # into SpatialPolygons
  sp::proj4string(covexhull_polygon) <- WGS84 # project
  covexhull_polygon_pr <- sp::spTransform(covexhull_polygon, AEQD) # reproject
  c_hull_extent <- rgeos::gIntersection(polygons, covexhull_polygon_pr, byid = TRUE, drop_lower_td = TRUE) # area of interest

  ext_occ_area_km2 <- raster::area(c_hull_extent) / 1000000
  extent_occ_km2 <- sum(ext_occ_area_km2) # total area of the species range

  ## area of occupancy
  grid <- raster::raster(ext = raster::extent(occ_pr) + 10000, res = c(2000, 2000), crs = AEQD)
  raster_sp <- raster::rasterize(occ_pr[, 2:3], grid)[[1]] # raster from points
  grid_sp <- as(raster_sp, "SpatialPolygonsDataFrame") # raster to polygon

  area_occ_area_km2 <- raster::area(grid_sp) / 1000000
  area_occ_km2 <- sum(area_occ_area_km2) # area calculation

  # adding characteristics to spatial polygons
  species <- as.character(occurrences[1, 1])
  clip_area <- sp::SpatialPolygonsDataFrame(clip_area, data = data.frame(species, area_km2, # species range
                                                                         extent_occ_km2, area_occ_km2),
                                            match.ID = FALSE)

  extent_occurrence <- sp::SpatialPolygonsDataFrame(c_hull_extent, # extent of occurrence
                                                    data = data.frame(species,
                                                                      ext_occ_area_km2),
                                                    match.ID = FALSE)

  area_occupancy <- sp::SpatialPolygonsDataFrame(grid_sp, data = data.frame(species, # area of occupancy
                                                                            area_occ_area_km2),
                                                 match.ID = FALSE)

  # exporting
  if (save_shp == TRUE) {
    cat("Writing shapefiles in the working directory.")
    rgdal::writeOGR(clip_area, ".", name, driver = "ESRI Shapefile")
    rgdal::writeOGR(extent_occurrence, ".", paste(name, "extent_occ", sep = "_"), driver = "ESRI Shapefile")
    rgdal::writeOGR(area_occupancy, ".", paste(name, "area_occ", sep = "_"), driver = "ESRI Shapefile")
    rgdal::writeOGR(occ_pr, ".", paste(name, "unique_records", sep = "_"), driver = "ESRI Shapefile")
  }

  # return results (list or a different object?)
  sp_dat <- data.frame(occ[1, 1], dim(occ_pr)[1], area_km2, extent_occ_km2, area_occ_km2) # extent of occ = total area?
  colnames(sp_dat) <- c("Species", "Unique records", "Range area", "Extent of occurrence", "Area of occupancy")

  results <- list(sp_dat, occ_pr, clip_area, extent_occurrence, area_occupancy)
  names(results) <- c("Summary", "Species unique records", "Species range", "Extent of occurrence",
                      "Area of occupancy")
  return(results)
}
