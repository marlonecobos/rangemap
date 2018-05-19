#' Species distributional ranges based on buffered occurrences
#'
#' @description rangemap_buff generates a species range polygon for a given species
#' by buffering provided occurrences using a defined distance. Shape files can be saved
#' in the working directory if it is needed.
#'
#' @param occurrences a data.frame containing species occurrences, columns must be:
#' Species, Longitude, and Latitude. Geographic coordinates must be in decimal degrees
#' @param distance (numeric) distance in meters to be used for creating the buffer areas
#' around occurrences, default = 100000.
#' @param polygons (optional) a SpatialPolygon object that will be clipped with the buffer areas
#' to create species ranges based on actual limits. Projection must be If not defined, a default world map will be
#' used.
#' @param export (logical) if TRUE a shapefile of the species range will be written in the working
#' directory, appart of the returned object.
#' @param name (character) valid if export TRUE. The name of the shapefile to be exported.
#'
#' @return A named list containing a data.frame with information about the species range and a
#' SpatialPolygonDataFrame object of the species range in Azimuthal equal area projection.
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
#'
#' buff_range <- rangemap_buff(occurrences = occ_g, distance = dist)

# Dependencies: sp (SpatialPointsDataFrame, spTransform),
#               raster (buffer, area), maps (map), maptools (map2SpatialPolygons),
#               rgeos (gUnaryUnion, gIntersection, gCentroid)

rangemap_buff <- function(occurrences, distance = 100000, polygons, export = FALSE, name) {
  # testing potential issues
  if (missing(occurrences)) {
    stop("Argument occurrences is necessary to perform the analysis")
  }

  if (dim(occurrences)[2] != 3) {
    stop("occurrences data.frame must have the following columns: \nSpecies, Longitude, and Latitude")
  }

  if (!missing(polygons)) {
    if (condition) {
      # stop if projection is not the one that is needed or only project it?
    }
  }

  # erase duplicate records
  occ <- unique(occurrences)

  # make a spatial object from coordinates
  WGS84 <- sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  occ_sp <- sp::SpatialPointsDataFrame(coords = occ[, 2:3], data = occ,
                                   proj4string = WGS84)

  # project the points using their centriods as reference
  centroid <- rgeos::gCentroid(occ_sp, byid = FALSE)

  AEQD <- sp::CRS(paste("+proj=aeqd +lat_0=", centroid@coords[2], " +lon_0=", centroid@coords[1],
                        " +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs", sep = ""))

  occ_pr <- sp::spTransform(occ_sp, AEQD)

  # create a buffer based on a user-defined distance
  buff_area <- rgeos::gBuffer(occ_pr, width = distance)

  # world map or user map fro creating species range
  if (missing(polygons)) {
    w_map <- maps::map(database = "world", fill = TRUE, plot = FALSE) # map of the world

    w_po <- sapply(strsplit(w_map$names, ":"), function(x) x[1]) # preparing data to create polygon
    polygons <- maptools::map2SpatialPolygons(w_map, IDs = w_po, proj4string = WGS84) # map to polygon
  }

  # project polygons
  polygons <- sp::spTransform(polygons, AEQD)

  # clip a world map based on the created buffer
  clip_area <- rgeos::gIntersection(polygons, buff_area, byid = TRUE, drop_lower_td = TRUE)

  # reproject to WGS84?
  clip_area_pro <- sp::spTransform(clip_area, WGS84)

  # calculate areas in km2
  area_km2 <- raster::area(clip_area) / 1000000 # total area of the species range

  extent_occ <- a### extent of occurrence

  area_occ <- length(occ[, 1]) * 4 # area of occupancy separated more than 4 km (make a grid for this?)

  # adding areas to species range
  clip_area <- sp::SpatialPolygonsDataFrame(clip_area, data = data.frame(area_km2, extent_occ, area_occ),
                                            match.ID = FALSE)

  #exporting
  if (export = TRUE) {
    rgdal::writeOGR(clip_area, ".", name, driver="ESRI Shapefile")
  }

  # return results (list or a different object?)
  sp_dat <- data.frame(occ[1, 1], length(occ[, 1]), area_t, area_o, area_e) # extent of occ = total area?
  names(sp_dat) <- c("Species", "Individual records", "Total area", "Area of occupancy", "Extent of occurrence")

  results <- list(sp_dat, clip_area)
  return(results)
}

