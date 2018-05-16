#' Species range maps based on buffered occurrences
#'
#' @description rangemap_buff generates a species range polygon for a given species
#' by buffering provided occurrences using a defined distance. Shape files can be saved
#' in the working directory if it is needed.
#'
#' @param occurrences a data.frame containing species occurrences, columns must be:
#' Species, Longitude, and Latitude.
#' @param distance (numeric) distance in decimal degrees to be used for creating the buffer areas
#' around occurrences, default = 1.
#' @param polygons (optional) a SpatialPolygon object that will be clipped with the buffer areas
#' to create species ranges based on actual limits. If not defined, a default world map will be
#' used.
#' @param export (logical) if TRUE a shapefile of the species range will be written in the working
#' directory, appart of the returned object.
#'
#' @return A named list containing a data.frame with information about the species range, a
#' SpatialPolygon object of the species range in latlong projection, and the same SpatialPolygon
#' object projected to the azimuthal equal area projection.
#'
#' @examples
#' if(!require(rgbif)){
#' install.packages("rgbif")
#' library(rgbif)
#' }
#'
#' # getting the data from GBIF
#' occ <- occ_search(taxonKey = pek[1], return = "data")
#'
#' # keeping only georeferenced records
#' occ_g <- pe_dat[!is.na(pe_dat$decimalLatitude) & !is.na(pe_dat$decimalLongitude),]
#'
#' # buffer distance
#' dist <- 0.5
#'
#' buff_range <- rangemap_buff(occurrences = occ_g, distance = dist)

# dependencies: sp (SpatialPointsDataFrame, spTransform),
#               raster (buffer, area), maps (map), maptools (map2SpatialPolygons),
#               rgeos (gIntersection, gCentroid)

rangemap_buff <- function(occurrences, distance = 1, polygons, export = FALSE) {

  # erase duplicate records
  occ <- occurrences[row.names(unique(occurrences[, 2:3])), ]

  # make a spatial object from coordinates
  occ_sp <- sp::SpatialPointsDataFrame(coords = occ[, 2:3], data = occ,
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

  # create a buffer based on a user-defined distance
  buff_area <- raster::buffer(occ_sp, width = distance)

  # disolve polygons


  # world map or user map
  if (missing(polygons)) {
    w_map <- maps::map(database = "world", fill = TRUE, plot = FALSE) # map of the world

    w_po <- sapply(strsplit(w_map$names, ":"), function(x) x[1]) # preparing data to create polygon
    polygons <- maptools::map2SpatialPolygons(w_map, IDs = w_po, proj4string = crs) # map to polygon
  }

  # clip a world map based on the created buffer (resolution?)
  clip_area <- rgeos::gIntersection(polygons, buff_area, byid = TRUE, drop_lower_td = TRUE)

  # reproject (change lat long based on the centroid of the polygon)
  ## centroid calculation
  centroid <- rgeos::gCentroid(sids, byid = FALSE)

  ## projecting
  clip_area_pro <- sp::spTransform(clip_area, CRS(paste("+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))

  # calculate areas
  area_a <- raster::area(clip_area_pro) # check this

  area_t <- sum(area_a[, 4]) # check this

  area_o <- length(occ[, 1]) * 4 # area of occupancy separated more than 4 km (make a grid for this?)

  area_e <- ### extent of occurrence

  # return results (list or a different object?)
  sp_dat <- data.frame(occ[1, 1], length(occ[, 1]), area_t, area_o, area_e) # extent of occ = total area?
  names(sp_dat) <- c("Species", "Individual records", "Total area", "Area of occupancy", "Extent of occurrence")

  results <- list(sp_dat, clip_area, area_a)
  return(results)
}
