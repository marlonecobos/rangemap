#' Species distributional ranges based on distinct hull polygons
#'
#' @description rangemap_hull generates a species range polygon for a given species
#' by considering all the polygons of political entities in which the species has
#' been detected.
#'
#' @param occurrences a data.frame containing species occurrences, columns must be:
#' Species, Longitude, and Latitude.
#' @param hull_type (character) type of hull polygons to be created. Available options are: "convex",
#' "concave", and "alpha" hulls.
#' @param distance (numeric) distance in decimal degrees to be used for creating a buffer area
#' around the hull polygons, default = 0.5.
#' @param split_distance (numeric) distance in decimal degrees that will limit connectivity among
#' hull polygons created with chunks of points separated by long distances, , default = 2.
#' @param export (logical) if TRUE a shapefile of the species range will be written in the working
#' directory, appart of the returned object.
#'
#' @return A named list containing a data.frame with information about the species range, a
#' SpatialPolygon object of the species range in Geographic projection, and the same SpatialPolygon
#' object projected to the Azimuthal equal area projection.
#'
#' @examples
#' if(!require(rgbif)){
#' install.packages("rgbif")
#' library(rgbif)
#' }
#'
#' # getting the data from GBIF
#' occ <- occ_search(taxonKey = 5219426, return = "data", limit = 1000)
#'
#' # keeping only georeferenced records
#' occ_g <- occ[!is.na(occ$decimalLatitude) & !is.na(occ$decimalLongitude),]
#'
#' disolve <- TRUE
#' hull <- "convex"
#' split <- 4
#'
#' hull_range <- rangemap_hull(occurrences = occ_g, hull_type = hull, distance = dist, split_distance = split)

# Dependencies: sp (SpatialPointsDataFrame, spTransform),
#               raster (buffer, area), maps (map), maptools (map2SpatialPolygons),
#               rgeos (gIntersection, gCentroid),

rangemap_hull <- function(occurrences, hull_type = "concave", distance = 0.5, split_distance = 2, export = FALSE) {

}

# erase duplicate records
# create hulls depending in the user-defined argument (convex, concave, and alpha)
# split hulls based on a user-defined distance
# create a buffer based on a user-defined distance
# clip a world map based on the created buffer (resolution?)
# calculate areas
# return results
