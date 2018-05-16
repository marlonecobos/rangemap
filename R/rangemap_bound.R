#' Species distributional ranges based on political boundaries
#'
#' @description rangemap_bound generates a species range polygon for a given species
#' by considering all the polygons of political entities in which the species has
#' been detected. Shape files can be saved in the working directory if it is needed.
#'
#' @param occurrences a data.frame containing species occurrences, columns must be:
#' Species, Longitude, and Latitude.
#' @param boundaries (character) type of boundaries to use from the available: "countries",
#' "states" for USA and CAN only, "counties" for USA, "departments" and "provinces" for other
#' countries; default = "countries".
#' @param polygons (optional) a SpatialPolygon object that will be used instead of boundaries
#' to create species ranges based on overlapping of species records with these layer. If defined,
#' argument boundaries will not be considered.
#' @param disolve (logical) if TRUE distint polygons selected as part of the species range will
#' be disolved for creating simpler polygons, default = TRUE.
#' @param export (logical) if TRUE a shapefile of the species range will be written in the working
#' directory, appart of the returned object.
#'
#' @return A named list containing a data.frame with information about the species range, a
#' SpatialPolygon object of the species range in Geographic projection, and the same SpatialPolygon
#' object projected to the Azimuthal equal area projection.
#'
#' @details Boundaries used are loaded using the \code{\link[GADMTools]{gadm.loadCountries}} funcion.
#'
#' @examples
#' if(!require(rgbif)){
#' install.packages("rgbif")
#' library(rgbif)
#' }
#'
#' # getting the data from GBIF
#' occ <- occ_search(taxonKey = 2440788, return = "data")
#'
#' # keeping only georeferenced records
#' occ_g <- occ[!is.na(occ$decimalLatitude) & !is.na(occ$decimalLongitude),]
#'
#' disolve <- TRUE
#'
#' bound_range <- rangemap_bound(occurrences = occ_g, boundaries = "countries", disolve = disolve)

# Dependencies: sp (SpatialPointsDataFrame, spTransform),
#               raster (buffer, area), maps (map), maptools (map2SpatialPolygons),
#               rgeos (gIntersection, gCentroid), GADMTools (gadm.loadCountries)

rangemap_bound <- function(occurrences, boundaries = "countries", polygons, disolve = TRUE, export = FALSE) {

}

# erase duplicate records
# select polygons based on records overlap
# poligons can be defined by the user and can also be loaded by the user
# calculate areas
# return results
