#' Species distributional ranges based on a trend surface analysis
#'
#' @description rangemap_tsa generates species range polygons for a given species
#' using .
#'
#' @param occurrences a data.frame containing species occurrences, columns must be:
#' Species, Longitude, and Latitude.
#' @param variables a RasterStack object of environmental variables that will be used for
#' performing the trend surface analysis.
#' @param threshold (numeric) percentage of occurrence records to be excluded when deciding
#' the minimum surface value to be considered part of the species range, default = 5.
#' @param export (logical) if TRUE a shapefile of the species range will be written in the working
#' directory, appart of the returned object.
#'
#' @return A named list containing a data.frame with information about the species range, a
#' SpatialPolygon object of the species range in Geographic projection, and the same SpatialPolygon
#' object projected to the Azimuthal equal area projection.
#'
#' @details Trend surface analysis Is a method based on low-order polynomials of spatial coordinates
#' for estimating a regular grid of points from scattered observations.
#'
#' @examples

# Dependencies: sp (SpatialPointsDataFrame, spTransform), rgdal?,
#               raster (buffer, area), maps (map), maptools (map2SpatialPolygons),
#               rgeos (gUnaryUnion, gIntersection, gCentroid), saptial?

rangemap_tsa <- function(occurrences, variables, threshold = 5, export = FALSE) {
  # perform a tsa using species records and environmental variables
  # give a value to get the area
  # calculate areas (number of pixels times resolution)
  # return results
}


