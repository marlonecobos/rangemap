#' Species distributional ranges based on a trend surface analysis
#'
#' @description rangemap_tsa generates species range polygons for a given species
#' using .
#'
#' @param occurrences a data.frame containing species occurrences, columns must be:
#' Species, Longitude, and Latitude.
#' @param region_of_interest a SpatialPolygon object on which the trend surface analysis
#' will be performed.
#' @param resolution (numeric) resolution in km in which the resultant surface will be created,
#' default = 5.
#' @param threshold (numeric) percentage of occurrence records to be excluded when deciding
#' the minimum surface value to be considered part of the species range, default = 0.
#' @param save_shp (logical) if TRUE shapefiles of the species range, extent of occurrence and area of
#' occupancy will be written in the working directory.
#' @param name (character) valid if save_shp TRUE. The name of the shapefile to be exported.
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

# Dependencies: sp (SpatialPointsDataFrame, spTransform), rgdal?,
#               raster (buffer, area), maps (map), maptools (map2SpatialPolygons),
#               rgeos (gUnaryUnion, gIntersection, gCentroid), saptial?

rangemap_tsa <- function(occurrences, region_of_interest, threshold = 0, save_shp = FALSE, name) {
  # perform a tsa using species records and environmental variables
  # give a value to get the area
  # calculate areas (number of pixels times resolution)
  # return results
}


