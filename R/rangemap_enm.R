#' Species distributional ranges based on ecological niche models
#'
#' @description rangemap_enm generates species range polygons for a given species
#' using a continuous raster layer produced with an ecological niche modeling tool.
#' This function split the model in suitable and unsuitable habitats using a user
#' specified level of error or a given threshold value. Shape files can be saved in
#' the working directory if it is needed.
#'
#' @param occurrences a data.frame containing species occurrences, columns must be:
#' Species, Longitude, and Latitude.
#' @param model a RasterLayer object that will binarized using the threshold value defined
#' by the user or a value calculated based on a threshold (from 0 - 100) defined in threshold.
#' @param threshold_value (numeric) decimal value used for reclasifying the model. This value will
#' be the lowest considered as suitable for the species.
#' @param threshold (numeric) percentage of occurrence records to be excluded from suitable areas
#' considering their values of suitability in the continuous model (e.g., 0, 5, or 10).
#' @param export (logical) if TRUE a shapefile of the species range will be written in the working
#' directory, appart of the returned object.
#'
#' @return A named list containing a data.frame with information about the species range, a
#' SpatialPolygon object of the species range in Geographic projection, and the same SpatialPolygon
#' object projected to the Azimuthal equal area projection.
#'
#' @details If threshold_value is provided, argument threshold is ignored.
#'
#' @examples

# Dependencies: sp (SpatialPointsDataFrame, spTransform),
#               raster (buffer, area), maps (map), maptools (map2SpatialPolygons),
#               rgeos (gUnaryUnion, gIntersection, gCentroid)

rangemap_enm <- function(occurrences, model, threshold_value, threshold, export = FALSE) {

}

# use records with which the enm was created and calculate a threshold
# that can be defined at distinct levels (0%, 5%, 10%, etc.)
# or use a threshold value specified by the user
# threshold the enm
# calculate areas (number of pixels times resolution)
# return results
