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
#' @param save_shp (logical) if TRUE shapefiles of the species range, extent of occurrence and area of
#' occupancy will be written in the working directory, default = FALSE.
#' @param name (character) valid if save_shp TRUE. The name of the shapefile to be exported.
#'
#' @return A named list containing a data.frame with information about the species range, a
#' SpatialPolygon object of the species range in Geographic projection, and the same SpatialPolygon
#' object projected to the Azimuthal equal area projection.
#'
#' @details If threshold_value is provided, argument threshold is ignored.
#'
#' @examples

# Dependencies: sp (SpatialPointsDataFrame, spTransform), rgdal?,
#               raster (buffer, area), maps (map), maptools (map2SpatialPolygons),
#               rgeos (gUnaryUnion, gIntersection, gCentroid)

rangemap_enm <- function(occurrences, model, threshold_value, threshold,
                         save_shp = FALSE, name) {

  # check for errors
  if (missing("model")) {
    stop("model must exist to perform the calculations.")
  }

  if (!missing("threshold_value") | !missing("threshold") | !missing("occurrences")) {
    if (!missing("threshold_value")) {
      binary <- model
      values(binary)[values(binary) < threshold_value] <- 0
      values(binary)[values(binary) >= threshold_value] <- 1
    }else {
      if (!missing("threshold") & !missing("occurrences")) {
        # keeping only coordinates
        occ <- occurrences[, 2:3]

        # threshold value calculation
        o_suit <- raster::extract(model, occ)
        o_suit_sort <- sort(o_suit)
        thres <- o_suit_sort[ceiling(length(occ[, 1]) * threshold / 100) + 1]

        # binarization
        binary <- model
        values(binary)[values(binary) < thres] <- 0
        values(binary)[values(binary) >= thres] <- 1
      }else {
        stop("Parameters threshold and occ.tra, or threshold_value must be defined to perform the calculations.")
      }
    }
  }
  # calculate areas (number of pixels times resolution)


  # return results


}


