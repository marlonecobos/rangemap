# R classes for rangemap objects
# June 2020
# Version 0.1.5
# Licence GPL v3

#' An S4 class to organize data and results of sp_range* objects
#' @description A list of classes (some of them inherited) to contain information
#' derived from analyses using \code{\link{rangemap}}. Available classes are:
#' sp_range, sp_range_iucn, and sp_range_iucnextra.
#' @name sp_range
#' @aliases sp_range-class sp_range_iucn-class sp_range_iucnextra-class
#' @slot Summary a data.frame.
#' @slot Species_unique_records ("numeric"), not in sp_range.
#' @slot Species_range a SpatialPolygonsDataFrame.
#' @slot Extent_of_occurrence a SpatialPolygonsDataFrame, not in sp_range.
#' @slot Area_of_occupancy a SpatialPolygonsDataFrame, not in sp_range.
#' @slot Trend_surface_model a RasterLayer, not in sp_range.
#' @export
#' @examples
#' showClass("sp_range")
#' showClass("sp_range_iucn")
#' @rdname sp_range

sp_range <- setClass("sp_range",
                     slots = c(Summary = "data.frame",
                               Species_range = "SpatialPolygonsDataFrame"),
                     prototype = list(Summary = data.frame(Species = NA_character_,
                                                           Range_area = NA_real_),
                                      Species_range = new("SpatialPolygonsDataFrame")),
                     validity = function(object) {
                       if (ncol(object@Summary) < 2) {
                         return("'Summary' in 'sp_range' must contain 2 columns")
                       }
                     })

#' @rdname sp_range
sp_range_iucn <- setClass("sp_range_iucn",
                          slots = c(Species_unique_records = "SpatialPointsDataFrame",
                                    Extent_of_occurrence = "SpatialPolygonsDataFrame",
                                    Area_of_occupancy = "SpatialPolygonsDataFrame"),
                          contains = "sp_range",
                          prototype = list(Summary = data.frame(Species = NA_character_,
                                                                Unique_records = NA_integer_,
                                                                Range_area = NA_real_,
                                                                Extent_of_occurrence = NA_real_,
                                                                Area_of_occupancy = NA_real_),
                                           Species_unique_records = new("SpatialPointsDataFrame"),
                                           Species_range = new("SpatialPolygonsDataFrame"),
                                           Extent_of_occurrence = new("SpatialPolygonsDataFrame"),
                                           Area_of_occupancy = new("SpatialPolygonsDataFrame")),
                          validity = function(object) {
                            if (ncol(object@Summary) < 5) {
                              return("'Summary' in 'sp_range_iucn' must contain 5 columns")
                            }
                          })

#' @rdname sp_range
sp_range_iucnextra <- setClass("sp_range_iucnextra",
                          slots = c(Trend_surface_model = "RasterLayer"),
                          contains = "sp_range_iucn",
                          prototype = list(Summary = data.frame(Species = NA_character_,
                                                                Unique_records = NA_integer_,
                                                                Range_area = NA_real_,
                                                                Extent_of_occurrence = NA_real_,
                                                                Area_of_occupancy = NA_real_),
                                           Species_unique_records = new("SpatialPointsDataFrame"),
                                           Species_range = new("SpatialPolygonsDataFrame"),
                                           Extent_of_occurrence = new("SpatialPolygonsDataFrame"),
                                           Area_of_occupancy = new("SpatialPolygonsDataFrame"),
                                           Trend_surface_model = new("RasterLayer"))
                          )
