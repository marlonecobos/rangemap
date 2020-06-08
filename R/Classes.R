# R classes for rangemap objects
# June 2020
# Version 0.1.5
# License GPL v3

#' An S4 class to organize data and results of sp_range* objects
#' @description A list of classes (some of them inherited) to contain information
#' derived from analyses using \code{\link{rangemap}}. Available classes are:
#' sp_range, sp_range_iucn, and sp_range_iucnextra.
#' @name sp_range
#' @aliases sp_range-class sp_range_iucn-class sp_range_iucnextra-class
#' @slot name name depending on how species range was constructed.
#' @slot summary a data.frame.
#' @slot species_unique_records a SpatialPointsDataFrame, not in sp_range.
#' @slot species_range a SpatialPolygonsDataFrame.
#' @slot extent_of_occurrence a SpatialPolygonsDataFrame, not in sp_range.
#' @slot area_of_occupancy a SpatialPolygonsDataFrame, not in sp_range.
#' @slot trend_surface_model a RasterLayer, not in sp_range.
#' @export
#' @importFrom methods new
#' @examples
#' showClass("sp_range")
#' showClass("sp_range_iucn")
#' @rdname sp_range

sp_range <- setClass("sp_range",
                     slots = c(name = "character",
                               summary = "data.frame",
                               species_range = "SpatialPolygonsDataFrame"),
                     prototype = list(name = NA_character_,
                                      summary = data.frame(Species = NA_character_,
                                                           Range_area = NA_real_),
                                      species_range = new("SpatialPolygonsDataFrame")),
                     validity = function(object) {
                       if (ncol(object@summary) < 2) {
                         return("'Summary' in 'sp_range' must contain 2 columns")
                       }
                     })

#' @rdname sp_range
sp_range_iucn <- setClass("sp_range_iucn",
                          slots = c(species_unique_records = "SpatialPointsDataFrame",
                                    extent_of_occurrence = "SpatialPolygonsDataFrame",
                                    area_of_occupancy = "SpatialPolygonsDataFrame"),
                          contains = "sp_range",
                          prototype = list(name = NA_character_,
                                           summary = data.frame(Species = NA_character_,
                                                                Unique_records = NA_integer_,
                                                                Range_area = NA_real_,
                                                                Extent_of_occurrence = NA_real_,
                                                                Area_of_occupancy = NA_real_),
                                           species_unique_records = new("SpatialPointsDataFrame"),
                                           species_range = new("SpatialPolygonsDataFrame"),
                                           extent_of_occurrence = new("SpatialPolygonsDataFrame"),
                                           area_of_occupancy = new("SpatialPolygonsDataFrame")),
                          validity = function(object) {
                            if (ncol(object@summary) < 5) {
                              return("'Summary' in 'sp_range_iucn' must contain 5 columns")
                            }
                          })

#' @rdname sp_range
sp_range_iucnextra <- setClass("sp_range_iucnextra",
                          slots = c(trend_surface_model = "RasterLayer"),
                          contains = "sp_range_iucn",
                          prototype = list(name = NA_character_,
                                           summary = data.frame(Species = NA_character_,
                                                                Unique_records = NA_integer_,
                                                                Range_area = NA_real_,
                                                                Extent_of_occurrence = NA_real_,
                                                                Area_of_occupancy = NA_real_),
                                           species_unique_records = new("SpatialPointsDataFrame"),
                                           species_range = new("SpatialPolygonsDataFrame"),
                                           extent_of_occurrence = new("SpatialPolygonsDataFrame"),
                                           area_of_occupancy = new("SpatialPolygonsDataFrame"),
                                           trend_surface_model = new("RasterLayer"))
                          )
