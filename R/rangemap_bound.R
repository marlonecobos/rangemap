#' Species distributional ranges based on administrative boundaries
#'
#' @description rangemap_boundaries generates a distributional range for a given
#' species by considering all the polygons of administrative entities in which
#' the species has been detected. Optionally, representations of the species
#' extent of occurrence (using convex hulls) and the area of occupancy according
#' to the IUCN criteria can also be generated. Shapefiles can be saved in the
#' working directory if it is needed.
#'
#' @param occurrences (optional) a data.frame containing geographic coordinates
#' of species occurrences, columns must be: Species, Longitude, and Latitude.
#' Geographic coordinates must be in decimal degrees (WGS84). If not defined,
#' \code{adm_areas} must be defined and these areas will be used as part of the
#' species range.
#' @param adm_areas (optional, character) a vector of names of administrative
#' areas known to be occupied by the species, names depend on the
#' \code{boundary_level} selected. Check \code{\link{adm_area_names}}
#' for an idea of how to define names in this parameter. If not
#' defined, \code{occurrences} must exist.
#' @param country_code (optional, character) vector of country codes that will be
#' considered when creating the species range. Including neighbor countries may
#' be necessary to obtain better results. Use \code{\link{rangemap_explore}}
#' for a preview of all potential countries involved in the analysis. Codes
#' follow the ISO-3166-1 norm as in function \code{\link[raster]{getData}}.
#' If not defined, \code{polygons} must be included. Ignored if \code{polygons}
#' is provided.
#' @param boundary_level (numeric) level of the administrative boundaries (from
#' 0 to 2; 0 is the country level and higher values indicate finer divisions).
#' Default = 0. Ignored if \code{polygons} is defined.
#' @param polygons (optional) a SpatialPolygonsDataFrame object that will be used
#' instead of boundaries specified in \code{country_code} to create species
#' ranges based on overlapping of species records with these layer, as well as
#' names defined in \code{adm_areas}. Projection must be WGS84 (EPSG:4326).
#' If \code{adm_areas} is defined, \code{polygons} must have, as part of its
#' data, a field (column) named "adm_names" for selecting extra areas based on
#' names. If \code{polygons} is defined, arguments \code{country_code} and
#' \code{boundary_level} will be ignored.
#' @param extent_of_occurrence (logical) whether to obtain the extent of occurrence
#' of the species based on a simple convex hull polygon; default = \code{TRUE}.
#' @param area_of_occupancy (logical) whether to obtain the area of occupancy
#' of the species based on a simple grid of 4 km^2 resolution;
#' default = \code{TRUE}.
#' @param keep_data (logical) if \code{TRUE} and \code{polygons} is not defined, data
#' downloaded from the GADM data base will be kept in the working directory. Useful
#' if all or part of the downloaded files will be used in posterior analyses since
#' those files will not be downloaded again and time will be saved. Default =
#' \code{FALSE}.
#' @param dissolve (logical) if \code{TRUE}, distinct polygons selected as part of the
#' species range will be dissolved for creating simpler shapes, default = \code{FALSE}.
#' Owing to the high resolution in the GADM data the dissolving process may be
#' time consuming, specially if the species has a broad
#' distribution.
#' @param final_projection (character) string of projection arguments for resulting
#' Spatial objects. Arguments must be as in the PROJ.4 documentation. See
#' \code{\link[sp]{CRS-class}} for details. If \code{NULL}, the default, projection
#' used is WGS84 (EPSG:4326).
#' @param save_shp (logical) if \code{TRUE}, shapefiles of the species range,
#' occurrences, extent of occurrence, and area of occupancy will be written in
#' the working directory. Default = \code{FALSE}.
#' @param name (character) valid if \code{save_shp} = \code{TRUE}. The name of
#' the shapefile to be exported. A suffix will be added to \code{name} depending
#' on the object, as follows: species extent of occurrence = "_extent_occ", area
#' of occupancy = "_area_occ", and occurrences = "_unique_records".
#' @param overwrite (logical) whether or not to overwrite previous results with
#' the same name. Default = \code{FALSE}.
#' @param verbose (logical) whether or not to print messages about the process.
#' Default = TRUE.
#'
#' @return
#' A sp_range object (S4) containing: (1) a data.frame with information about
#' the species range, and SpatialPolygons objects of (2) unique occurrences,
#' (3) species range, (4) extent of occurrence, and (5) area of occupancy.
#'
#' If only \code{adm_areas} are defined, the result will be a sp_range object
#' (S4) with two elements: (1) a data.frame with information about the species
#' range, and (2) a SpatialPolygons object of the species range.
#'
#' If \code{extent_of_occurrence} and/or \code{area_of_occupancy} = \code{FALSE},
#' the corresponding spatial objects in the resulting sp_range object will be
#' empty, an areas will have a value of 0.
#'
#' If downloading data based on \code{country_code} fails, the result is
#' \code{NULL}.
#'
#' @details
#' Data for countries defined in \code{country_code} are downloaded and loaded
#' using the function \code{\link[raster]{getData}}. Information about country
#' codes and names of administrative areas, at distinct levels, can be consulted
#' using: \code{\link{country_codes}} and \code{\link{adm_area_names}}.
#'
#' @usage
#' rangemap_boundaries(occurrences, adm_areas, country_code, boundary_level = 0,
#'                     polygons, extent_of_occurrence = TRUE,
#'                     area_of_occupancy = TRUE, keep_data = FALSE,
#'                     dissolve = FALSE, final_projection, save_shp = FALSE,
#'                     name, overwrite = FALSE, verbose = TRUE)
#'
#' @export
#'
#' @importFrom sp CRS SpatialPointsDataFrame SpatialPolygonsDataFrame
#' @importFrom sp SpatialPolygons Polygons Polygon proj4string over spTransform
#' @importFrom raster getData disaggregate area extent rasterize
#' @importFrom rgeos gCentroid gUnaryUnion gIntersection
#' @importFrom rgdal writeOGR
#'
#' @examples
#' \donttest{
#' # getting the data
#' data("occ_d", package = "rangemap")
#'
#' # checking which countries may be involved in the analysis
#' rangemap_explore(occurrences = occ_d)
#'
#' # preparing arguments
#' level <- 0
#' adm <- "Ecuador" # Athough no record is on this country, we know it is in Ecuador
#' countries <- c("PER", "BRA", "COL", "VEN", "ECU", "GUF", "GUY", "SUR", "BOL")
#'
#' # running using occurrence data
#' b_range <- rangemap_boundaries(occurrences = occ_d, adm_areas = adm,
#'                                country_code = countries, boundary_level = level)
#'
#' summary(b_range)
#'
#' # running using only names of areas
#' adm1 <- c("Brazil", "Ecuador", "Peru", "Bolivia", "Colombia", "Venezuela")
#' b_range1 <- rangemap_boundaries(adm_areas = adm1, country_code = countries,
#'                                 boundary_level = level)
#'
#' summary(b_range1)
#' }

rangemap_boundaries <- function(occurrences = NULL, adm_areas = NULL,
                                country_code = NULL, boundary_level = 0,
                                polygons = NULL, extent_of_occurrence = TRUE,
                                area_of_occupancy = TRUE, keep_data = FALSE,
                                dissolve = FALSE, final_projection = NULL,
                                save_shp = FALSE, name, overwrite = FALSE,
                                verbose = TRUE) {
  # testing potential issues
  if (!is.null(polygons)) {
    if (class(polygons) != "SpatialPolygonsDataFrame"){
      stop("If defined, 'polygons' must be a SpatialPolygonsDataFrame object. If 'adm_areas'",
           "\nis defined, one of the fields (columns) of polygons data must be named 'adm_areas'",
           "\nand contain names of administrative areas for selection.")
    }
  }

  if (is.null(occurrences) & is.null(adm_areas)) {
    stop("'occurrences' and/or 'adm_areas' must exist to perform the analysis.")
  } else {
    if (!is.null(occurrences) & is.null(adm_areas)) {
      if (dim(occurrences)[2] != 3) {
        stop("'occurrences' must have the following columns: \nSpecies, Longitude, Latitude.")
      }
    }
    if (!is.null(occurrences) & !is.null(adm_areas)) {
      if (dim(occurrences)[2] != 3) {
        occurrences <- NULL
        warning("'occurrences' does not have the arrangment required:",
                "\nSpecies, Longitude, Latitude.", " Species range will be",
                "\ncreated using 'adm_areas' only.")
      }
      if (!is.null(polygons)) {
        if(sum("adm_names" %in% names(polygons@data)) == 0) {
          stop("Data of 'polygons' does not contain a field (column) named 'adm_names', see help.")
        }
      }
    }

    if (!is.null(adm_areas)) {
      if (!is.null(polygons)) {
        if(sum("adm_names" %in% names(polygons@data)) == 0) {
          stop("Data of 'polygons' does not contain a field (column) named 'adm_names', see help.")
        }

        polynam <- polygons@data
        a_a_names <- as.vector(unique(polynam[, "adm_names"]))

        if (sum(adm_areas %in% a_a_names) != length(adm_areas)) {
          warning("Not all of the administrative areas defined in 'adm_names' coincide",
                  "\nwith the names available for administrative areas in 'polygons',",
                  "\nonly those that coincide will be used.")
        }

        if (sum(adm_areas %in% a_a_names) == 0 & is.null(occurrences)) {
          stop("None of the administrative areas defined in 'adm_names' coincides",
               "\nwith the names available for administrative areas in 'polygons'.")
        }

        if (sum(adm_areas %in% a_a_names) == 0 & !is.null(occurrences)) {
          adm_areas <- NULL
          warning("None of the administrative areas defined in 'adm_names' coincides",
                  "\nwith the names available for administrative areas in 'polygons'.",
                  "\nSpecies range will be created using occurrences only.")
        }
      } else {
        data("adm_area_names", package = "rangemap", envir = environment())
        a_names <- paste("NAME", boundary_level, sep = "_")
        a_a_names <- as.vector(unique(adm_area_names[adm_area_names$ISO3 %in% country_code,
                                                     a_names]))

        if (sum(adm_areas %in% a_a_names) != length(adm_areas)) {
          warning("Not all of the administrative areas defined in 'adm_names' coincide",
                  "\nwith the names available for that level in the countries listed",
                  "\nin 'country_code'.")
        }

        if (sum(adm_areas %in% a_a_names) == 0 & is.null(occurrences)) {
          stop("None of the administrative areas defined in 'adm_names' coincides",
               "\nwith the names available for that level in the countries listed",
               "\nin 'country_code'.")
        }

        if (sum(adm_areas %in% a_a_names) == 0 & !is.null(occurrences)) {
          adm_areas <- NULL
          warning("None of the administrative areas defined in 'adm_names' coincides",
                  "\nwith the names available for that level in the countries listed",
                  "\nin 'country_code'. Species range will be created using occurrences only.")
        }
      }
    }
  }

  # initial projection
  WGS84 <- sp::CRS("+init=epsg:4326")

  # final projection
  if (is.null(final_projection)) {
    final_projection <- WGS84
  } else {
    final_projection <- sp::CRS(final_projection) # character to projection
  }

  # world map or user map fro creating species range
  if (is.null(polygons)) {
    poly <- GADM_spoly(country_code, boundary_level, keep_data)

    if (is.null(poly)) {
      return(NULL)
    }
  } else {
    # project polygons
    poly <- sp::spTransform(polygons, WGS84)
  }

  # anlysis
  if (!is.null(occurrences)) {
    # erase duplicate records
    occ <- as.data.frame(unique(occurrences))[, 1:3]
    colnames(occ) <- c("Species", "Longitude", "Latitude")

    # make a spatial object from coordinates
    occ_sp <- sp::SpatialPointsDataFrame(coords = occ[, 2:3], data = occ,
                                         proj4string = WGS84)

    # keeping only records in land
    occ_sp <- occ_sp[poly, ]

    # centriods of points as reference
    LAEA <- LAEA_projection(spatial_object = occ_sp)

    # reproject occurrences
    occ_pr <- sp::spTransform(occ_sp, LAEA)
  } else {
    LAEA <- LAEA_projection(spatial_object = poly)
  }

  # reproject polygons
  polygons <- sp::spTransform(poly, LAEA)

  #selecting polygons
  if (!is.null(occurrences) | !is.null(adm_areas)) {
    if (!is.null(occurrences) & !is.null(adm_areas)) {
      # select polygons that overlap with points
      boundaries <- polygons[occ_pr, ]

      # select polygons by names
      boundaries1 <- polygons[polygons@data$adm_names %in% adm_areas, ]

      # combining boundaries
      boundaries <- sp::rbind.SpatialPolygonsDataFrame(boundaries, boundaries1)
    }
    if (!is.null(occurrences) & is.null(adm_areas)) {
      # select polygons that overlap with points
      boundaries <- polygons[occ_pr, ]
    }

    if (!is.null(adm_areas) & is.null(occurrences)) {
      # select polygons by names
      boundaries <- polygons[polygons@data$adm_names %in% adm_areas, ]
    }
  }

  # disolve
  if (dissolve == TRUE) {
    if (verbose == TRUE) {
      message("\nDissolving polygons, please wait...\n")
    }

    boundaries@data$union_field <- rep("Union", length(boundaries@data[, 1])) # new field for union
    boundaries <- rgeos::gUnaryUnion(boundaries, id = boundaries@data$union_field) # now dissolve
    boundaries <- raster::disaggregate(boundaries)
  }

  # final part
  if (is.null(occurrences)) {
    # calculate areas in km2
    rangekm2 <- raster::area(boundaries) / 1000000
    areakm2 <- sum(rangekm2) # total area of the species range

    # adding characteristics to spatial polygons
    boundaries <- sp::SpatialPolygonsDataFrame(boundaries, # species range
                                               data = data.frame("Species", rangekm2),
                                               match.ID = FALSE)

    # reprojection
    boundaries <- sp::spTransform(boundaries, final_projection)

    # exporting
    if (save_shp == TRUE) {
      if (verbose == TRUE) {
        message("\nWriting shapefile in the working directory...\n")
      }
      rgdal::writeOGR(boundaries, ".", name, driver = "ESRI Shapefile")
    }

    # return results
    sp_dat <- data.frame(Species = "Species", Range_area = areakm2)

    results <- sp_range(name = "Boundaries", summary = sp_dat,
                        species_range = boundaries)

  } else {
    # calculate areas in km2
    rangekm2 <- raster::area(boundaries) / 1000000
    areakm2 <- sum(rangekm2) # total area of the species range

    # adding characteristics to spatial polygons
    species <- as.character(occurrences[1, 1])
    boundaries <- sp::SpatialPolygonsDataFrame(boundaries, # species range
                                               data = data.frame(species, rangekm2),
                                               match.ID = FALSE)

    # extent of occurrence
    if (extent_of_occurrence == TRUE) {
      eooc <- eoo(occ_sp@data, poly)
      eocckm2 <- eooc$area
      extent_occurrence <- eooc$spolydf
      extent_occurrence <- sp::spTransform(extent_occurrence, final_projection)
    } else {
      eocckm2 <- 0
      extent_occurrence <- new("SpatialPolygonsDataFrame")
    }

    # area of occupancy
    if (area_of_occupancy == TRUE) {
      aooc <- aoo(occ_pr, species)
      aocckm2 <- aooc$area
      area_occupancy <- aooc$spolydf
      area_occupancy <- sp::spTransform(area_occupancy, final_projection)
    } else {
      aocckm2 <- 0
      area_occupancy <- new("SpatialPolygonsDataFrame")
    }

    # reprojection
    boundaries <- sp::spTransform(boundaries, final_projection)
    occ_pr <- sp::spTransform(occ_pr, final_projection)

    # exporting
    if (save_shp == TRUE) {
      if (verbose == TRUE) {
        message("\nWriting shapefiles in the working directory...\n")
      }
      rgdal::writeOGR(boundaries, ".", name, driver = "ESRI Shapefile",
                      overwrite_layer = overwrite)
      rgdal::writeOGR(occ_pr, ".", paste(name, "unique_records", sep = "_"),
                      driver = "ESRI Shapefile", overwrite_layer = overwrite)
      if (extent_of_occurrence == TRUE) {
        rgdal::writeOGR(extent_occurrence, ".", paste(name, "extent_occ", sep = "_"),
                        driver = "ESRI Shapefile", overwrite_layer = overwrite)
      }
      if (area_of_occupancy == TRUE) {
        rgdal::writeOGR(area_occupancy, ".", paste(name, "area_occ", sep = "_"),
                        driver = "ESRI Shapefile", overwrite_layer = overwrite)
      }
    }

    # return results (list or a different object?)
    sp_dat <- data.frame(Species = species, Unique_records = dim(occ_pr)[1],
                         Range_area = areakm2, Extent_of_occurrence = eocckm2,
                         Area_of_occupancy = aocckm2)

    results <- sp_range_iucn(name = "Boundaries", summary = sp_dat,
                             species_unique_records = occ_pr,
                             species_range = boundaries,
                             extent_of_occurrence = extent_occurrence,
                             area_of_occupancy = area_occupancy)
  }

  return(results)
}
