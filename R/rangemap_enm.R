#' Species distributional ranges based on ENMs/SDMs outputs
#'
#' @description rangemap_enm generates a distributional range for a given species
#' using a continuous raster layer produced using ecological niche modeling or
#' species distribution modeling tools. This function binarizes the model in
#' suitable and unsuitable areas using a user specified level of omission or a
#' given threshold value. Optionally, representations of the species extent of
#' occurrence (using convex hulls) and the area of occupancy according to the
#' IUCN criteria can also be generated. Shapefiles can be saved in the working
#' directory if it is needed.
#'
#' @param model_output a RasterLayer of suitability for the species of interest
#' generated using a ENM or SDM algorithm, that will be binarized using the a
#' user-defined \code{threshold_value} or a value calculated based on a percentage
#' of omission (0 - 100) defined in \code{threshold_omission}. If the layer is
#' projected, this projection must be WGS84 (EPSG:4326); if not projected, WGS84
#' projection will be assigned for the analysis.
#' @param occurrences a data.frame containing geographic coordinates of species
#' occurrences, columns must be: Species, Longitude, and Latitude. Geographic
#' coordinates must be in decimal degrees. \code{occurrences} may not be defined,
#' but if so, \code{threshold_value} must be defined. Default = \code{NULL}.
#' @param threshold_value (numeric) value used for reclassifying the
#' \code{model_output}. This value will be the lowest considered as suitable for
#' the species and must be inside the range of values present in \code{model_output}.
#' If defined, \code{threshold_omission} will be ignored. If \code{occurrences}
#' is not defined, this parameter is required. Default = \code{NULL}.
#' @param threshold_omission (numeric) percentage of occurrence records to be
#' excluded from suitable areas considering their values of suitability in the
#' continuous model (e.g., 0, 5, or 10). Ignored if \code{threshold_value} is
#' provided. Default = \code{NULL}.
#' @param min_polygon_area (numeric) minimum area of polygons that will be kept
#' as part of the species ranges after defining suitable areas and convert
#' raster layer to polygon. Default = 0. A value of 0 will keep all polygons.
#' @param simplify (logical) if \code{TRUE}, polygons of suitable areas will be
#' simplified at a tolerance defined in \code{simplify_level}. Default =
#' \code{FALSE}.
#' @param simplify_level (numeric) tolerance to consider when simplifying polygons
#' created from suitable areas in \code{model_output}. Lower values will produce
#' polygons more similar to the original geometry. Default = 0. If simplifying is
#' needed, try numbers 0-1 first. Ignored if \code{simplify} = \code{FALSE}.
#' @param polygons (optional) a SpatialPolygons* object to clip polygons and
#' adjust extent of occurrence to these limits. Projection must be WGS84
#' (EPSG:4326). If \code{NULL}, the default, a simplified world map will be used.
#' @param extent_of_occurrence (logical) whether to obtain the extent of occurrence
#' of the species based on a simple convex hull polygon; default = \code{TRUE}.
#' @param area_of_occupancy (logical) whether to obtain the area of occupancy
#' of the species based on a simple grid of 4 km^2 resolution;
#' default = \code{TRUE}.
#' @param final_projection (character) string of projection arguments for
#' resulting Spatial objects. Arguments must be as in the PROJ.4 documentation.
#' See \code{\link[sp]{CRS-class}} for details. If \code{NULL}, the default,
#' projection used is WGS84 (EPSG:4326).
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
#' If \code{occurrences} and \code{threshold_omission} are defined, a  sp_range
#' object (S4) containing: (1) a data.frame with information about the species
#' range, and Spatial objects of (2) unique occurrences, (3) species range,
#' (4) extent of occurrence, and (5) area of occupancy.
#'
#' If instead of \code{occurrences} and \code{threshold_omission},
#' \code{threshold_value} is provided, the result will be a sp_range object
#' (S4) of two elements: (1) a data.frame with information about the species
#' range, and (2) a SpatialPolygons object of the species range.
#'
#' If \code{extent_of_occurrence} and/or \code{area_of_occupancy} = \code{FALSE},
#' the corresponding spatial objects in the resulting sp_range object will be
#' empty, an areas will have a value of 0.
#'
#' @details
#' All resulting Spatial objects in the list of results will be projected to the
#' \code{final_projection}. Areas are calculated in square kilometers using the
#' Lambert Azimuthal Equal Area projection, centered on the centroid of occurrence
#' points given as inputs or, if points are not provided, the resulting range.
#'
#' @usage
#' rangemap_enm(model_output, occurrences = NULL, threshold_value = NULL,
#'              threshold_omission = NULL, min_polygon_area = 0,
#'              simplify = FALSE, simplify_level = 0, polygons = NULL,
#'              extent_of_occurrence = TRUE, area_of_occupancy = TRUE,
#'              final_projection = NULL, save_shp = FALSE, name,
#'              overwrite = FALSE, verbose = TRUE)
#'
#' @export
#'
#' @importFrom raster extract crs area
#' @importFrom rgeos gSimplify gBuffer gUnaryUnion
#' @importFrom sp CRS SpatialPointsDataFrame SpatialPolygonsDataFrame
#' @importFrom sp spTransform
#' @importFrom rgdal writeOGR
#' @importFrom stats na.omit
#'
#' @examples
#' \donttest{
#' # parameters
#' sp_mod <- raster::raster(list.files(system.file("extdata", package = "rangemap"),
#'                                     pattern = "sp_model", full.names = TRUE))
#' data("occ_train", package = "rangemap")
#'
#' thres <- 5
#' save <- TRUE
#' name <- "test"
#'
#' enm_range <- rangemap_enm(model_output = sp_mod, occurrences = occ_train,
#'                           threshold_omission = thres)
#'
#' summary(enm_range)
#' }

rangemap_enm <- function(model_output, occurrences = NULL, threshold_value = NULL,
                         threshold_omission = NULL, min_polygon_area = 0,
                         simplify = FALSE, simplify_level = 0, polygons = NULL,
                         extent_of_occurrence = TRUE, area_of_occupancy = TRUE,
                         final_projection = NULL, save_shp = FALSE,
                         name, overwrite = FALSE, verbose = TRUE) {

  # check for errors
  if (is.null(model_output)) {
    stop("'model_output' is necessary to perform the analysis.")
  }
  if (save_shp == TRUE) {
    if (missing(name)) {
      stop("Argument 'name' must be defined if 'save_shp' = TRUE.")
    }
    if (file.exists(paste0(name, ".shp")) & overwrite == FALSE) {
      stop("Files already exist, use 'overwrite' = TRUE.")
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

  # finding threshold value
  if (!is.null(threshold_value) | !is.null(threshold_omission) |
      !is.null(occurrences)) {
    if (!is.null(threshold_value)) {
      binary <- model_output >= threshold_value
    } else {
      if (!is.null(threshold_omission) & !is.null(occurrences)) {
        # keeping only coordinates
        occ <- occurrences[, 2:3]

        # threshold value calculation
        o_suit <- na.omit(raster::extract(model_output, occ))
        o_suit_sort <- sort(o_suit)
        thres <- o_suit_sort[ceiling(length(occ[, 1]) * threshold_omission / 100) + 1]

        # binarization
        binary <- model_output >= thres
      } else {
        stop(paste0("Parameters 'threshold_omission' and 'occurrences', or 'threshold_value'",
                    "\nmust be defined to perform the calculations."))
      }
    }
  }

  # keeping only areas of presence
  binary[binary[] == 0] <- NA

  # erase duplicate records
  if (!is.null(occurrences)) {
    occ <- as.data.frame(unique(occurrences))[, 1:3]
    colnames(occ) <- c("Species", "Longitude", "Latitude")
  }

  # convert raster to polygons
  if (is.na(model_output@crs)) {
    raster::crs(binary) <- WGS84@projargs
  }

  enm_range <- as(binary, "SpatialPolygonsDataFrame")
  enm_range <- rgeos::gUnaryUnion(enm_range, enm_range$layer)
  enm_range <- sp::SpatialPolygonsDataFrame(enm_range, data = data.frame(ID = 1))

  # erasing small polygons
  enm_range <- keep_big_polygons(polygons = enm_range, min_polygon_area)

  if (simplify == TRUE) {
    enm_range <- suppressWarnings(rgeos::gSimplify(enm_range, tol = simplify_level))
  }
  enm_range <- raster::disaggregate(enm_range)

  # world map or user map for creating extent of occurrence
  if (is.null(polygons)) {
    polygons <- simple_wmap(which = "simple")
  }

  # project area and occurrences using the area centriod as reference
  LAEA <- LAEA_projection(spatial_object = enm_range)

  if (!is.null(occurrences)) {
    occ_sp <- sp::SpatialPointsDataFrame(coords = occ[, 2:3], data = occ,
                                         proj4string = WGS84)
    occ_pr <- sp::spTransform(occ_sp, LAEA)
  }

  enm_range_pr <- sp::spTransform(enm_range, LAEA)

  #polygons <- suppressWarnings(rgeos::gBuffer(polygons, byid = TRUE, width = 0))
  #polygons <- rgeos::gUnaryUnion(polygons)

  # calculate areas in km2
  area <- raster::area(enm_range_pr) / 1000000
  areakm2 <- sum(area) # total area of the species range

  if (is.null(occurrences)) {
    # adding characteristics to spatial polygons
    df <- data.frame(Species = "Species", area)
    clip_area <- sp::SpatialPolygonsDataFrame(enm_range_pr, data = df,
                                              match.ID = FALSE)

    # reprojection
    if (is.null(final_projection)) {
      final_projection <- WGS84
    } else {
      final_projection <- sp::CRS(final_projection) # character to projection
    }

    clip_area <- sp::spTransform(clip_area, final_projection)

    # exporting
    if (save_shp == TRUE) {
      if (verbose == TRUE) {
        message("Writing shapefiles in the working directory.")
      }
      rgdal::writeOGR(clip_area, ".", name, driver = "ESRI Shapefile")
    }

    # return results
    sp_dat <- data.frame(Species = "Species", Range_area = areakm2)

    results <- sp_range(name = "ENM", summary = sp_dat, species_range = clip_area)

  } else {
    # extent of occurrence
    if (extent_of_occurrence == TRUE) {
      eooc <- eoo(occ_sp@data, polygons)
      eocckm2 <- eooc$area
      extent_occurrence <- eooc$spolydf
      extent_occurrence <- sp::spTransform(extent_occurrence, final_projection)
    } else {
      eocckm2 <- 0
      extent_occurrence <- new("SpatialPolygonsDataFrame")
    }

    # area of occupancy
    species <- as.character(occurrences[1, 1])
    if (area_of_occupancy == TRUE) {
      aooc <- aoo(occ_pr, species)
      aocckm2 <- aooc$area
      area_occupancy <- aooc$spolydf
      area_occupancy <- sp::spTransform(area_occupancy, final_projection)
    } else {
      aocckm2 <- 0
      area_occupancy <- new("SpatialPolygonsDataFrame")
    }

    # adding characteristics to spatial polygons
    clip_area <- sp::SpatialPolygonsDataFrame(enm_range_pr, # species range
                                              data = data.frame(species, area),
                                              match.ID = FALSE)

    # reprojection
    clip_area <- sp::spTransform(clip_area, final_projection)
    occ_pr <- sp::spTransform(occ_pr, final_projection)

    # exporting
    if (save_shp == TRUE) {
      if (verbose == TRUE) {
        message("Writing shapefiles in the working directory.")
      }
      rgdal::writeOGR(clip_area, ".", name, driver = "ESRI Shapefile",
                      overwrite_layer = overwrite)
      rgdal::writeOGR(occ_pr, ".", paste(name, "unique_records", sep = "_"),
                      driver = "ESRI Shapefile", overwrite_layer = overwrite)
      if (!is.null(occurrences)) {
        if (extent_of_occurrence == TRUE) {
          rgdal::writeOGR(extent_occurrence, ".", paste(name, "extent_occ", sep = "_"),
                          driver = "ESRI Shapefile", overwrite_layer = overwrite)
        }
        if (area_of_occupancy == TRUE) {
          rgdal::writeOGR(area_occupancy, ".", paste(name, "area_occ", sep = "_"),
                          driver = "ESRI Shapefile", overwrite_layer = overwrite)
        }
      }
    }

    # return results
    sp_dat <- data.frame(Species = species, Unique_records = dim(occ_pr)[1],
                         Range_area = areakm2, Extent_of_occurrence = eocckm2,
                         Area_of_occupancy = aocckm2)

    results <- sp_range_iucn(name = "ENM", summary = sp_dat,
                             species_unique_records = occ_pr,
                             species_range = clip_area,
                             extent_of_occurrence = extent_occurrence,
                             area_of_occupancy = area_occupancy)
  }

  return(results)
}
