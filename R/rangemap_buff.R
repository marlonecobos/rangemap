#' Species distributional ranges based on buffered occurrences
#'
#' @description rangemap_buffer generates a distributional range for a given
#' species by buffering provided occurrences using a defined distance. Optionally,
#' representations of the species extent of occurrence (using convex hulls) and
#' the area of occupancy according to the IUCN criteria can also be generated.
#' Shapefiles can be saved in the working directory if it is needed.
#'
#' @param occurrences a data.frame containing geographic coordinates of species
#' occurrences, columns must be: Species, Longitude, and Latitude. Geographic
#' coordinates must be in decimal degrees (WGS84).
#' @param buffer_distance (numeric) distance, in meters, to be used for creating
#' the buffer areas around occurrences, default = 100000.
#' @param polygons (optional) a SpatialPolygons* object to clip buffer areas and
#' adjust the species range and other polygons to these limits. Projection must
#' be WGS84 (EPSG:4326). If \code{NULL}, the default, a simplified world map
#' will be used.
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
#' A sp_range object (S4) containing: (1) a data.frame with information about the
#' species range, and Spatial objects of (2) unique occurrences, (3) species range,
#' (4) extent of occurrence, and (5) area of occupancy.
#'
#' If \code{extent_of_occurrence} and/or \code{area_of_occupancy} = \code{FALSE},
#' the corresponding spatial objects in the resulting sp_range object will be
#' empty, an areas will have a value of 0.
#'
#' @details
#' All resulting Spatial objects in the results will be projected to the
#' \code{final_projection}. Areas are calculated in square kilometers using the
#' Lambert Azimuthal Equal Area projection, centered on the centroid of occurrence
#' points given as inputs.
#'
#' @usage
#' rangemap_buffer(occurrences, buffer_distance = 100000, polygons = NULL,
#'                 extent_of_occurrence = TRUE, area_of_occupancy = TRUE,
#'                 final_projection = NULL, save_shp = FALSE, name,
#'                 overwrite = FALSE, verbose = TRUE)
#'
#' @export
#'
#' @importFrom sp CRS SpatialPointsDataFrame SpatialPolygonsDataFrame
#' @importFrom sp spTransform
#' @importFrom raster disaggregate area
#' @importFrom rgeos gUnaryUnion gIntersection
#' @importFrom rgdal writeOGR
#'
#' @examples
#' \donttest{
#' # getting the data
#' data("occ_p", package = "rangemap")
#'
#' # buffer distance
#' dist <- 100000
#'
#' buff_range <- rangemap_buffer(occurrences = occ_p, buffer_distance = dist)
#'
#' summary(buff_range)
#' }

rangemap_buffer <- function(occurrences, buffer_distance = 100000, polygons = NULL,
                            extent_of_occurrence = TRUE, area_of_occupancy = TRUE,
                            final_projection = NULL, save_shp = FALSE, name,
                            overwrite = FALSE, verbose = TRUE) {
  # testing potential issues
  if (missing(occurrences)) {
    stop("Argument 'occurrences' is necessary to perform the analysis.")
  }
  if (dim(occurrences)[2] != 3) {
    stop("'occurrences' data.frame must have the following columns: \nSpecies, Longitude, Latitude")
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

  # erase duplicate records
  occ <- as.data.frame(unique(occurrences))[, 1:3]
  colnames(occ) <- c("Species", "Longitude", "Latitude")

  # make a spatial object from coordinates
  occ_sp <- sp::SpatialPointsDataFrame(coords = occ[, 2:3], data = occ,
                                       proj4string = WGS84)

  # world map or user map fro creating species range
  if (is.null(polygons)) {
    polygons <- simple_wmap(which = "simple")
  }

  # keeping only records in land
  occ_sp <- occ_sp[polygons, ]

  # create a buffer based on a user-defined distance
  buff_area <- geobuffer_points(data = occ_sp@coords, radius = buffer_distance)

  # getting only relevant polygons
  polygons <- suppressWarnings(rgeos::gBuffer(polygons, byid = TRUE, width = 0))
  polygons1 <- polygons[buff_area, ]

  # clip a world map based on the created buffer
  polygons1 <- rgeos::gUnaryUnion(polygons1)
  clip_area <- rgeos::gIntersection(buff_area, polygons1, byid = TRUE,
                                    drop_lower_td = TRUE)

  # project polygons and points
  LAEA <- LAEA_projection(spatial_object = occ_sp)
  clip_area <- sp::spTransform(clip_area, LAEA)
  occ_pr <- sp::spTransform(occ_sp, LAEA)

  # calculate areas in km2
  areakm2 <- raster::area(clip_area) / 1000000
  areackm2 <- sum(areakm2) # total area of the species range

  # adding characteristics to spatial polygons
  species <- as.character(occurrences[1, 1])
  clip_area <- sp::SpatialPolygonsDataFrame(clip_area, # species range
                                            data = data.frame(species, areakm2),
                                            match.ID = FALSE)

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
    if (extent_of_occurrence == TRUE) {
      rgdal::writeOGR(extent_occurrence, ".", paste(name, "extent_occ", sep = "_"),
                      driver = "ESRI Shapefile", overwrite_layer = overwrite)
    }
    if (area_of_occupancy == TRUE) {
      rgdal::writeOGR(area_occupancy, ".", paste(name, "area_occ", sep = "_"),
                      driver = "ESRI Shapefile", overwrite_layer = overwrite)
    }
  }

  # return results
  sp_dat <- data.frame(Species = species, Unique_records = dim(occ_pr)[1],
                       Range_area = areackm2, Extent_of_occurrence = eocckm2,
                       Area_of_occupancy = aocckm2)

  results <- sp_range_iucn(name = "Buffer", summary = sp_dat,
                           species_unique_records = occ_pr,
                           species_range = clip_area,
                           extent_of_occurrence = extent_occurrence,
                           area_of_occupancy = area_occupancy)

  return(results)
}
