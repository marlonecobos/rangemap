#' Species distributional ranges based on trend surface analyses
#'
#' @description rangemap_tsa generates a distributional range for a given species
#' using a trend surface analysis. An approach to the species extent of occurrence
#' (using convex hulls) and the area of occupancy according to the IUCN criteria
#' is also generated. Shapefiles can be saved in the working directory if it is
#' needed.
#'
#' @param occurrences a data.frame containing geographic coordinates of species
#' occurrences, columns must be: Species, Longitude, and Latitude. Geographic
#' coordinates must be in decimal degrees  (WGS84).
#' @param region_of_interest a SpatialPolygonsDataFrame object on which the trend
#' surface analysis will be performed. For instance, a country, an ecoregion, or
#' a biogeographic region. Projection must be WGS84 (EPSG:4326).
#' @param cell_size (numeric) vector of length 1 or 2, defining the size of cells
#' (km) at which the resultant trend surface will be created; default = 5.
#' \code{cell_size} will depend on the extent of \code{region_of_interest}.
#' Values lower than 1 are only recommended when the species is locally distributed.
#' @param threshold (numeric) percentage of occurrence records to be excluded
#' when deciding the minimum value trend surface output to be considered as part
#' of the species range. Default = 0.
#' @param simplify (logical) if \code{TRUE}, polygons of suitable areas will be
#' simplified at a tolerance defined in \code{simplify_level}. Default =
#' \code{FALSE}.
#' @param simplify_level (numeric) tolerance at the moment of simplifying polygons
#' created using the trend surface model. Lower values will produce polygons more
#' similar to the original geometry. Default = 0. If simplifying is needed, try
#' numbers between 0 and 1 first.
#' @param extent_of_occurrence (logical) whether to obtain the extent of occurrence
#' of the species based on a simple convex hull polygon; default = \code{TRUE}.
#' @param area_of_occupancy (logical) whether to obtain the area of occupancy
#' of the species based on a simple grid of 4 km^2 resolution;
#' default = \code{TRUE}.
#' @param final_projection (character) string of projection arguments for resulting
#' Spatial objects. Arguments must be as in the PROJ.4 documentation. See
#' \code{\link[sp]{CRS-class}} for details. If \code{NULL}, the default, projection
#' used is WGS84 (EPSG:4326).
#' @param save_shp (logical) if \code{TRUE}, shapefiles of the species range,
#' occurrences, extent of occurrence, and area of occupancy will be written in
#' the working directory. Default = \code{FALSE}.
#' @param save_ts_layer (logical) if \code{TRUE}, the TSA layer will be included
#' as part of the object returned. If \code{save_shp} = TRUE, the TSA layer will
#' be written in GeoTiff format. Default = \code{FALSE}
#' @param name (character) valid if \code{save_shp} = TRUE. The name of the
#' geographic files to be exported. A suffix will be added to \code{name}
#' depending on the object as follows: species extent of occurrence = "_extent_occ",
#' area of occupancy = "_area_occ", occurrences = "_unique_records", and,
#' if \code{save_ts_layer} = \code{TRUE}, trend surface layer "_tsa".
#' @param overwrite (logical) whether or not to overwrite previous results with
#' the same name. Default = \code{FALSE}.
#' @param verbose (logical) whether or not to print messages about the process.
#' Default = TRUE.
#'
#' @return
#' A sp_range object (S4) containing: (1) a data.frame with information about
#' the species range, and SpatialPolygons objects of (2) unique occurrences,
#' (3) species range, (4) extent of occurrence, and (5) area of occupancy.
#' If \code{save_ts_layer} = TRUE, a (6) TSA layer will be included as well.
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
#' Trend surface analysis is a method based on low-order polynomials of spatial
#' coordinates for estimating a regular grid of points from scattered observations.
#' This method assumes that all cells not occupied by occurrences are absences;
#' hence its use depends on the quality of data and the certainty of having or
#' not a complete sampling of the \code{regiong_of_interest}.
#'
#' @usage
#' rangemap_tsa(occurrences, region_of_interest, cell_size = 5,
#'              threshold = 0, simplify = FALSE, simplify_level = 0,
#'              extent_of_occurrence = TRUE, area_of_occupancy = TRUE,
#'              final_projection = NULL, save_shp = FALSE,
#'              save_ts_layer = FALSE, name, overwrite = FALSE, verbose = TRUE)
#'
#' @export
#'
#' @importFrom sp CRS SpatialPointsDataFrame SpatialPolygonsDataFrame
#' @importFrom sp proj4string spTransform
#' @importFrom raster disaggregate area extent rasterize res values mask
#' @importFrom raster writeRaster extract rasterToPolygons
#' @importFrom rgeos gUnaryUnion
#' @importFrom rgdal writeOGR
#' @importFrom spatial surf.ls predict.trls
#' @importFrom stats na.omit
#'
#' @examples
#' # data
#' data("occ_f", package = "rangemap")
#'
#' CU <- simple_wmap("simple", regions = "Cuba")
#'
#' # running
#' tsa_range <- rangemap_tsa(occurrences = occ_f, region_of_interest = CU,
#'                           cell_size = 5)
#'
#' summary(tsa_range)

rangemap_tsa <- function(occurrences, region_of_interest, cell_size = 5,
                         threshold = 0, simplify = FALSE, simplify_level = 0,
                         extent_of_occurrence = TRUE, area_of_occupancy = TRUE,
                         final_projection = NULL, save_shp = FALSE,
                         save_ts_layer = FALSE, name, overwrite = FALSE,
                         verbose = TRUE) {

  # testing potential issues
  if (missing(occurrences)) {
    stop("Argument 'occurrences' is necessary to perform the analysis")
  }
  if (dim(occurrences)[2] != 3) {
    stop("'occurrences' must have the following columns: \nSpecies, Longitude, Latitude")
  }
  if (missing(region_of_interest)) {
    stop("Argument 'region_of_interest' is necessary to perform the analysis")
  }
  if (threshold > 0) {
    warning("As 'threshold' > 0, some occurrences may be excluded from the species range.")
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

  # making spatial points
  occ_sp <- sp::SpatialPointsDataFrame(coords = occ[, 2:3], data = occ,
                                       proj4string = WGS84)

  # keeping only records in land
  occ_sp <- occ_sp[region_of_interest, ]

  # project the points using their centriods as reference
  LAEA <- LAEA_projection(spatial_object = occ_sp)
  occ_pr <- sp::spTransform(occ_sp, LAEA)

  # region of interest projected
  region <- region_of_interest
  region_of_interest <- sp::spTransform(region_of_interest, LAEA)

  # preparing variables
  ## creating a grid
  grid <- raster::raster(raster::extent(region_of_interest))

  ## grid resolution and values
  raster::res(grid) <- cell_size * 1000
  raster::values(grid) <- 0

  ## grid projection
  sp::proj4string(grid) <- sp::proj4string(region_of_interest)

  ## extract grid with region
  grid_reg <- raster::mask(grid, region_of_interest)

  ## grid for region of interest
  grid_r_pol <- raster::rasterToPolygons(grid_reg)

  ## points for region of interest
  matrix_pa <- raster::rasterToPoints(grid_reg)

  ## selecting grids with occurrences
  grid_r_pol <- grid_r_pol[occ_pr, ]

  ## grid to points
  ras_grid <- raster::rasterize(grid_r_pol, grid_reg, "layer")
  ras_grid <- raster::rasterToPoints(ras_grid)[, 1:2]

  ## asigning 1 to cells occupied by occurrenes
  matrix_pa[, 3] <- ifelse(paste(matrix_pa[, 1], matrix_pa[, 2]) %in%
                             paste(ras_grid[, 1], ras_grid[, 2]), 1, 0)

  ## data for models
  condition <- nrow(matrix_pa) > (10000 + nrow(ras_grid))
  if (condition) {
    all_matrix <- matrix_pa[, 1:2]

    ma_a <- matrix_pa[matrix_pa[, 3] == 1, ]
    matrix_pa <- matrix_pa[matrix_pa[, 3] == 0, ]
    matrix_pa <- matrix_pa[sample(nrow(matrix_pa), 10000), ]

    matrix_pa <- rbind(matrix_pa, ma_a)
  }

  # tsa
  ## tsa model
  tsa <- spatial::surf.ls(np = 3, x = matrix_pa[, 1], y = matrix_pa[, 2],
                          z = matrix_pa[, 3])

  # tsa prediction to region of insterest
  if (condition) {
    tsa_reg <- spatial::predict.trls(tsa, all_matrix[, 1], all_matrix[, 2])
    tsa_model <- raster::rasterize(all_matrix, grid_reg, tsa_reg)
  } else {
    tsa_reg <- spatial::predict.trls(tsa, matrix_pa[, 1], matrix_pa[, 2])
    tsa_model <- raster::rasterize(matrix_pa[, 1:2], grid_reg, tsa_reg)
  }

  # tsa thresholded
  tsa_t <- tsa_model
  occ_val <- na.omit(raster::extract(tsa_t, occ_pr@coords))
  val <- ceiling(nrow(occ) * threshold / 100) + 1
  thres <- sort(occ_val)[val]

  tsa_t <- tsa_t >= thres

  # only presence
  tsa_t[tsa_t[] == 0] <- NA

  # tsa to spatial polygon
  tsa_t <- raster::rasterToPolygons(tsa_t)
  tsa_t <- rgeos::gUnaryUnion(tsa_t, tsa_t$layer)
  tsa_t <- raster::disaggregate(tsa_t)

  if (simplify == TRUE) {
    tsa_t <- suppressWarnings(rgeos::gSimplify(tsa_t, tol = simplify_level))
  }

  # calculate areas in km2
  area <- raster::area(tsa_t) / 1000000
  areakm2 <- sum(area) # total area of the species range

  # adding characteristics to spatial polygons
  species <- as.character(occurrences[1, 1])
  clip_area <- sp::SpatialPolygonsDataFrame(tsa_t,
                                            data = data.frame(species, area),
                                            match.ID = FALSE)

  # extent of occurrence
  if (extent_of_occurrence == TRUE) {
    eooc <- eoo(occ_sp@data, region)
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

    if (save_ts_layer == TRUE) {
      if (verbose == TRUE) {
        message("Writing trend surface layer in the working directory.")
      }
      raster::writeRaster(tsa_model, paste0(name, "_ts_layer.tif"),
                          format = "GTiff", overwrite = overwrite)
    }
  }

  # return results
  sp_dat <- data.frame(Species = species, Unique_records = dim(occ_pr)[1],
                       Range_area = areakm2, Extent_of_occurrence = eocckm2,
                       Area_of_occupancy = aocckm2)

  if (save_ts_layer == TRUE) {
    results <- sp_range_iucnextra(name = "TSA", summary = sp_dat,
                                  species_unique_records = occ_pr,
                                  species_range = clip_area,
                                  extent_of_occurrence = extent_occurrence,
                                  area_of_occupancy = area_occupancy,
                                  trend_surface_model = tsa_model)
  }else {
    results <- sp_range_iucn(name = "TSA", summary = sp_dat,
                             species_unique_records = occ_pr,
                             species_range = clip_area,
                             extent_of_occurrence = extent_occurrence,
                             area_of_occupancy = area_occupancy)
  }

  return(results)
}


