#' Comparison of species ranges in environmental space
#'
#' @description ranges_espace generates a three dimensional comparison of
#' distributional ranges for a species created using distinct functions of
#' \code{\link{rangemap}}, to visualize them in environmental conditions.
#'
#' @param ... one or more objects of class \code{\link{sp_range}} produced with
#' any of the following functions: \code{\link{rangemap_buffer}},
#' \code{\link{rangemap_boundaries}}, \code{\link{rangemap_hull}},
#' \code{\link{rangemap_enm}}, and/or \code{\link{rangemap_tsa}}. Using up to
#' five ranges is allowed for more precise comparisons.
#' @param add_occurrences (logical) if \code{TRUE}, species occurrences contained
#' in one of the \code{sp_range} objects will be plotted. Default = \code{TRUE}.
#' If  none of the objects contains occurrences this argument will be ignored.
#' @param variables a RasterStack object of at least 3 environmental variables
#' that will be used to perform a principal component analysis, and use the 3
#' first principal components to represent the environmental space. Projection
#' is assumed to be WGS84 (EPSG:4326).
#' @param max_background (numeric) maximum number of data from variables to be
#' used for representing the environmental space. Default = 10000.
#' @param occurrence_color color for occurrence records in environmental space.
#' @param range_colors vector of colors for the ranges to be represented. If
#' \code{NULL}, the default, a set of colors will be used. Since transparency is
#' used for representing ranges in the plot, colors may look different.
#' @param alpha (numeric) degree of opacity for plotting species ranges.
#' Default = 0.6.
#' @param legend (logical) if \code{TRUE}, a simple legend will be added.
#' Default = \code{TRUE}.
#' @param verbose (logical) whether or not to print messages about the process.
#' Default = TRUE.
#'
#' @return
#' A figure showing distributional ranges of a species represented in environmental
#' space (3 principal components).
#'
#' @usage
#' ranges_espace(..., add_occurrences = TRUE, variables,
#'               max_background = 10000, occurrence_color = "blue",
#'               range_colors = NULL, alpha = 0.6, legend = TRUE,
#'               verbose = TRUE)
#'
#' @export
#'
#' @importFrom sp CRS SpatialPointsDataFrame
#' @importFrom raster rasterToPoints extract
#' @importFrom rgl ellipse3d plot3d legend3d aspect3d
#' @importFrom stats prcomp cov
#'
#' @examples
#' # example data
#' data("buffer_range", package = "rangemap")
#' data("cxhull_range", package = "rangemap")
#'
#' vars <- raster::stack(system.file("extdata", "variables.tif",
#'                                   package = "rangemap"))
#' names(vars) <- c("bio5", "bio6", "bio13", "bio14")
#'
#' ## comparison
#' ranges_espace(buffer_range, cxhull_range, variables = vars,
#'               add_occurrences = TRUE)

ranges_espace <- function(..., add_occurrences = TRUE, variables,
                          max_background = 10000, occurrence_color = "blue",
                          range_colors = NULL, alpha = 0.6, legend = TRUE,
                          verbose = TRUE) {

  # testing potential issues
  if (missing(...)) {
    stop("Argument '...' is necessary to perform the analysis")
  } else {
    ranges <- list(...)
    if (length(ranges) < 1) {
      stop("At least one sp_range* object is needed to produce plots.")
    }
    if (length(ranges) > 5) {
      stop("Only 5 sp_range* objects can be plotted at the time.")
    }
    cls <- sapply(ranges, function(x) {class(x)[1]})
    if (any(!cls %in% c("sp_range", "sp_range_iucn", "sp_range_iucnextra"))) {
      stop("All objects to be plotted must be of class sp_range*.")
    }
  }
  if (missing(variables)) {
    stop("Argument 'variables' must be defined. See function's help for details.")
  }

  # preparing data
  ## plain projection
  WGS84 <- ranges[[1]]@species_range@proj4string

  ## extracting data
  sp_ranges <- lapply(ranges, function(x) {x@species_range})

  rnames <- sapply(ranges, function(x) {x@name})

  if (add_occurrences == TRUE) {
    if (any(cls %in% c("sp_range_iucn", "sp_range_iucnextra"))){
      wh <- which(cls %in% c("sp_range_iucn", "sp_range_iucnextra"))[1]

      ## occurrences
      sp_records <- ranges[[wh]]@species_unique_records

      ## variables in occurrences
      pdata <- na.omit(cbind(sp_records@data[, 2:3],
                             raster::extract(variables, sp_records)))
    } else {
      add_occurrences <- FALSE
      sp_records <- NULL
      if (verbose == TRUE) {
        message("None of the objects contain occurrences.")
      }
    }
  } else {
    sp_records <- NULL
  }

  ## projecting
  variables <- raster::projectRaster(variables, crs = WGS84@projargs)

  ## raster to variables data
  vdata <- raster::rasterToPoints(variables)

  if (nrow(vdata) > max_background) {
    vdata <- vdata[sample(nrow(vdata), max_background), ]
  }
  colnames(vdata)[1:2] <- colnames(pdata)[1:2]

  nr <- nrow(vdata)
  nc <- ncol(vdata)

  if (add_occurrences == TRUE) {
    ## combining these data
    vdata <- rbind(vdata, pdata)
  }else {
    vdata <- vdata
  }

  # pca
  ## pca with vdata
  vdata <- data.frame(vdata[, 1:2], prcomp(vdata[, 3:nc], center = TRUE,
                                           scale = TRUE)$x[, 1:3])

  nc <- 5

  if (add_occurrences == TRUE) {
    pc_occ <- vdata[(nr - nrow(pdata) + 1):nrow(vdata), 3:nc]
  }

  vdata <- sp::SpatialPointsDataFrame(coords = vdata[, 1:2], data = vdata[, 1:nc],
                                      proj4string = WGS84)

  # getting the species ranges from objects in ranges, and
  # getting environmental (PCs) data in ranges
  if (verbose == TRUE) {
    message("Getting environmental conditions inside ranges, please wait...")
  }
  env_ranges <- lapply(sp_ranges, function(x) {vdata[x, ]@data})

  # range colors
  if (is.null(range_colors)) {
    colors <- c("black", "green", "red", "blue", "purple")
  }else {
    colors <- range_colors
  }

  # plot
  if (verbose == TRUE) {
    message("\nCreating interactive visualization...\n")
  }

  ## interactive plot
  if (add_occurrences == TRUE) {
    rgl::plot3d(pc_occ[, 1:3], col = occurrence_color, size = 6, xlab = "PC 1",
                ylab = "PC 2", zlab = "PC 3")
    ad <- TRUE
  }else {
    ad <- FALSE
  }

  ## ellipsoids
  ellipsoides <- lapply(1:length(env_ranges), function(x) {
    centroid <- apply(env_ranges[[x]][, 3:nc], 2, mean)
    cov_mat <- cov(env_ranges[[x]][, 3:nc])
    ell <- rgl::ellipse3d(cov_mat, centre = centroid, level = 0.99)

    adt <- ifelse(x == 1, ad, TRUE)
    rgl::wire3d(ell, col = colors[x], alpha = alpha, xlab = "PC 1",
                ylab = "PC 2", zlab = "PC 3", add = adt)
  })

  ## legend
  if (legend == TRUE) {
    rgl::legend3d("topright", legend = rnames, lty = 1, inset = 0.02,
                  col = colors[1:length(ranges)],
                  bty = "n")
  }
  rgl::aspect3d(1, 1, 1)
}
