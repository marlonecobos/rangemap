#' Plots of species ranges on maps of environmental variables
#'
#' @description ranges_emaps plots one or more ranges of the same species on
#' various maps of environmental factors (e.g. climatic variables) to detect
#' implications of using one or other type of range regarding the environmental
#' conditions in the areas.
#'
#' @param ... one or more objects of class \code{\link{sp_range}} produced with
#' any of the following functions: \code{\link{rangemap_buffer}},
#' \code{\link{rangemap_boundaries}}, \code{\link{rangemap_hull}},
#' \code{\link{rangemap_enm}}, and/or \code{\link{rangemap_tsa}}. Using up to
#' three or four ranges is recommended for more precise comparisons.
#' @param variables a RasterLayer or RasterStack object of environmental variables
#' that will be used as the base for maps. Projection is assumed to be WGS84
#' (EPSG:4326). Consider that depending on the species range, using more than 9
#' variables creates a plot that may not fit in an A4 paper sheet. A maximum of
#' 21 variables is allowed, if this limit is surpassed, other variables will be
#' ignored.
#' @param add_occurrences (logical) if \code{TRUE}, species occurrences contained
#' in \code{\link{sp_range}} objects will be added to the figure. Default =
#' \code{FALSE}. If the none of the objects contains occurrences, this argument
#' will be ignored.
#' @param range_colors vector of colors for borders of species ranges. If
#' \code{NULL}, the default, distinct levels of gray will be used. If more than
#' 3 \code{\link{sp_range}} objects are included, defining your own colors is
#' recommended.
#' @param color_variables a color palette (a vector of continuous colors generated
#' by functions like heat.colors). If \code{NULL}, the default, rev(terrain.colors(255))
#' will be used.
#' @param ranges_legend (logical) if \code{TRUE}, a legend of the plotted ranges
#' will be added to the last panel of the plot at \code{legend_position}.
#' Default = \code{TRUE}.
#' @param legend_position (numeric or character) location where the legend will
#' be placed in the plot. If numeric, vector of length = 2 indicating x and y
#' coordinates to position the legend. See details in \code{\link[graphics]{legend}}
#' for character options of position. Default = "bottomright".
#' @param legend_cex (numeric) size of the legend with respect to \code{cex}
#' option in \code{\link[graphics]{par}}. Default = 0.7.
#' @param zoom (numeric) zoom factor when plotting the species range in a map
#' (based on the largest range). Default = 1.3. Larger values will zoom in into
#' the species range and smaller values will zoom out. A value of 0.5 will
#' duplicate the area that the biggest range is covering.
#' @param verbose (logical) whether or not to print messages about the process.
#' Default = TRUE.
#'
#' @return
#' A plot showing species ranges on top of maps of environmental variables.
#'
#' @details
#' Position of distinct elements depend on the spatial configuration of the
#' species range. Therefore, their position may need to be changed if such
#' elements are needed (e.g., legend). Current character options available for
#' position are: "bottomright", "bottomleft", "topleft", and "topright".
#'
#' @usage
#' ranges_emaps(..., variables, add_occurrences = FALSE,
#'              range_colors = NULL, color_variables = NULL,
#'              ranges_legend = TRUE, legend_position = "bottomright",
#'              legend_cex = 0.7, zoom = 0.7, verbose = TRUE)
#'
#' @export
#'
#' @importFrom sp CRS
#' @importFrom raster projectRaster
#' @importFrom grDevices gray.colors terrain.colors
#' @importFrom graphics legend par
#'
#' @examples
#' # example data
#' data("buffer_range", package = "rangemap")
#' data("cxhull_range", package = "rangemap")
#' data("cvehull_range", package = "rangemap")
#'
#' vars <- raster::stack(system.file("extdata", "variables.tif",
#'                                   package = "rangemap"))
#' names(vars) <- c("bio5", "bio6", "bio13", "bio14")
#'
#' # plotting
#' ranges_emaps(buffer_range, cxhull_range, cvehull_range, variables = vars)

ranges_emaps <- function(..., variables, add_occurrences = FALSE,
                         range_colors = NULL, color_variables = NULL,
                         ranges_legend = TRUE, legend_position = "bottomright",
                         legend_cex = 0.7, zoom = 0.7, verbose = TRUE) {

  # testing potential issues
  if (missing(...)) {
    stop("Argument '...' is necessary to perform the analysis")
  } else {
    ranges <- list(...)
    if (length(ranges) < 1) {
      stop("At least one sp_range* object is needed to produce plots.")
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

  # par settings
  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))

  # variable treatment
  ## reducing
  if (raster::nlayers(variables) > 21) {
    variables <- variables[[1:21]]
    if (verbose == TRUE) {
      message("Only 21 'variables' will be used. See function's help.")
    }
  }

  ## projecting
  variables <- raster::projectRaster(variables, crs = WGS84@projargs)

  # x and y limits of plots
  xbox <- unlist(lapply(sp_ranges, function(x) {c(x@bbox[1, ])}))
  ybox <- unlist(lapply(sp_ranges, function(x) {c(x@bbox[2, ])}))

  xbox <- as.numeric(c(min(xbox), max(xbox)))
  ybox <- as.numeric(c(min(ybox), max(ybox)))

  ## limits for map
  xlim <- c(xbox[1] - ((((xbox[2] - xbox[1]) * 1/zoom) - (xbox[2] - xbox[1])) / 2),
            xbox[2] + ((((xbox[2] - xbox[1]) * 1/zoom) - (xbox[2] - xbox[1])) / 2))
  ylim <- c(ybox[1] - ((((ybox[2] - ybox[1]) * 1/zoom) - (ybox[2] - ybox[1])) / 2),
            ybox[2] + ((((ybox[2] - ybox[1]) * 1/zoom) - (ybox[2] - ybox[1])) / 2))

  # plot
  ## par options
  fig_conf <- list(c(1, 1), c(1, 2), c(1, 3), c(2, 2), c(2, 3), c(2, 3), c(3, 3),
                   c(3, 3), c(3, 3), c(4, 3), c(4, 3), c(4, 3), c(5, 3), c(5, 3),
                   c(5, 3), c(6, 3), c(6, 3), c(6, 3), c(7, 3), c(7, 3), c(7, 3))

  for (i in 1:raster::nlayers(variables)) {
    if (raster::nlayers(variables) == i) {
      fig_config <- fig_conf[[i]]
    }
  }

  # range and variable colors
  if (is.null(range_colors)) {
    range_colors <- gray.colors(length(sp_ranges))
  }

  if (is.null(color_variables)) {
    color_variables <- rev(terrain.colors(255))
  }

  par(mar = c(0, 0, 0, 3), mfrow = fig_config)

  ## the plot and variable legends
  for (i in 1:raster::nlayers(variables)) {
    plot_ranges(sp_ranges, sp_records, variable = variables[[i]],
                range_colors, color_variables, xlim, ylim)
  }

  ## legend
  if (ranges_legend == TRUE) {
    if (class(legend_position) == "character") {
      if (add_occurrences == TRUE) {
        legend(legend_position, legend = c("Occurrences", rnames),  bty = "n",
               inset = 0.03, pch = c(20, rep(22, length(rnames))),
               pt.cex = c(1, rep(1.5, length(rnames))), cex = legend_cex,
               col = c("black", range_colors))
      } else {
        legend(legend_position, legend = rnames,  bty = "n", inset = 0.03,
               pch = c(rep(22, length(rnames))), cex = legend_cex,
               col = range_colors, pt.cex = c(rep(1.5, length(rnames))))
      }
    } else {
      xleg <- legend_position[1]
      yleg <- legend_position[2]
      if (add_occurrences == TRUE) {
        legend(x = xleg, y = yleg, legend = c("Occurrences", rnames),  bty = "n",
               inset = 0.03, pch = c(20, rep(22, length(rnames))),
               pt.cex = c(1, rep(1.5, length(rnames))),
               cex = legend_cex, col = c("black", range_colors))
      } else {
        legend(x = xleg, y = yleg, legend = rnames,  bty = "n", inset = 0.03,
               pch = c(rep(22, length(rnames))), cex = legend_cex,
               col = range_colors, pt.cex = c(rep(1.5, length(rnames))))
      }
    }
  }
}
