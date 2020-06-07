



plot_ranges <- function(sp_ranges, sp_records = NULL, variable, range_colors,
                        color_variable = rev(terrain.colors(255)), xlim, ylim) {
  if (missing(sp_ranges)) {
    stop("Argument 'sp_ranges' must be defined. See function's help for details.")
  }
  if (missing(variable)) {
    stop("Argument 'variable' must be defined See function's help for details.")
  }
  if (missing(range_colors)) {
    stop("Argument 'range_colors' must be defined See function's help for details.")
  }

  # raster
  raster::plot(variable, col = color_variable, xlim = xlim,
               ylim = ylim, legend = FALSE, axes = FALSE)

  # ranges
  for (j in 1:length(sp_ranges)) {
    sp::plot(sp_ranges[[j]], col = NA, border = range_colors[j], add = TRUE)
  }

  if (!is.null(sp_records)) {
    points(sp_records, pch = 20, cex = 1)
  }

  # legend
  var_range <- c(ceiling(raster::minValue(variable)),
                 floor(raster::maxValue(variable)))

  raster::plot(variable, legend.only = TRUE, col = color_variable,
               legend.width = 1, legend.shrink = 0.8,
               axis.args = list(at = c(var_range[1], var_range[2]),
                                labels = c(var_range[1], var_range[2]),
                                line = -0.519, cex.axis = 0.8),
               legend.args = list(text = names(variable), side = 4, font = 2,
                                  line = 0.5, cex = 0.9))
}
