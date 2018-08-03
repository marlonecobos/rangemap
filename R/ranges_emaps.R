#' Species ranges on maps of environmental factors
#'
#' @description ranges_emaps represents one or more ranges of the same species on
#' various maps of environmental factors (e.g. climatic variables) to detect implications
#' of using one or other type of range regarding the environmental conditions in the areas.
#'
#' @param ranges (list) of SpatialPolygons or SpatialPolygonsDataFrame or a
#' list of objects produced with any of the following functions:
#' \code{\link{rangemap_buff}}, \code{\link{rangemap_bound}}, \code{\link{rangemap_hull}},
#' \code{\link{rangemap_enm}}, and \code{\link{rangemap_tsa}}. For visualization purposes,
#' using up to three ranges is recommended.
#' @param add_occurrences (logical) if TRUE, species occurrences contained in one of the
#' elements of the list \code{ranges} will be ploted in the figure. Default = TRUE. If the
#' none of the ranges contains occurrences (e.g. a list of one object created with the
#' \code{\link{rangemap_bound}} function in which occurrences were not used), this parameter
#' will be ignored.
#' @param variables a RasterStack object of environmental variables that will be used for
#' representing the environmental factors. Projection is assumed to be Geographic (longitude
#' and latitude). Consider that depending on the species range, more than 9 variables
#' may create a figure that does not fit in an A4 sheet of paper. A maximum of 21
#' variables is allowed, if this limit is surpassed, other variables will be ignored.
#' @param range_colors vector of colors of the range borders to be represented. If not
#' defined, default = NULL and default colors will be used. If more than 7 objects are
#' included in \code{ranges}, default colors will be recycled; therefore, defining colors
#' here is recommended in those cases.
#' @param ranges_legend (logical) if TRUE, a legend of the plotted ranges will be added
#' to the last panel at \code{legend_position}. Default = TRUE.
#' @param variables_color a color palette like terrain.colors, heat.colors, topo.colors,
#' or your own. Default = NULL. If not provided, rev(terrain.colors(255)) is used.
#' @param legend_position (numeric or character) site in the figure where the legend will
#' be placed. If numeric, vector of leght two indicating x and y coordinates to be used
#' to position the legend. See details for options of character indicators of position.
#' Default = "bottomright".
#' @param legend_size (numeric) size of the legend with respect to one of the panels.
#' Default = 0.7.
#' @param scalebar (logical) if TRUE, a simple scale bar will be inserted in the last panel
#' at \code{scalebar_position} with a length of \code{scalebar_length}. Default = FALSE.
#' @param scalebar_position (numeric or character) site in the figure where the scale bar
#' will be placed. If numeric, vector of leght two indicating x and y coordinates to be
#' used to position the scale bar. See details for options of character indicators of
#' position. Default = "bottomleft".
#' @param scalebar_length (numeric) length of the scale bar in km. Using entire numbers
#' divisble for two is recommended. Default = 100.
#' @param zoom (numeric) zoom factor when ploting the species range in a map based on the
#' biggest range. Default = 1.3. Lower #' values will zoom in into the species range and
#' bigger values will zoom out. A value of 2 will duplicate the area that the biggest range
#' is covering.
#' @param save_fig (logical) if TRUE, the figure will be written in the working directory.
#' Default = FALSE.
#' @param name (character) if \code{save_fig} = TRUE, name of the figure to be exported.
#' Default = "ranges_emaps".
#' @param format (character) if \code{save_fig} = TRUE, format in which the figure will be
#' written. Options include "bmp", "png", "jpeg", "tiff", and "pdf". Default = "png".
#' @param resolution (numeric) if \code{save_fig} = TRUE, resolution (ppi) in wich the figure
#' will be exported. Default = 300.
#' @param width (numeric) if \code{save_fig} = TRUE, width of the figure in mm. Default = 166.
#' Height will be adjusted considering the amount of variables that will be plotted.
#'
#' @return A figure showing species ranges on maps of environmental factors.
#'
#' @details Ranges should be generated with any of the functions: \code{\link{rangemap_buff}},
#' \code{\link{rangemap_bound}}, \code{\link{rangemap_hull}}, \code{\link{rangemap_enm}}, and
#' \code{\link{rangemap_tsa}}.
#'
#' Position of distinct elements depend on the spatial configuration of the species range.
#' Therefore, their position may need to be changed if the elements are needed. Position
#' options are: "bottomright", "bottomleft", "topleft", and "topright". Numerical descriptions
#' of positions are also allowed.
#'
#' Scale bar is ploted using a modification of the "scalebar" function developed by
#' Tanimura et al. (2007) \url{http://hdl.handle.net/10.18637/jss.v019.c01}.
#'
#' @examples
#' if(!require(rgbif)){
#' install.packages("rgbif")
#' library(rgbif)
#' }
#'
#' # getting the data from GBIF
#' species <- name_lookup(query = "Dasypus kappleri",
#'                        rank="species", return = "data") # information about the species
#'
#' occ_count(taxonKey = species$key[14], georeferenced = TRUE) # testing if keys return records
#'
#' key <- species$key[14] # using species key that return information
#'
#' occ <- occ_search(taxonKey = key, return = "data") # using the taxon key
#'
#' # keeping only georeferenced records
#' occ_g <- occ[!is.na(occ$decimalLatitude) & !is.na(occ$decimalLongitude),
#'              c("name", "decimalLongitude", "decimalLatitude")]
#'
#'
#' # range based on buffers
#' dist <- 500000
#'
#' buff <- rangemap_buff(occurrences = occ_g, buffer_distance = dist)
#'
#'
#' # range based on concave hulls
#' dist1 <- 250000
#' hull1 <- "concave"
#'
#' concave <- rangemap_hull(occurrences = occ_g, hull_type = hull1, buffer_distance = dist1)
#'
#'
#' # range based on convex disjunct hulls
#' split <- TRUE
#' hull2 <- "convex"
#'
#' convex <- rangemap_hull(occurrences = occ_g, hull_type = hull2, buffer_distance = dist1,
#'                          split = split, cluster_method = "k-means", n_k_means = 3)
#'
#' # ranges representation on environmental factor maps
#' ## list of ranges
#' ranges <- list(buff, concave, convex)
#' names(ranges) <- c("buff", "concave", "convex")
#'
#' ## other data for environmental comparisson
#' if(!require(raster)){
#'   install.packages("raster")
#'   library(raster)
#' }
#' if(!require(maps)){
#'   install.packages("maps")
#'   library(maps)
#' }
#' if(!require(maptools)){
#'   install.packages("maptools")
#'   library(maptools)
#' }
#'
#' ## geting bioclimatic variables (some of them)
#' vars <- getData("worldclim", var = "bio", res = 5)[[c("bio1", "bio7", "bio12", "bio15")]]
#' ## after the first view try with distinct or more variables
#'
#' ## mask variables to region of interest
#' WGS84 <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
#' w_map <- map(database = "world", regions = c("Ecuador", "Peru", "Bolivia", "Colombia",
#'                                              "Venezuela", "Suriname", "Guyana",
#'                                              "French Guyana", "Brazil"),
#'              fill = TRUE, plot = FALSE) # map of the world
#'
#' w_po <- sapply(strsplit(w_map$names, ":"), function(x) x[1]) # preparing data to create polygon
#' reg <- map2SpatialPolygons(w_map, IDs = w_po, proj4string = WGS84) # map to polygon
#'
#' e <- extent(reg)
#' mask <- as(e, 'SpatialPolygons')
#'
#' variables <- crop(vars, mask)
#' save <- TRUE
#' name <- "test"
#'
#' ## ranges on evironmental factor maps
#' ranges_emaps(ranges = ranges, variables = variables,
#'              save_fig = save, name = name)
#'
#' #dev.off() # for returning to default par settings

ranges_emaps <- function(ranges, add_occurrences = TRUE, variables, range_colors = NULL,
                         variables_color = NULL, ranges_legend = TRUE,
                         legend_position = "bottomright", legend_size = 0.7,
                         scalebar = FALSE, scalebar_position = "bottomleft",
                         scalebar_length = 100, zoom = 1.3, save_fig = FALSE,
                         name = "ranges_emaps", format = "png", resolution = 300,
                         width = 166) {

  # testing potential issues
  if (missing(ranges)) {
    stop("ranges must exist. See the function's help for more details.")
  }

  # preparing data
  ## plain projection
  WGS84 <- sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

  ## unlist nested lists
  r <- lapply(ranges, unlist)

  ## extracting data
  if (add_occurrences == TRUE) {
    lranges <- sapply(ranges, length)

    if (any(lranges > 2)){
      n <- which(lranges > 2)[1]
      ## occurrences
      if (isnested(r)) {
        sp_rec <- ranges[[n]]$Species_unique_records
        AEQD <- ranges[[n]]$Species_range@proj4string # initial projection of ranges
      }else {
        sp_rec <- ranges$Species_unique_records
        AEQD <- ranges$Species_range@proj4string
      }
    }else {
      AEQD <- ranges[[n]]$Species_range@proj4string
      warning("None of the objects in \"range\" contain occurrences, \"add_occurrences = TRUE\" ignored.")
    }
  }

  # variable treatment
  ## reducing
  if (dim(variables)[3] > 21) {
    nvar <- dim(variables)[3]
    variables <- variables[[1:21]]
    warning(paste("Only 21 of the", nvar, "variables will be used. See function documentation."))
  }

  ## projecting
  variables <- raster::projectRaster(variables, crs = WGS84)
  variables <- raster::projectRaster(variables, crs = AEQD)

  # getting the species ranges and x and y limits of plots
  if (isnested(r)) {
    sp_ranges <- list()
    xboxs1 <- vector()
    xboxs2 <- vector()
    yboxs1 <- vector()
    yboxs2 <- vector()

    for (i in 1:length(ranges)) {
      sp_ranges[[i]] <- ranges[[i]]$Species_range
      xboxs1[i] <- as.numeric(c(sp_ranges[[i]]@bbox[1, 1]))
      xboxs2[i] <- as.numeric(c(sp_ranges[[i]]@bbox[1, 2]))
      yboxs1[i] <- as.numeric(c(sp_ranges[[i]]@bbox[2, 1]))
      yboxs2[i] <- as.numeric(c(sp_ranges[[i]]@bbox[2, 2]))
    }
    xbox <- as.numeric(c(min(xboxs1), max(xboxs2)))
    ybox <- as.numeric(c(min(yboxs1), max(yboxs2)))

  }else {
    sp_ranges <- ranges$Species_range

    xbox <- as.numeric(c(sp_ranges@bbox[1, 1:2]))
    ybox <- as.numeric(c(sp_ranges@bbox[2, 1:2]))
  }

  rnames <- names(ranges)

  # range and variable colors
  if (is.null(range_colors)) {
    cols <- c("black", "blue", "red", "purple", "cyan", "purple", "magenta",
              "black", "blue", "red", "purple", "cyan", "purple", "magenta")
    if (isnested(r)) {
      colors <- cols[1:length(ranges)]
    }else {
      colors <- cols[1]
    }
  }else {
    colors <- range_colors
  }

  if (is.null(variables_color)) {
    variables_color <- rev(terrain.colors(255))
  }

  # plot
  ## limits of map
  xlim <- c(xbox[1] - ((((xbox[2] - xbox[1]) * zoom) -
                          (xbox[2] - xbox[1])) / 2),
            xbox[2] + ((((xbox[2] - xbox[1]) * zoom) -
                          (xbox[2] - xbox[1])) / 2))
  ylim <- c(ybox[1] - ((((ybox[2] - ybox[1]) * zoom) -
                          (ybox[2] - ybox[1])) / 2),
            ybox[2] + ((((ybox[2] - ybox[1]) * zoom) -
                          (ybox[2] - ybox[1])) / 2))

  ## par options
  fig_conf <- list(c(1, 1), c(1, 2), c(1, 3), c(2, 2), c(2, 3), c(2, 3), c(3, 3), c(3, 3),
                   c(3, 3), c(4, 3), c(4, 3), c(4, 3), c(5, 3), c(5, 3), c(5, 3), c(6, 3),
                   c(6, 3), c(6, 3), c(7, 3), c(7, 3), c(7, 3))
  for (i in 1:dim(variables)[3]) {
    if (dim(variables)[3] == i) {fig_config <- fig_conf[[i]]}
  }

  par(mar = c(0, 0, 0, 2), mfrow = fig_config)

  ## the plot and variable legends
  for (i in 1:dim(variables)[3]) {
    raster::plot(variables[[i]], col = variables_color, xlim = xlim,
                 ylim = ylim, legend = FALSE, axes = FALSE)

    if (isnested(r)) {
      for (j in 1:length(sp_ranges)) {
        sp::plot(sp_ranges[[j]], col = "transparent", border = colors[j], add = TRUE)
      }
    }else {
      sp::plot(sp_ranges, col = "transparent", border = colors[1])
    }

    if (add_occurrences == TRUE & any(lranges > 2)) {
      points(sp_rec, pch = 20, cex = 1)
    }

    var_range <- c(ceiling(minValue(variables[[i]])),
                   floor(maxValue(variables[[i]])))

    raster::plot(variables[[i]], legend.only = TRUE, col = variables_color,
                 legend.width = 1, legend.shrink = 0.8,
                 axis.args = list(at = c(var_range[1], var_range[2]),
                                  labels = c(var_range[1], var_range[2]),
                                  line = -0.519, cex.axis = 0.8),
                 legend.args = list(text = names(variables)[i], side = 4, font = 2,
                                    line = 0.5, cex = 0.9))
  }

  ## scale bar
  if (scalebar == TRUE) {
    if (class(scalebar_position) == "character") {
      if (scalebar_position == "topright"){
        xscale <- xlim[1] + ((xlim[2] - xlim[1]) * 0.80)
        yscale <- ylim[1] + ((ylim[2] - ylim[1]) * 0.93)
      }
      if (scalebar_position == "topleft") {
        xscale <- xlim[1] + ((xlim[2] - xlim[1]) * 0.02)
        yscale <- ylim[1] + ((ylim[2] - ylim[1]) * 0.93)
      }
      if (scalebar_position == "bottomleft") {
        xscale <- xlim[1] + ((xlim[2] - xlim[1]) * 0.02)
        yscale <- ylim[1] + ((ylim[2] - ylim[1]) * 0.04)
      }
      if (scalebar_position == "bottomright") {
        xscale <- xlim[1] + ((xlim[2] - xlim[1]) * 0.80)
        yscale <- ylim[1] + ((ylim[2] - ylim[1]) * 0.04)
      }
    }else {
      xscale <- scalebar_position[1]
      yscale <- scalebar_position[2]
    }

    scalebarf(loc = c(xscale, yscale), length = (scalebar_length * 1000),
              division.cex = 0.7)
  }

  ## ranges legends
  if (ranges_legend == TRUE) {
    if (class(legend_position) == "character") {
      if (add_occurrences == TRUE & any(lranges > 2)) {
        legend(legend_position, legend = c("Occurrences", rnames),  bty = "n", inset = 0.03,
               pch = c(20, rep(22, length(rnames))), pt.cex = c(1, rep(1.5, length(rnames))),
               cex = legend_size, col = c("black", colors))
      }else {
        legend(legend_position, legend = c(rnames),  bty = "n", inset = 0.03,
               pch = c(rep(22, length(rnames))), pt.cex = c(rep(1.5, length(rnames))),
               cex = legend_size, col = c(colors))
      }
    }else {
      xleg <- legend_position[1]
      yleg <- legend_position[2]
      if (add_occurrences == TRUE & any(lranges > 2)) {
        legend(x = xleg, y = yleg, legend = c("Occurrences", rnames),  bty = "n", inset = 0.03,
               pch = c(20, rep(22, length(rnames))), pt.cex = c(1, rep(1.5, length(rnames))),
               cex = legend_size, col = c("black", colors))
      }else {
        legend(x = xleg, y = yleg, legend = c(rnames),  bty = "n", inset = 0.03,
               pch = c(rep(22, length(rnames))), pt.cex = c(rep(1.5, length(rnames))),
               cex = legend_size, col = c(colors))
      }
    }
  }

  # saving the figure
  h_rate <- (ybox[2] - ybox[1]) / (xbox[2] - xbox[1])
  height <- width * h_rate / fig_config[2] * fig_config[1]

  if (save_fig == TRUE) {
    cat("\nWriting figure in working directory.\n")
    if (format == "bmp") {
      bmp(filename = paste(name, "bmp", sep = "."), width = width, height = height,
          units = "mm", res = resolution)
    }
    if (format == "png") {
      png(filename = paste(name, "png", sep = "."), width = width, height = height,
          units = "mm", res = resolution)
    }
    if (format == "jpeg") {
      jpeg(filename = paste(name, "jpg", sep = "."), width = width, height = height,
           units = "mm", res = resolution)
    }
    if (format == "tiff") {
      tiff(filename = paste(name, "tif", sep = "."), width = width, height = height,
           units = "mm", res = resolution)
    }
    if (format == "pdf") {
      pdf(file = paste(name, "pdf", sep = "."), width = width)
    }

    par(mar = c(0, 0, 0, 2), mfrow = fig_config)

    ## the plot and variable legends
    for (i in 1:dim(variables)[3]) {
      raster::plot(variables[[i]], col = variables_color, xlim = xlim,
                   ylim = ylim, legend = FALSE, axes = FALSE)

      if (isnested(r)) {
        for (j in 1:length(sp_ranges)) {
          sp::plot(sp_ranges[[j]], col = "transparent", border = colors[j], add = TRUE)
        }
      }else {
        sp::plot(sp_ranges, col = "transparent", border = colors[1])
      }

      if (add_occurrences == TRUE & any(lranges > 2)) {
        points(sp_rec, pch = 20, cex = 1)
      }

      var_range <- c(ceiling(minValue(variables[[i]])),
                     floor(maxValue(variables[[i]])))

      raster::plot(variables[[i]], legend.only = TRUE, col = variables_color,
                   legend.width = 1, legend.shrink = 0.8,
                   axis.args = list(at = c(var_range[1], var_range[2]),
                                    labels = c(var_range[1], var_range[2]),
                                    line = -0.519, cex.axis = 0.8),
                   legend.args = list(text = names(variables)[i], side = 4, font = 2,
                                      line = 0.5, cex = 0.9))
    }

    ## scale bar
    if (scalebar == TRUE) {
      if (class(scalebar_position) == "character") {
        if (scalebar_position == "topright"){
          xscale <- xlim[1] + ((xlim[2] - xlim[1]) * 0.75)
          yscale <- ylim[1] + ((ylim[2] - ylim[1]) * 1.01)
        }
        if (scalebar_position == "topleft") {
          xscale <- xlim[1] + ((xlim[2] - xlim[1]) * 0.07)
          yscale <- ylim[1] + ((ylim[2] - ylim[1]) * 1.01)
        }
        if (scalebar_position == "bottomleft") {
          xscale <- xlim[1] + ((xlim[2] - xlim[1]) * 0.07)
          yscale <- ylim[1] + ((ylim[2] - ylim[1]) * -0.01)
        }
        if (scalebar_position == "bottomright") {
          xscale <- xlim[1] + ((xlim[2] - xlim[1]) * 0.75)
          yscale <- ylim[1] + ((ylim[2] - ylim[1]) * -0.01)
        }
      }else {
        xscale <- scalebar_position[1]
        yscale <- scalebar_position[2]
      }

      scalebarf(loc = c(xscale, yscale), length = (scalebar_length * 1000),
                division.cex = 0.7)
    }

    ## ranges legends
    if (ranges_legend == TRUE) {
      if (class(legend_position) == "character") {
        if (add_occurrences == TRUE & any(lranges > 2)) {
          legend(legend_position, legend = c("Occurrences", rnames),  bty = "n", inset = 0.03,
                 pch = c(20, rep(22, length(rnames))), pt.cex = c(1, rep(1.5, length(rnames))),
                 cex = legend_size, col = c("black", colors))
        }else {
          legend(legend_position, legend = c(rnames),  bty = "n", inset = 0.03,
                 pch = c(rep(22, length(rnames))), pt.cex = c(rep(1.5, length(rnames))),
                 cex = legend_size, col = c(colors))
        }
      }else {
        xleg <- legend_position[1]
        yleg <- legend_position[2]
        if (add_occurrences == TRUE & any(lranges > 2)) {
          legend(x = xleg, y = yleg, legend = c("Occurrences", rnames),  bty = "n", inset = 0.03,
                 pch = c(20, rep(22, length(rnames))), pt.cex = c(1, rep(1.5, length(rnames))),
                 cex = legend_size, col = c("black", colors))
        }else {
          legend(x = xleg, y = yleg, legend = c(rnames),  bty = "n", inset = 0.03,
                 pch = c(rep(22, length(rnames))), pt.cex = c(rep(1.5, length(rnames))),
                 cex = legend_size, col = c(colors))
        }
      }
    }

    invisible(dev.off())
  }
}
