#' Exploring occurrences before creating range maps
#'
#' @description rangemap_explore generates simple figures to visualize species
#' occurrence data in the geography.
#'
#' @param occurrences a data.frame containing geographic coordinates of species
#' occurrences, columns must be: Species, Longitude, and Latitude. Geographic
#' coordinates must be in decimal degrees  (WGS84).
#' @param show_countries (logical) if \code{TRUE}, ISO 3 country codes will label
#' country polygons. Default = \code{FALSE}.
#' @param graphic_device (logical) if \code{TRUE}, a new graphic device is opened
#' to plot the figure. Default = \code{FALSE}.
#'
#' @return
#' A simple figure of species occurrences in a geographical context.
#'
#' @details
#' Base map of countries of the world is a SpatialPolygonsDataFrame downloaded from
#' the Natural Earth database (scale = 50).
#'
#' @usage
#' rangemap_explore(occurrences, show_countries = FALSE, graphic_device = FALSE)
#'
#' @export
#'
#' @importFrom sp CRS spTransform plot SpatialPointsDataFrame
#' @importFrom rgeos gCentroid
#' @importFrom graphics points text axis box
#' @importFrom grDevices dev.new
#' @import rnaturalearthdata
#'
#' @examples
#' # getting the data
#' data("occ_f", package = "rangemap")
#'
#' # simple figure of the species occurrence data
#' rangemap_explore(occurrences = occ_f, show_countries = TRUE)

rangemap_explore <- function(occurrences, show_countries = FALSE,
                             graphic_device = FALSE) {

  # testing potential issues
  if (missing(occurrences)) {
    stop("Argument occurrences is necessary to perform the analysis")
  }

  if (dim(occurrences)[2] != 3) {
    stop("occurrences data.frame must have the following columns: \nSpecies, Longitude, and Latitude")
  }

  # erase duplicate records
  occ <- as.data.frame(unique(occurrences))[, 1:3]
  colnames(occ) <- c("Species", "Longitude", "Latitude")

  # make a spatial object from coordinates
  WGS84 <- sp::CRS("+init=epsg:4326")
  occ_sp <- sp::SpatialPointsDataFrame(coords = occ[, 2:3], data = occ,
                                       proj4string = WGS84)

  # bringing maps if polygons false
  polygons1 <- simple_wmap(which = "simple")

  # keeping only records in land with better resolution polygon
  requireNamespace(package = "rnaturalearthdata", quietly = TRUE)
  data("countries50", package = "rnaturalearthdata", envir = environment())
  polygons <- sp::spTransform(countries50, WGS84)

  occ_sp <- occ_sp[polygons, ]
  polygons1 <- polygons1[occ_sp, ] # polygons with occurrences


  # plot a background map and the range
  ## limits of map
  xlim <- as.numeric(c(polygons1@bbox[1, 1:2]))
  ylim <- as.numeric(c(polygons1@bbox[2, 1:2]))

  ## labels
  if (show_countries == TRUE) {
    lab <- polygons@data$adm0_a3_is
    cent <- rgeos::gCentroid(polygons, byid = TRUE)
    cords <- cent@coords
  }

  ## generic plot
  if (graphic_device == TRUE) {
    dev.new()
  }

  sp::plot(polygons, xlim = xlim, ylim = ylim, col = "grey92", xaxt = "n",
           yaxt = "n")
  points(occ_sp, pch = 21, bg = scales::alpha("yellow", 0.3), cex = 1.2)

  if (show_countries == TRUE) {
    text(x = cords, labels = lab, cex = 0.65)
  }

  axis(side = 1, tcl = 0.3, lwd.ticks = 1,
       mgp = c(0, -1.3, 0), cex.axis = 0.7)
  axis(side = 2, tcl = 0.3, lwd.ticks = 1,
       mgp = c(0, -1.3, 0), cex.axis = 0.7, las = 1)

  box()
}
