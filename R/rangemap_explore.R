#' Figures for exploring occurrences on maps before creating range maps
#'
#' @description rangemap_explore generates simple figures to visualize species occurrence
#' data in the geographic space before using other function sof this package.
#'
#' @param occurrences a data.frame containing species occurrences, columns must be:
#' Species, Longitude, and Latitude.
#' @param polygons a SpatialPolygon object to be used as base map for plotting the species range.
#' If not provided, a simplified world map will be used.
#'
#' @return A simple figure of the species occurrences in a geographical context.
#'
#' @examples
#'
#' if(!require(rgbif)){
#'   install.packages("rgbif")
#'   library(rgbif)
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
#' # simple figure of the species occurrence data
#' explore_map<- rangemap_explore(occ_g)
#'
#' dev.off() # for returning to default par settings

# Dependencies: maptools (data(wrld_simpl)),
#               scales (alpha),
#               sp (plot, spTransform, CRS),

rangemap_explore <- function(occurrences, polygons) {

  suppressMessages(library(maptools))

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
  WGS84 <- sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") # generic
  occ_sp <- sp::SpatialPointsDataFrame(coords = occ[, 2:3], data = occ,
                                       proj4string = WGS84)

  # bringing maps if polygons false
  if (missing(polygons)) {
    data(wrld_simpl)
    polygons <- wrld_simpl
  }
  rm("wrld_simpl", pos = ".GlobalEnv")

  # keeping only records in land
  occ_sp <- occ_sp[!is.na(sp::over(occ_sp, polygons)), ]

  # projecting polygons and occurrences
  ROBIN <- sp::CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") # for pretty maps

  occ_pr <- sp::spTransform(occ_sp, ROBIN)
  polygons <- sp::spTransform(polygons, ROBIN) # for base map

  # plot a background map and the range
  ## limits of map
  xlim <- as.numeric(c(occ_pr@bbox[1, 1:2]))
  ylim <- as.numeric(c(occ_pr@bbox[2, 1:2]))

  ## generic plot
  par(mar = c(0, 0, 0, 0), tcl = 0.25)
  sp::plot(polygons, xlim = xlim, ylim = ylim, col = "grey90") # base map
  points(occ_sp, pch = 21, bg = scales::alpha("blue", 0.6), cex = 0.95)  #plot my sample sites
  box()
}


