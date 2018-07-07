#' Comparison of species ranges in environmental space
#'
#' @description ranges_envcomp generates a two dimensional comparison of a species'
#' ranges created using distinct algortihms, to visualize implications of selecting
#' one of them if environmental conditions are considered.
#'
#' @param occurrences a data.frame containing species occurrences, columns must be:
#' Species, Longitude, and Latitude.
#' @param ranges (list) of SpatialPolygons or SpatialPolygonsDataFrame or a
#' list of objects produced with any of the following functions:
#' \code{\link{rangemap_buff}}, \code{\link{rangemap_bound}}, \code{\link{rangemap_hull}},
#' \code{\link{rangemap_enm}}, and \code{\link{rangemap_tsa}}. For graphical purposes, using
#' up to four ranges is recomended.
#' @param variables a RasterStack object of environmental variables that will be used for
#' create the principal components to represent the environmental space.
#' @param save_fig (logical) if TRUE a figure in format = format will be written in the working
#' directory, appart of the returned object.
#'
#' @return A figure showing, in the environmental space, the species ranges generated with any
#' of the functions: \code{\link{rangemap_buff}}, \code{\link{rangemap_bound}},
#' \code{\link{rangemap_hull}}, \code{\link{rangemap_enm}}, and \code{\link{rangemap_tsa}}.
#'
#' @details .
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
#' # range based on boundaries
#' ## checking which countries may be involved in the analysis
#' rangemap_explore(occurrences = occ_g)
#'
#' level <- 0
#' adm <- "Ecuador" # Athough no record is on this country, we know it is in Ecuador
#'
#' countries <- c("PER", "BRA", "COL", "VEN", "ECU", "GUF", "GUY", "SUR", "BOL")
#'
#' bound <- rangemap_bound(occurrences = occ_g, adm_areas = adm, country_code = countries,
#'                         boundary_level = level)
#'
#'
#' # range based on concave hulls
#' dist1 <- 250000
#' hull1 <- "concave"
#'
#' concave <- rangemap_hull(occurrences = occ_g, hull_type = hull1, buffer_distance = dist1)
#'
#'
#' # ranges comparison in environmental space
#' ## list of ranges
#' ranges <- list(buff, bound, concave)
#' names(ranges) <- c("buff", "bound", "concave")
#'
#' ## other data for environmental comparisson
#' if(!require(raster)){
#'   install.packages("raster")
#'   library(raster)
#' }
#' if(!require(maps)){
#' install.packages("maps")
#' library(maps)
#' }
#'
#' vars <- getData("worldclim", var = "bio", res = 5)
#'
#' ## mask variables to region of interest
#' WGS84 <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
#' w_map <- map(database = "world", regions = c("Ecuador", "Peru", "Bolivia", "Colombia", "Venezuela",
#'                                              "Suriname", "Guyana", "French Guyana"),
#'              fill = TRUE, plot = FALSE) # map of the world
#' w_po <- sapply(strsplit(w_map$names, ":"), function(x) x[1]) # preparing data to create polygon
#' reg <- map2SpatialPolygons(w_map, IDs = w_po, proj4string = WGS84) # map to polygon
#'
#' e <- extent(reg)
#' mask <- as(e, 'SpatialPolygons')
#'
#' variables <- crop(vars, mask)
#'
#' ## comparison
#' env_comp <- ranges_envcomp(occurrences = occ_g, ranges = ranges, variables = variables)

ranges_envcomp <- function(occurrences, ranges, variables, save_fig = FALSE) {

  # testing potential issues

  # preparing data
  ## ocurrencias
  occ <- as.data.frame(unique(occurrences))[, 1:3]
  colnames(occ) <- c("Species", "Longitude", "Latitude")

  occ1 <- occ[, 2:3]
  colnames(occ1) <- c("x", "y")

  ## projection
  WGS84 <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

  # pca
  ## raster to varaibles data
  idata <- raster::rasterToPoints(variables)

  ## varaible data in occurrences
  pdata <- na.omit(cbind(occ1, raster::extract(variables, occ1)))

  ## combining these data
  vdata <- rbind(idata, pdata)

  ## pca with vdata
  pcav <- prcomp(na.omit(vdata[, 3:dim(vdata)[2]]), center = TRUE,
                  scale = TRUE)

  ## getting the data of components in points
  pca_scores = pcav$x
  pc3 <- data.frame(vdata[, 1:2], pca_scores[, 1:3])

  pc_occ <- pc3[(length(pc3[, 1]) - length(pdata[, 1]) + 1):length(pc3[, 1]), ]

  pc_points <- sp::SpatialPointsDataFrame(coords = pc_occ[, 1:2], data = pc_occ[, 3:dim(pc3)[2]],
                                          proj4string = WGS84)

  if (dim(pc3)[1] > 50000) {
    pc3 <- pc3[sample(row.names(pc3), 50000), ]
  }

  pc_var <- sp::SpatialPointsDataFrame(coords = pc3[, 1:2], data = pc3[, 3:dim(pc3)[2]],
                                          proj4string = WGS84)

  # project the points using their centriods as reference
  centroid <- rgeos::gCentroid(pc_points, byid = FALSE)

  AEQD <- sp::CRS(paste("+proj=aeqd +lat_0=", centroid@coords[2], " +lon_0=", centroid@coords[1],
                        " +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs", sep = ""))

  pc_spp <- sp::spTransform(pc_points, AEQD)

  pc_varp <- sp::spTransform(pc_var, AEQD)

  # getting the species ranges from objects in ranges
  if (class(unlist(ranges)) == "list") {
    sp_ranges <- list()
    for (i in 1:length(ranges)) {
      sp_ranges[[i]] <- sp::spTransform(ranges[[i]][[3]], AEQD)
    }
  }else {
    sp_ranges <- ranges
  }


  rnames <- names(ranges)

  # getting environmental (PCs) data in ranges
  cat("\nGetting environmental conditions in ranges, please wait...\n")
  env_ranges <- list()
  for (i in 1:length(ranges)) {
    env_ranges[[i]] <- pc_varp[sp_ranges[[i]], ]
  }

  env_ranges1 <- env_ranges

  colors <- c("black", "blue", "yellow", "purple", "red", "brown", "pink", "green")

  # plot
  cat("\nCreating an interactive visualization...\n")
  p <- plotly::plot_ly()
  for(i in 1:length(env_ranges1)){
    ell <- rgl::ellipse3d(cov(env_ranges1[[i]]@data))
    p <- plotly::add_trace(p, x = ell$vb[1, ], y = ell$vb[2, ], z = ell$vb[3, ],
                           type = "scatter3d", size = 1,
                           mode = "markers",
                           marker = list(color = colors[i],
                                         opacity = 0.01), name = rnames[i])
  }

  points <- pc_points@data

  p <- plotly::add_trace(p, x = pc_varp$PC1, y = pc_varp$PC2, z = pc_varp$PC3, mode = "markers", type = "scatter3d",
                      marker = list(size = 2, color = "gray65",
                                    opacity = 0.02, symbol = 104), name = "Available space") %>%
    plotly::add_trace(x = points$PC1, y = points$PC2, z = points$PC3, mode = "markers", type = "scatter3d",
              marker = list(size = 6, color = "black", symbol = 104), name = "Occurrences") %>%
    plotly::layout(scene = list(xaxis = list(title = "PC 1", backgroundcolor="white", showbackground=TRUE,
                                     titlefont = list(color = "black", family = "Arial", size = 16)),
                        yaxis = list(title = "PC 2", backgroundcolor="white", showbackground=TRUE,
                                     titlefont = list(color = "black", family = "Arial", size = 16)),
                        zaxis = list(title = "PC 3", backgroundcolor="white", showbackground=TRUE,
                                     titlefont = list(color = "black", family = "Arial", size = 16)),
                        camera = list(eye = list(x = 1.95, y = 1.25, z = 1.35))))

  # present the figure
  print(p)

  # saving the figure
  if (save_fig == TRUE) {
    connection <- !is.null(curl::nslookup("r-project.org", error = FALSE))
    if (connection == FALSE) {
      stop("\nInternet conection is required to download the figure.\n")

    }else {
      cat("\nExporting the figure, this process may take some time, please wait...\n")
      # Save viewer settings (e.g. RStudio viewer pane)
      op <- options()

      # Set viewer to web browser
      options(viewer = NULL)

      # Use web browser to save image
      p %>% htmlwidgets::onRender(
        "function(el, x) {
           var gd = document.getElementById(el.id);
           Plotly.downloadImage(gd, {format: 'svg', width: 1000, height: 800, filename: 'ranges_env_comparison'});
        }"
      )

      # Restore viewer to old setting (e.g. RStudio)
      options(viewer = op$viewer)

      cat("Figure saved in your browser download folder as ranges_env_comparison.svg.")
    }
  }

  cat("\nFor further work with the figure use the object created with the function.\n")

  # return results
  return(p)
}
