#' Comparison of species ranges in environmental space
#'
#' @description ranges_envcomp generates a two dimensional comparison of a species'
#' ranges created using distinct algortihms, to visualize implications of selecting
#' one of them if environmental conditions are considered.
#'
#' @param occurrences a data.frame containing species occurrences, columns must be:
#' Species, Longitude, and Latitude.
#' @param ranges a list of objects produced with any of the following functions:
#' \code{\link{rangemap_buff}}, \code{\link{rangemap_bound}}, \code{\link{rangemap_hull}},
#' \code{\link{rangemap_enm}}, and \code{\link{rangemap_tsa}}.
#' @param variables a RasterStack object of environmental variables that will be used for
#' create the principal components to represent the environmental space.
#' @param save (logical) if TRUE a figure in format = format will be written in the working
#' directory, appart of the returned object.
#' @param format (character) format of the figure that will be written in the working directory
#' if export = TRUE.
#' @param name (character) valid if save = TRUE. The name of the figure to be exported.
#'
#' @return A figure showing, in the environmental space, the species ranges generated with any
#' of the functions: \code{\link{rangemap_buff}}, \code{\link{rangemap_bound}},
#' \code{\link{rangemap_hull}}, \code{\link{rangemap_enm}}, and \code{\link{rangemap_tsa}}.
#'
#' @details Trend surface analysis Is a method based on low-order polynomials of spatial coordinates
#' for estimating a regular grid of points from scattered observations.
#'
#' @examples
#' # comming soon

ranges_envcomp <- function(occurrences, ranges, variables, save = FALSE, format = "png", name) {

  # testing potential issues

  # pca
  ## raster to varaibles data
  vdata <- raster::rasterToPoints(variables)

  ## pca with vdata
  pcav <- prcomp(na.omit(vdata[, 3:dim(vdata)[2]]), center = TRUE,
                  scale = TRUE)

  ## getting the data of components in points
  pca_scores = pcav$x
  pc3 <- data.frame(vdata[, 1:2], pca_scores[, 1:3])

  pc_points <- sp::SpatialPointsDataFrame(coords = occ[, 2:3], data = occ,
                                          proj4string = WGS84)

  # polygons to avoind records in the ocean
  if (missing(polygons)) {
    w_map <- maps::map(database = "world", fill = TRUE, plot = FALSE) # map of the world

    w_po <- sapply(strsplit(w_map$names, ":"), function(x) x[1]) # preparing data to create polygon
    polygons <- maptools::map2SpatialPolygons(w_map, IDs = w_po, proj4string = WGS84) # map to polygon
  }

  pc_land <- pc_points[!is.na(sp::over(pc_points, polygons)), ]

  # project the points using their centriods as reference
  centroid <- rgeos::gCentroid(pc_land, byid = FALSE)

  AEQD <- sp::CRS(paste("+proj=aeqd +lat_0=", centroid@coords[2], " +lon_0=", centroid@coords[1],
                        " +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs", sep = ""))

  pc_proj <- sp::spTransform(pc_land, AEQD)

  # getting the species ranges from objects in ranges
  sp_ranges <- list()
  for (i in 1:length(ranges)) {
    sp_ranges[[i]] <- ranges[[i]][[3]]
  }
  rnames <- names(ranges)

  # getting environmental (PCs) data in ranges
  env_ranges <- list()
  for (i in 1:length(ranges)) {
    env_ranges[[i]] <- pc_proj[!is.na(sp::over(pc_proj, sp_ranges[[i]])), ]
  }

  # plot a background environment using the entire dataset from variables
  ellipse <- list()
  for (i in 1:length(ranges)) {
    ellipse[[i]] <- rgl::ellipse3d(cov(env_ranges[[i]][, 3:5]))
  }

  for(i in 1:length(ranges)) {

    W <- c(0,cumsum( sqrt(Delta) * rnorm(N, mean=mean, sd=sd)))

    assign(paste("W_",i,sep=""),W)
    assign(paste("Name_", i, sep=""), paste("Name",i,sep=""))
    if(i==1){
      pString<-"p<-plot_ly(x = t, y = W_1, name='W1')"
    } else {
      pString<-paste(pString, " %>% add_trace(x=t, y =",  eval(paste("W", i, sep="_")),
                     ", name=", eval(paste("Name", i, sep="_")), ")", sep="")
    }

  }
  eval(parse(text=pString))
  print(p)

  # plot environments of ranges created with distinct algorithms with transparencies

  # plot an ellipsoid
  # insert a legend
  # present a figure with multiple panels?
  # return results
}
