#' Geodesic buffer for unprojected points
#'
#' @description geobuffer_points helps in creating geodesic buffers of points
#' represented by longitude and latitude coordinates.
#'
#' @param data matrix of geographic unprojected coordinates (i.e., in EPSG:4326).
#' Columns must be "longitude" and "latitude", in that order.
#' @param radius (numeric) radius of buffer in meters.
#' @param by_point (logical) whether or not to do buffers by point. If FALSE, the
#' default, buffer polygons that overlap will be dissolved to obtain an only
#' feature. Default = 100.
#' @param n_segments (numeric) number of segments to approximate a circle.
#' @param wrap_antimeridian (logical) whether or not to wrap buffers in the
#' antimeridian when they overpass it.
#'
#' @details
#' The process is done using an algorithm that calculates the buffer in the North
#' Pole and then rotates this buffer into the actual location. The rotation is
#' effected by converting the original buffer to geocentric Cartesian (XYZ)
#' coordinates. A matrix multiplication helps to  rotate those coordinates
#' along the Prime Meridian to the target latitude, converting the coordinates
#' back to Geographic (WGS84). Then the buffer is spun around the Earth's
#' axis by adding the target longitude to each second coordinate.
#'
#' The algorithm was developed by a moderator of the Geographic Information
#' Systems Stack Exchange (online community). More details are available available
#' at the following
#' \href{https://gis.stackexchange.com/questions/250389/euclidean-and-geodesic-buffering-in-r}{site}.
#'
#' @return
#' A SpatialPolygons object of buffered points. Final projection is WGS84
#' (EPSG:4326).
#'
#' @usage
#' geobuffer_points(data, radius, by_point = FALSE, n_segments = 100,
#'                  wrap_antimeridian = FALSE)
#'
#' @export
#' @importFrom sp SpatialPolygons Polygons Polygon rbind.SpatialPolygons CRS
#' @importFrom raster disaggregate
#' @importFrom rgeos gUnaryUnion
#' @importFrom dplyr %>%
#' @importFrom sf st_wrap_dateline st_union as_Spatial
#' @importFrom methods as
#'
#' @examples
#' #data
#' data("occ_p", package = "rangemap")
#' coords <- occ_p[, 2:3]
#'
#' # buffers
#' bufferp <- geobuffer_points(data = coords, radius = 25000)
#'
#' sp::plot(bufferp, axes = TRUE)


geobuffer_points <- function(data, radius, by_point = FALSE, n_segments = 100,
                             wrap_antimeridian = FALSE) {
  if (missing(data)) {
    stop("Argument 'data' is necessary to perform the analysis")
  }
  if (missing(radius)) {
    stop("Argument 'radius' is necessary to perform the analysis")
  }
  # data as needed
  data <- t(data)[2:1, ]

  # buffer the North Pole and convert to XYZ once and for all.
  p.0 <- circle.create(radius, resolution = n_segments)
  p <- latlon.to.xyz(p.0)

  # buffer the set of points
  circles <- apply(data, 2, function(x) {
    rotate(p, x[1], x[2])
  })

  # convert into an array indexed by (latlon, vertex, point id).
  n <- ncol(data)
  circles <- array(circles, c(2, n_segments + 1, n))

  # convert into spatial polygons
  polys <- lapply(1:n, function(x) {
    coord_pol <- cbind(circles[2, , x], circles[1, , x])
    sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(coord_pol)), ID = 1)))
  })
  polys <- do.call(sp::rbind.SpatialPolygons, c(polys, list(makeUniqueIDs = TRUE)))

  # union if needed
  if (by_point == FALSE) {
    polys <- rgeos::gUnaryUnion(polys)
    polys <- raster::disaggregate(polys)
  }

  # defining projection
  WGS84 <- sp::CRS("+init=epsg:4326")
  polys@proj4string <- WGS84

  # reprojecting for precise representation in antimeridians
  LAEA <- sp::CRS(paste("+proj=laea +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=GRS80",
                        "+towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
  polys <- sp::spTransform(polys, LAEA)
  polys <- sp::spTransform(polys, WGS84)

  # fixing if needed
  if (wrap_antimeridian == TRUE) {
    ext <- polys@bbox[1, ]
    if (ext[1] < -180 | ext[2] > 180) {
      polys <- as(polys, Class = "sf") %>%
        sf::st_wrap_dateline(options = c("WRAPDATELINE=YES")) %>%
        sf::st_union()

      polys <- sf::as_Spatial(polys)
      polys <- raster::disaggregate(polys)
    }
  }

  return(polys)
}
