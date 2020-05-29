#' Helper function to test if lists are nested
#' @param l list to be tested.
#' @export
isnested <- function(l) {
  stopifnot(is.list(l))
  for (i in l) {
    if (is.list(i)) {return(TRUE)}
  }
  return(FALSE)
}


#' Get a simplified SpatialPolygonsDataFrame of the world
#' @param which (charatcer) name of type of SpatialPolygons to be obtained. Options
#' are: "simple" and "simplest"; default = "simplest".
#' @return A simplified SpatialPolygonsDataFrame of the world in WGS84 projection.
#' @export
#' @importFrom sp CRS SpatialPolygonsDataFrame spTransform
#' @importFrom maps map
#' @importFrom maptools map2SpatialPolygons
#' @examples
#' map <- simple_wmap()

simple_wmap <- function(which = "simplest") {
  WGS84 <- sp::CRS("+init=epsg:4326")
  if (which == "simplest") {
    data("wrld_simpl", package = "maptools")
    polygons <- sp::spTransform(wrld_simpl, WGS84)
    rm("wrld_simpl", pos = ".GlobalEnv")
  } else {
    w_map <- maps::map(database = "world", fill = TRUE, plot = FALSE)

    w_po <- sapply(strsplit(w_map$names, ":"), function(x) x[1])
    polygons <- maptools::map2SpatialPolygons(w_map, IDs = w_po, proj4string = WGS84)
    polygons <- sp::SpatialPolygonsDataFrame(polygons, data = data.frame(ID = 1:length(polygons)),
                                             match.ID = FALSE)
  }
  return(polygons)
}


#' Prepare Equal-Area or Equidistante Projection
#' @param occurrences matrix or data.frame conaining coordinates to serve as
#' a reference for the center of the projection. Columns must be:
#' "longitude" and "latitude", in that order.
#' @param spatial_onject Spatial* objects, Poinst or Polygons, to be used to
#' calculate a reference for the center of the projection. Projection must be
#' WGS84 (EPSG:4326).
#' @details If arguments are not defined projection is centered in 0, 0 for
#' longitude and latitude.
#' @return An object of class CRS.
#' @export
#' @importFrom rgeos gCentroid
#' @importFrom sp CRS
#' @examples
#' LAEA_projection()
#'
#' data("occ_p", package = "rangemap")
#' occ <- unique(occ_p)[, 2:3]
#' AED_projection(occ)
#' @rdname LAEA_projection

LAEA_projection <- function(occurrences = NULL, spatial_onject = NULL) {
  if (!is.null(occurrences) | !is.null(spatial_onject)) {
    if (!is.null(occurrences)) {
      if (!class(occurrences)[1] %in% c("matrix", "data.frame", "tbl_df")) {
        stop("Argument 'occurrences' is not valid, see functions help.")
      }
      cent <- apply(occ, 2, mean)
    } else {
      if (!class(spatial_onject)[1] %in% c("SpatialPointsDataFrame", "SpatialPoints",
                                           "SpatialPolygonsDataFrame", "SpatialPolygons")) {
        stop("Argument 'spatial_onject' is not valid, see functions help.")
      } else {
        cent <- rgeos::gCentroid(spatial_onject, byid = FALSE)@coords
      }
    }
    LAEA <- sp::CRS(paste0("+proj=laea +lat_0=", cent[2], " +lon_0=", cent[1],
                           " +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
  } else {
    LAEA <- sp::CRS(paste("+proj=laea +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84",
                          "+datum=WGS84 +units=m +no_defs"))
  }
  return(LAEA)
}

#' @rdname LAEA_projection
AED_projection <- function(occurrences = NULL, spatial_onject = NULL) {
  if (!is.null(occurrences) | !is.null(spatial_onject)) {
    if (!is.null(occurrences)) {
      if (!class(occurrences)[1] %in% c("matrix", "data.frame", "tbl_df")) {
        stop("Argument 'occurrences' is not valid, see functions help.")
      }
      cent <- apply(occ, 2, mean)
    } else {
      if (!class(spatial_onject)[1] %in% c("SpatialPointsDataFrame", "SpatialPoints",
                                           "SpatialPolygonsDataFrame", "SpatialPolygons")) {
        stop("Argument 'spatial_onject' is not valid, see functions help.")
      } else {
        cent <- rgeos::gCentroid(spatial_onject, byid = FALSE)@coords
      }
    }
    AEQD <- sp::CRS(paste0("+proj=aeqd +lat_0=", cent[2], " +lon_0=", cent[1],
                          " +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
  } else {
    AEQD <- sp::CRS(paste("+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84",
                          "+datum=WGS84 +units=m +no_defs"))
  }
  return(AEQD)
}


#' Get SpatialPolygons of countries at distinct administrative levels
#' @param country_code (character) code of country or countries of interest.
#' @param boundary_level (numeric) level of admisnistrative division to be considered.
#' @param keep_data (logical) whether or not to keep downloaded files. Default = \code{FALSE}.
#' @return A SpatialPolygonsDataFrame from the GADM database at the level selected.
#' @export
#' @importFrom raster getData
#' @importFrom maps map
#' @importFrom maptools map2SpatialPolygons
#' @examples
#' map_gadm <- GADM_spoly(country_code = "EC", boundary_level = 1)

GADM_spoly <- function(country_code, boundary_level, keep_data = FALSE) {
  bounds <- list()
  for (i in 1:length(country_code)){
    bounds[[i]] <- raster::getData('GADM', country = country_code[i], level = boundary_level)
  }
  polygon <- do.call("rbind", bounds)
  polygon@data$GID_0 <- 1:length(polygon@data$GID_0)

  a_names <- ifelse(boundary_level == 0, "NAME_0",
                    paste("NAME", boundary_level, sep = "_"))

  polygon@data[, !names(polygon@data) %in% c("GID_0", a_names)] <- NULL # erase columns
  names(polygon@data) <- c("GID_0", "adm_names")

  if (keep_data == FALSE) {
    erase_rds <- list.files(path = ".", pattern = "^GADM_", full.names = TRUE)
    unlink(erase_rds)
  }
  return(polygon)
}


#' Extent of occurrence of a species based on convex hull polygons
#' @param occurrences a data.frame containing geographic coordinates of species
#' occurrences, columns must be: Species, Longitude, and Latitude. Geographic
#' coordinates must be in decimal degrees (WGS84).
#' @param polygons SpatialPolygon object to clip convex hulls to these limits.
#' Projection of this object defines projection of the extent of occurrence. This
#' projection must be one that allows safe calculation of areas (e.g., Lambert
#' Azimuthal Equal Area).
#' @return
#' A list comtaining a SpatialPolygonsDataFrame of the extent of occurrence, and
#' a vector with the areas in km2 of the spatial polygons resulted.
#' @export
#' @importFrom sp CRS SpatialPolygons Polygons Polygon proj4string over spTransform
#' @importFrom sp SpatialPolygonsDataFrame
#' @importFrom grDevices chull
#' @importFrom raster area
#' @importFrom rgeos gIntersection
#' @examples
#' # occurrences
#' data("occ_p", package = "rangemap")
#' occ <- unique(occ_p)
#'
#' # polygons
#' data("wrld_simpl", package = "maptools")
#' LAEA <- LAEA_projection(occ_sp)
#' poly_pr <- sp::spTransform(wrld_simpl, ECK4)
#'
#' # EOO
#' eoo_pe <- eoo(occurrences = occ, polygons = poly_pr)

eoo <- function(occurrences, polygons) {
  WGS84 <- sp::CRS("+init=epsg:4326")
  species <- as.character(occurrences[1, 1])
  coord <- as.data.frame(occurrences[, 2:3])
  covexhull <- chull(coord) # convex hull from points
  coord_pol <- coord[c(covexhull, covexhull[1]), ] # defining coordinates
  covexhull_polygon <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(coord_pol)), ID = 1)))
  sp::proj4string(covexhull_polygon) <- WGS84 # project
  covexhull_polygon_pr <- sp::spTransform(covexhull_polygon, polygons@proj4string) # reproject
  c_hull_extent <- rgeos::gIntersection(polygons, covexhull_polygon_pr,
                                        byid = TRUE, drop_lower_td = TRUE) # area of interest

  eockm2 <- raster::area(c_hull_extent) / 1000000
  eocckm2 <- sum(eockm2) # total area of the species range

  extent_occurrence <- sp::SpatialPolygonsDataFrame(c_hull_extent, # extent of occurrence
                                                    data = data.frame(species, eockm2),
                                                    match.ID = FALSE)

  return(list(spolydf = extent_occurrence, area = eocckm2))
}


#' Area of occupancy of a species as defined by the IUCN
#' @param occ_pr SpatialPointsDataFrame of ocurrence records. Projection must be
#' one that allows safe calculation of areas (e.g., Lambert Azimuthal Equal Area).
#' @param species (character) scientific name of the species.
#' @return
#' A list comtaining a SpatialPolygonsDataFrame of the area of occupancy, and
#' a vector with the areas in km2 of the spatial polygons resulted.
#' @export
#' @importFrom sp SpatialPolygonsDataFrame
#' @importFrom raster area extent rasterize
#' @examples
#' # data
#' data("occ_p", package = "rangemap")
#' occ <- unique(occ_p)
#' WGS84 <- sp::CRS("+init=epsg:4326")
#' occ_sp <- sp::SpatialPointsDataFrame(coords = occ[, 2:3], data = occ,
#'                                      proj4string = WGS84)
#'
#' LAEA <- LAEA_projection(occ_sp)
#' occ_pr <- sp::spTransform(occ_sp, LAEA)
#'
#' sp <- as.character(occ[1, 1])
#'
#' # AOO
#' aoo_pe <- aoo(occ_pr = occ_pr, species = sp)

aoo <- function(occ_pr, species) {
  grid <- raster::raster(ext = raster::extent(occ_pr) + 10000,
                         res = c(2000, 2000), crs = occ_pr@proj4string)
  raster_sp <- raster::rasterize(occ_pr[, 2:3], grid)[[1]] # raster from points
  grid_sp <- as(raster_sp, "SpatialPolygonsDataFrame") # raster to polygon

  aockm2 <- raster::area(grid_sp) / 1000000
  aocckm2 <- sum(aockm2) # area calculation

  area_occupancy <- sp::SpatialPolygonsDataFrame(grid_sp,  # area of occupancy
                                                 data = data.frame(species, aockm2),
                                                 match.ID = FALSE)
  return(list(spolydf = area_occupancy, area = aocckm2))
}


#' Finds clusters for SpatialPointsDataFrame based on distinct methods
#' @param occ_pr SpatialPointsDataFrame of ocurrence records. Projection must be
#' one that allows safe calculation of distances (e.g., Azimuthal equidistant)
#' @param cluster_method (character) name of the method to be used for clustering
#' the occurrences. Options are "hierarchical" and "k-means"; default = "hierarchical".
#' See details for more information on the two available methods.
#' @param split_distance (numeric) distance in meters that will limit connectivity
#' among hull polygons created with chunks of points separated by long distances.
#' This parameter is used when \code{cluster_method} = "hierarchical".
#' @param n_k_means (numeric) number of clusters in which the species occurrences
#' will be grouped when using the "k-means" \code{cluster_method}.
#' @return A SpatialPointsDataFrame with an extra column in data defining clusters.
#' @details
#' \code{cluster_method} must be chosen based on the spatial configuration of the
#' species occurrences. Both methods make distinct assumptions and one of them may
#' perform better than the other depending on the spatial pattern of the data.
#'
#' The k-means method, for example, perfomrs better when the following assumptions
#' are fulfilled: Clusters are spatially grouped—or “spherical” and Clusters are
#' of a similar size. Owing to the nature of the hierarchical clustering algorithm
#' it may take more time than the k-means method. Both methods make assumptions
#' and they may work well on some data sets, and fail on others.
#'
#' Another important factor to consider is that the k-means method allways starts
#' with a random choice of cluster centers, thus it may end in different results
#' on different runs. That may be problematic when trying to replicate your methods.
#' With hierarchical clustering, most likely the same clusters can be obtained if
#' the process is repeated.
#'
#' For more information on these clustering methods see Aggarwal and Reddy (2014)
#' \url{https://goo.gl/RQ2ebd}.
#'
#' @export
#' @importFrom sp coordinates
#' @importFrom stats hclust cutree kmeans
#' @examples
#' # data
#' data("occ_p", package = "rangemap")
#'
#' # preparing spatial points
#' occ <- as.data.frame(unique(occ_p))
#' WGS84 <- sp::CRS("+init=epsg:4326")
#' occ_sp <- sp::SpatialPointsDataFrame(coords = occ[, 2:3], data = occ,
#'                                      proj4string = WGS84)
#'
#' # reprojecting for measuring distances
#' LAEA <- LAEA_projection(occ_sp)
#' occ_pr <- sp::spTransform(occ_sp, LAEA)
#'
#' # clustering
#' occ_clus <- clusters(occ_pr, cluster_method = "k-means", n_k_means = 2)

clusters <- function(occ_pr, cluster_method = "hierarchical", split_distance,
                     n_k_means) {
  if (cluster_method == "hierarchical" & missing(split_distance)) {
    stop("Argument 'split_distance' must be defined when hierarchical cluster method is used.")
  }
  if (cluster_method == "k-means" & missing(n_k_means)) {
    stop("Argument 'n_k_means' must be defined when k-means cluster method is used.")
  }
  if (cluster_method == "hierarchical" | cluster_method == "k-means") {
    if (cluster_method == "hierarchical") {
      ## defining a hierarchical cluster method for the occurrences
      message("Clustering method: hierarchical")
      coor <- sp::coordinates(occ_pr)
      df <- data.frame(rownames = 1:nrow(coor), x = coor[, 1], y = coor[, 2])
      cluster_method <- hclust(dist(df), method = "complete")

      ## defining wich points are clustered based on the user-defined distance
      cluster_vector <- cutree(cluster_method, h = split_distance)
    }else {
      message("Clustering method: k-means")
      set.seed(1) # to get always the same answer with using the same data
      ## identifying clusters from occurrences
      cluster_method <- kmeans(as.matrix(sp::coordinates(occ_pr)), n_k_means)

      # vector for cluster separation
      cluster_vector <- cluster_method$cluster
    }
  } else {
    stop("'cluster_method' is not valid, options are: \nhierarchical or k-means")
  }
  ## Join results to occurrences
  occ_pr@data <- data.frame(occ_pr@data, clusters = cluster_vector)

  return(occ_pr)
}


#' Convex or concave hull polygons from spatial points
#' @param occ_pr SpatialPoints* object containing geographic points to be used
#' to create hull polygons. This spatial object must be projected to a system
#' with th argument "+units=m".
#' @param hull_type (character) type of hull polygons to be created. Available
#' options are: "convex" and "concave". Default = "convex".
#' @param concave_distance_lim (numeric) distance, in meters, to be passed to the
#' length_threshold parameter of the \code{\link[concaveman]{concaveman}} funcion.
#' Default = 5000. Ignored if \code{hull_type} is not "concave".
#' @return A SpatialPolygon object with the hull polygon. If the number of points
#' in occ_pr is 1 or 2 a SpatialPointsDataFrame object is returned.
#' @export
#' @importFrom sp CRS SpatialPointsDataFrame
#' @importFrom sp SpatialPolygons Polygons Polygon
#' @importFrom sf st_multipoint st_sf st_sfc as_Spatial st_zm
#' @importFrom concaveman concaveman
#' @examples
#' # data
#' data("occ_p", package = "rangemap")
#'
#' # preparing spatial points
#' occ <- as.data.frame(unique(occ_p))
#' WGS84 <- sp::CRS("+init=epsg:4326")
#' occ_sp <- sp::SpatialPointsDataFrame(coords = occ[, 2:3], data = occ,
#'                                      proj4string = WGS84)
#'
#' # reprojecting
#' LAEA <- LAEA_projection(occ_sp)
#' occ_pr <- sp::spTransform(occ_sp, LAEA)
#'
#' # convex hull polygon
#' cvx_hull <- hull_polygon(occ_pr, hull_type = "convex")

hull_polygon <- function(occ_pr, hull_type = "convex", concave_distance_lim = 5000) {
  if (hull_type == "convex" | hull_type == "concave") {
    condition <- !is.list(occ_pr)
    if (condition) {
      proje <- occ_pr@proj4string
      occ_pr <- list(occ_pr)
    } else {
      proje <- occ_pr[[1]]@proj4string
    }
    if (is.na(proje)) {
      stop("'occ_pr' must be projected.")
    }
    if (hull_type == "convex") {
      message("Hull type: convex")

      hulls <- lapply(1:length(occ_pr), function(i) {
        coord <- as.data.frame(sp::coordinates(occ_pr[[i]]))
        if (dim(coord)[1] > 2) {
          covexhull <- chull(coord) # convex hull from points
          coord_pol <- coord[c(covexhull, covexhull[1]),] # defining coordinates
          hull <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(coord_pol)),
                                                        ID = 1)), proj4string = proje)
          #sp::proj4string(hull) <- AEQD # project
        }else {
          hull <- sp::SpatialPointsDataFrame(coords = coord, data = coord,
                                             proj4string = proje)
        }
      })
    } else {
      message("Hull type: concave")
      hulls <- lapply(1:length(occ_pr), function(i) {
        coord <- as.data.frame(sp::coordinates(occ_pr[[i]]))
        if (dim(coord)[1] > 2) {
          sppoints <- sf::st_multipoint(as.matrix(coord))
          sf_points <- sf::st_sf(sf::st_sfc(sppoints, crs = proje@projargs))
          concavehull <- concaveman::concaveman(sf_points,
                                                length_threshold = concave_distance_lim)
          hull <- sf::as_Spatial(sf::st_zm(concavehull$polygons))
        } else {
          hull <- sp::SpatialPointsDataFrame(coords = coord, data = coord,
                                             proj4string = proje)
        }
      })
    }
  } else {
    stop(paste("'hull_type' is not valid, potential options are:\nconvex or concave"))
  }
  if (condition) {
    hulls <- hulls[[1]]
  }
  return(hulls)
}


#' Exclude small polygons from SpatialPolygons object
#' @param polygons SpatialPolygonsDataFrame object.
#' @param threshold_size (numeric) threshold value of area to determine whether
#' polygons are big or not. Areas must be according to projection of \code{polygons}.
#' @return A SpatialPolygonsDataFrame with polygons with areas above
#' \code{threshold_size}.
#' @export
#' @examples
#' data("spdf_range", package = "rangemap")
#' sp::plot(spdf_range)
#'
#' big_polys <- keep_big_polygons(polygons = spdf_range, threshold_size = 0.2)
#' sp::plot(big_polys)

keep_big_polygons <- function(polygons, threshold_size) {
  areas <- lapply(polygons@polygons, function(x){
    sapply(x@Polygons, function(y) {y@area})
  })
  bigpolys <- lapply(areas, function(x){which(x > threshold_size)})

  for (i in 1:length(bigpolys)) {
    polygons@polygons[[i]]@Polygons <- polygons@polygons[[i]]@Polygons[bigpolys[[i]]]
    porder <- polygons@polygons[[i]]@plotOrder
    polygons@polygons[[i]]@plotOrder <- porder[porder %in% bigpolys[[i]]]
  }
  slot(polygons, "polygons") <- lapply(slot(polygons, "polygons"),
                                       "comment<-", NULL)

  return(polygons)
}
