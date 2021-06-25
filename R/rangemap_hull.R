#' Species distributional ranges based on convex or concave hull polygons
#'
#' @description rangemap_hull generates a distributional range for a given species
#' by creating convex or concave hull polygons based on occurrence data.
#' Optionally, representations of the species extent of occurrence (using convex
#' hulls) and the area of occupancy according to the IUCN criteria can also be
#' generated. Shapefiles can be saved in the working directory if it is needed.
#'
#' @param occurrences a data.frame containing geographic coordinates of species
#' occurrences, columns must be: Species, Longitude, and Latitude. Geographic
#' coordinates must be in decimal degrees  (WGS84).
#' @param hull_type (character) type of hull polygons to be created. Available
#' options are: "convex" and "concave". Default = "convex".
#' @param concave_distance_lim (numeric) distance, in meters, to be passed to the
#' length_threshold parameter of the \code{\link[concaveman]{concaveman}} function.
#' Default = 5000. Ignored if \code{hull_type} is not "concave".
#' @param buffer_distance (numeric) distance, in meters, to be used for creating
#' a buffer around resulting hull polygons; default = 50000.
#' @param split (logical) if \code{TRUE}, a distance (for hierarchical clustering)
#' or a number (for K-means clustering) is used to separate distinct chunks of
#' occurrences. Recommended when the species of interest has a disjunct
#' distribution. Default = \code{FALSE}.
#' @param cluster_method (character) name of the method to be used for clustering
#' the occurrences. Options are "hierarchical" and "k-means"; default =
#' "hierarchical". Note that this parameter is ignored when \code{split} =
#' \code{FALSE}. See details for more information on the two available methods.
#' @param split_distance (numeric) distance in meters that will limit connectivity
#' among hull polygons created with chunks of points separated by long distances.
#' This parameter is used when \code{cluster_method} = "hierarchical" and
#' \code{split} = \code{TRUE}. Default = \code{NULL}.
#' @param n_k_means (numeric) if \code{split} = \code{TRUE}, number of clusters
#' in which the species occurrences will be grouped when using the "k-means"
#' \code{cluster_method}. Default = \code{NULL}.
#' @param polygons (optional) a SpatialPolygons* object to clip polygons and
#' adjust the species range and other polygons to these limits. Projection must
#' be WGS84 (EPSG:4326). If \code{NULL}, the default, a simplified world map will
#' be used.
#' @param extent_of_occurrence (logical) whether to obtain the extent of occurrence
#' of the species based on a simple convex hull polygon; default = \code{TRUE}.
#' @param area_of_occupancy (logical) whether to obtain the area of occupancy
#' of the species based on a simple grid of 4 km^2 resolution;
#' default = \code{TRUE}.
#' @param final_projection (character) string of projection arguments for resulting
#' Spatial objects. Arguments must be as in the PROJ.4 documentation. See
#' \code{\link[sp]{CRS-class}} for details. If \code{NULL}, the default, projection
#' used is WGS84 (EPSG:4326).
#' @param save_shp (logical) if \code{TRUE}, shapefiles of the species range,
#' occurrences, extent of occurrence, and area of occupancy will be written in
#' the working directory. Default = \code{FALSE}.
#' @param name (character) valid if \code{save_shp} = \code{TRUE}. The name of
#' the shapefile to be exported. A suffix will be added to \code{name} depending
#' on the object, as follows: species extent of occurrence = "_extent_occ", area
#' of occupancy = "_area_occ", and occurrences = "_unique_records".
#' @param overwrite (logical) whether or not to overwrite previous results with
#' the same name. Default = \code{FALSE}.
#' @param verbose (logical) whether or not to print messages about the process.
#' Default = TRUE.
#'
#' @return
#' A sp_range object (S4) containing: (1) a data.frame with information about the
#' species range, and Spatial objects of (2) unique occurrences, (3) species range,
#' (4) extent of occurrence, and (5) area of occupancy.
#'
#' If \code{extent_of_occurrence} and/or \code{area_of_occupancy} = \code{FALSE},
#' the corresponding spatial objects in the resulting sp_range object will be
#' empty, an areas will have a value of 0.
#'
#' @details
#' All resulting Spatial objects in the results will be projected to the
#' \code{final_projection}. Areas are calculated in square kilometers using the
#' Lambert Azimuthal Equal Area projection, centered on the centroid of occurrence
#' points given as inputs.
#'
#' If \code{split} = \code{TRUE}, some point clusters may end up having less than
#' three points, in which cases creating hull polygons is not possible. Such
#' point clusters will be discarded if \code{buffer_distance} = 0. To keep them
#' as part of the final result, use a \code{buffer_distance} > 0.
#'
#' The \code{cluster_method} must be chosen based on the spatial configuration of
#' the species occurrences. Both methods make distinct assumptions and one of
#' them may perform better than the other depending on the spatial pattern of the
#' data.
#'
#' The k-means method, for example, performs better when the following assumptions
#' are fulfilled: Clusters are spatially grouped—or “spherical” and Clusters are
#' of a similar size. Owing to the nature of the hierarchical clustering algorithm
#' it may take more time than the k-means method. Both methods make assumptions
#' and they may work well on some data sets, and fail on others.
#'
#' Another important factor to consider is that the k-means method always starts
#' with a random choice of cluster centers, thus it may end in different results
#' on different runs. That may be problematic when trying to replicate your
#' methods. With hierarchical clustering, most likely the same clusters can be
#' obtained if the process is repeated.
#'
#' For more information on these clustering methods see Aggarwal and Reddy (2014),
#' \href{https://goo.gl/RQ2ebd}{here}.
#'
#' @usage
#' rangemap_hull(occurrences, hull_type = "convex", concave_distance_lim = 5000,
#'               buffer_distance = 50000, split = FALSE,
#'               cluster_method = "hierarchical", split_distance = NULL,
#'               n_k_means = NULL, polygons = NULL, extent_of_occurrence = TRUE,
#'               area_of_occupancy = TRUE, final_projection = NULL,
#'               save_shp = FALSE, name, overwrite = FALSE, verbose = TRUE)
#'
#' @export
#'
#' @importFrom sp CRS SpatialPointsDataFrame SpatialPolygonsDataFrame
#' @importFrom sp spTransform rbind.SpatialPolygons
#' @importFrom raster disaggregate area
#' @importFrom rgeos gUnaryUnion gIntersection gBuffer
#' @importFrom rgdal writeOGR
#'
#' @examples
#' \donttest{
#' # getting the data
#' data("occ_d", package = "rangemap")
#'
#' # other info for running
#' dist <- 100000
#' hull <- "convex" # try also "concave"
#'
#' hull_range <- rangemap_hull(occurrences = occ_d, hull_type = hull,
#'                             buffer_distance = dist)
#'
#' summary(hull_range)
#' }

rangemap_hull <- function(occurrences, hull_type = "convex", concave_distance_lim = 5000,
                          buffer_distance = 50000, split = FALSE,
                          cluster_method = "hierarchical", split_distance = NULL,
                          n_k_means = NULL, polygons = NULL,
                          extent_of_occurrence = TRUE, area_of_occupancy = TRUE,
                          final_projection = NULL, save_shp = FALSE,
                          name, overwrite = FALSE, verbose = TRUE) {
  # testing potential issues
  if (missing(occurrences)) {
    stop("Argument 'occurrences' is necessary to perform the analysis")
  }

  if (dim(occurrences)[2] != 3) {
    stop("'occurrences' data.frame must have the following columns: \nSpecies, Longitude, Latitude")
  }
  if (split == TRUE) {
    if (cluster_method == "hierarchical" & is.null(split_distance)) {
      stop("Argument 'split_distance' must be defined when hierarchical cluster method is used.")
    }
    if (cluster_method == "k-means" & is.null(n_k_means)) {
      stop("Argument 'n_k_means' must be defined when k-means cluster method is used.")
    }
  }
  if (save_shp == TRUE) {
    if (missing(name)) {
      stop("Argument 'name' must be defined if 'save_shp' = TRUE.")
    }
    if (file.exists(paste0(name, ".shp")) & overwrite == FALSE) {
      stop("Files already exist, use 'overwrite' = TRUE.")
    }
  }

  # initial projection
  WGS84 <- sp::CRS("+init=epsg:4326")

  # final projection
  if (is.null(final_projection)) {
    final_projection <- WGS84
  } else {
    final_projection <- sp::CRS(final_projection) # character to projection
  }

  # erase duplicate records
  occ <- as.data.frame(unique(occurrences))[, 1:3]
  colnames(occ) <- c("Species", "Longitude", "Latitude")

  # make a spatial object from coordinates
  occ_sp <- sp::SpatialPointsDataFrame(coords = occ[, 2:3], data = occ,
                                       proj4string = WGS84)

  # world map or user map fro creating species range
  if (is.null(polygons)) {
    poly <- simple_wmap(which = "simple")
  } else {
    poly <- polygons
  }

  # keeping only records in land
  occ_sp <- occ_sp[poly, ]

  # project the points
  AEQD <- AED_projection(spatial_object = occ_sp)
  occ_pr <- sp::spTransform(occ_sp, AEQD)

  # project polygons
  LAEA <- LAEA_projection(spatial_object = occ_sp)
  polygons <- sp::spTransform(poly, LAEA)

  # clustering
  if (split == TRUE) {
    occ_pr <- clusters(occ_pr, cluster_method, split_distance, n_k_means)
  }else {
    occ_pr@data <- data.frame(occ_pr@data, clusters = 1)
  }

  ## splitting points
  occ_pr <- sp::spTransform(occ_pr, LAEA)
  clust <- sort(unique(occ_pr@data$clusters))
  if (length(clust) > 1) {
    occ_prs <- lapply(clust, function(x) {occ_pr[occ_pr@data$clusters == x, ]})
  }else {
    occ_prs <- list(occ_pr)
  }

  # create hulls depending in the user-defined argument (convex, concave, and alpha)
  # and per each group of points if they were clustered
  hulls <- hull_polygon(occ_prs, hull_type, concave_distance_lim)

  # create a buffer based on a user-defined distance
  if (buffer_distance > 0) {
    hulls_buffer <- lapply(hulls, rgeos::gBuffer, width = buffer_distance)

    # union buffered polygons
    hulls_buff_un <- do.call(sp::rbind.SpatialPolygons,
                             c(hulls_buffer, list(makeUniqueIDs = TRUE)))
  } else {
    classes <- vapply(hulls, FUN.VALUE = character(1), function(x) {class(x)[1]})

    pols <- classes == "SpatialPolygons"
    if (sum(pols) != length(hulls)) {
      message("Certain clusters don't have enough points to create hull polygons\n",
              "Some point clusters will be discarded. If needed use a value > 0\n",
              "for 'buffer_distance' to create polygons from such points and keep them.")
    }

    hulls <- hulls[pols]

    hulls_buff_un <- do.call(sp::rbind.SpatialPolygons,
                             c(hulls, list(makeUniqueIDs = TRUE)))
  }

  # Clipping with the world
  polygons <- suppressWarnings(rgeos::gBuffer(polygons, byid = TRUE, width = 0))
  polygons <- rgeos::gUnaryUnion(polygons)

  hulls_buff_un <- rgeos::gIntersection(hulls_buff_un, polygons, byid = TRUE,
                                        drop_lower_td = TRUE) # area of interest
  hulls_buff_un <- raster::disaggregate(hulls_buff_un)

  # calculate areas in km2
  areakm2 <- raster::area(hulls_buff_un) / 1000000
  areackm2 <- sum(areakm2) # total area of the species range

  # adding characteristics to spatial polygons
  species <- as.character(occurrences[1, 1])
  clip_area <- sp::SpatialPolygonsDataFrame(hulls_buff_un, # species range
                                            data = data.frame(species, areakm2),
                                            match.ID = FALSE)

  # extent of occurrence
  if (extent_of_occurrence == TRUE) {
    eooc <- eoo(occ_sp@data, poly)
    eocckm2 <- eooc$area
    extent_occurrence <- eooc$spolydf
    extent_occurrence <- sp::spTransform(extent_occurrence, final_projection)
  } else {
    eocckm2 <- 0
    extent_occurrence <- new("SpatialPolygonsDataFrame")
  }

  # area of occupancy
  if (area_of_occupancy == TRUE) {
    aooc <- aoo(occ_pr, species)
    aocckm2 <- aooc$area
    area_occupancy <- aooc$spolydf
    area_occupancy <- sp::spTransform(area_occupancy, final_projection)
  } else {
    aocckm2 <- 0
    area_occupancy <- new("SpatialPolygonsDataFrame")
  }

  # reprojection
  clip_area <- sp::spTransform(clip_area, final_projection)
  occ_pr <- sp::spTransform(occ_pr, final_projection)

  # exporting
  if (save_shp == TRUE) {
    if (verbose == TRUE) {
      message("Writing shapefiles in the working directory.")
    }
    rgdal::writeOGR(clip_area, ".", name, driver = "ESRI Shapefile",
                    overwrite_layer = overwrite)
    rgdal::writeOGR(occ_pr, ".", paste(name, "unique_records", sep = "_"),
                    driver = "ESRI Shapefile", overwrite_layer = overwrite)
    if (extent_of_occurrence == TRUE) {
      rgdal::writeOGR(extent_occurrence, ".", paste(name, "extent_occ", sep = "_"),
                      driver = "ESRI Shapefile", overwrite_layer = overwrite)
    }
    if (area_of_occupancy == TRUE) {
      rgdal::writeOGR(area_occupancy, ".", paste(name, "area_occ", sep = "_"),
                      driver = "ESRI Shapefile", overwrite_layer = overwrite)
    }
  }

  # return results
  sp_dat <- data.frame(Species = species, Unique_records = dim(occ_pr)[1],
                       Range_area = areackm2, Extent_of_occurrence = eocckm2,
                       Area_of_occupancy = aocckm2)

  rmname <- paste0(gsub("^c", "C", hull_type),
                   ifelse(split, "_hull_split", "_hull"))

  results <- sp_range_iucn(name = rmname, summary = sp_dat,
                           species_unique_records = occ_pr,
                           species_range = clip_area,
                           extent_of_occurrence = extent_occurrence,
                           area_of_occupancy = area_occupancy)

  return(results)
}
