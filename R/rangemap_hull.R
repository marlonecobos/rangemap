#' Species distributional ranges based on distinct hull polygons
#'
#' @description rangemap_hull generates a distributional range for a given species
#' by creating convex or concave hull polygons based on occurrence data.
#' An approach to the species extent of occurrence (using convex hulls) and the area
#' of occupancy according to the IUCN criteria are also generated. Shapefiles can be
#' saved in the working directory if it is needed.
#'
#' @param occurrences a data.frame containing geographic coordinates of species occurrences,
#' columns must be: Species, Longitude, and Latitude. Geographic coordinates must be in decimal
#' degrees.
#' @param hull_type (character) type of hull polygons to be created. Available options are:
#' "convex" or "concave" hulls. Default = "convex".
#' @param concave_distance_lim (numeric) distance, in meters, to be passed to the length_threshold
#' parameter of the \code{\link[concaveman]{concaveman}} funcion. Default = 5000. Ignored if
#' \code{hull_type} is not "concave".
#' @param buffer_distance (numeric) distance, in meters, to be used for creating the buffer
#' areas around occurrences, default = 50000.
#' @param split (logical) if TRUE, a distance (for hierarchical clustering) or a number (for
#' K-means clustering) is used to separate distinct chunks of occurrences. Recommended when
#' the species of interest has a disjunct distribution. Default = FALSE.
#' @param cluster_method (character) name of the method to be used for clustering the occurrences.
#' Options are "hierarchical" and "k-means"; default = "hierarchical". Note that this parameter is
#' ignored when \code{split} = FALSE. See details for more information on the two available methods.
#' @param split_distance (numeric) distance in meters that will limit connectivity among hull
#' polygons created with chunks of points separated by long distances. This parameter is used
#' when \code{cluster_method} = "hierarchical" and \code{split} = TRUE.
#' @param n_k_means (numeric) if \code{split} = TRUE, number of clusters in which the species
#' occurrences will be grouped when using the "k-means" \code{cluster_method}.
#' @param polygons (optional) a SpatialPolygon object to clip buffer areas and adjust the species
#' range and other polygons to these limits. Projection must be Geographic (EPSG:4326).
#' If NULL (the default), a simplified world map will be used.
#' @param final_projection (character) string of projection arguments for resulting Spatial objects.
#' Arguments must be as in the PROJ.4 documentation. See \code{\link[sp]{CRS-class}} for details.
#' Default = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" = WGS84.
#' @param save_shp (logical) if TRUE, shapefiles of the species range, occurrences, extent of
#' occurrence and area of occupancy will be written in the working directory. Default = FALSE.
#' @param name (character) valid if \code{save_shp} = TRUE. The name of the shapefile to be
#' exported. A suffix will be added to \code{name} depending on the object as follows: species
#' extent of occurrence = "_extent_occ", area of occupancy = "_area_occ", and occurrences =
#' "_unique_records".
#'
#' @return
#' A named list containing: (1) a data.frame with information about the species range,
#' and SpatialPolygon objects of (2) unique occurrences, (3) species range, (4) extent of
#' occurrence, and (5) area of occurpancy.
#'
#' @details
#' All resultant Spatial objects in the list of results will be projected to the \code{final_projection}.
#' Areas are calculated in square kilometers using the Azimuthal equal area projection.
#'
#' The \code{cluster_method} must be chosen based on the spatial configuration of the
#' species occurrences. Both methods make distinct assumptions and one of them may perform better
#' than the other depending on the spatial pattern of the data.
#'
#' The k-means method, for example, perfomrs better when the following assumptions are fulfilled:
#' Clusters are spatially grouped—or “spherical” and Clusters are of a similar size. Owing to the
#' nature of the hierarchical clustering algorithm it may take more time than the k-means method.
#' Both methods make assumptions and they may work well on some data sets, and fail on others.
#'
#' Another important factor to consider is that the k-means method allways starts with a random
#' choice of cluster centers, thus it may end in different results on different runs. That may be
#' problematic when trying to replicate your methods. With hierarchical clustering, most likely
#' the same clusters can be obtained if the process is repeated.
#'
#' For more information on these clustering methods see Aggarwal and Reddy (2014)
#' \url{https://goo.gl/RQ2ebd}.
#'
#' @usage
#' rangemap_hull(occurrences, hull_type = "convex", buffer_distance = 50000,
#'     concave_distance_lim = 5000, split = FALSE, cluster_method = "hierarchical",
#'     split_distance, n_k_means, polygons, final_projection, save_shp = FALSE,
#'     name = "range_hull", overwrite = FALSE)
#'
#' @export
#'
#' @importFrom sp CRS SpatialPointsDataFrame SpatialPolygonsDataFrame
#' @importFrom sp SpatialPolygons Polygons Polygon proj4string over spTransform
#' @importFrom sp rbind.SpatialPolygons
#' @importFrom raster disaggregate area extent rasterize
#' @importFrom rgeos gCentroid gUnaryUnion gIntersection gBuffer
#' @importFrom maps map
#' @importFrom maptools map2SpatialPolygons
#' @importFrom rgdal writeOGR
#' @importFrom sf st_multipoint st_sf st_sfc as_Spatial st_zm
#' @importFrom concaveman concaveman
#'
#' @examples
#' suppressWarnings({if(!require(spocc)){
#'   install.packages("spocc")
#'   library(spocc)
#' }})
#'
#' # getting the data from GBIF
#' occs <- occ(query = "Dasypus kappleri", from = "gbif",
#'             limit = 1000)$gbif$data[[1]]
#'
#' # keeping only georeferenced records
#' occ_g <- occs[!is.na(occs$latitude) & !is.na(occs$longitude),
#'               c("name", "longitude", "latitude")]
#'
#' dist <- 100000
#' hull <- "convex" # try also "concave"
#' split <- TRUE
#' c_method <- "hierarchical"
#' split_d <- 1500000
#' save <- TRUE
#' name <- "test"
#'
#' hull_range <- rangemap_hull(occurrences = occ_g, hull_type = hull, buffer_distance = dist,
#'                             split = split, cluster_method = c_method, split_distance = split_d,
#'                             save_shp = save, name = name, overwrite = TRUE)
#'
#' # see the species range in a figure
#' extent <- TRUE
#' occ <- TRUE
#' legend <- TRUE
#' north <- TRUE
#'
#' # creating the species range figure
#' rangemap_fig(hull_range, add_extent = extent, add_occurrences = occ,
#'              legend = legend, northarrow = north)

rangemap_hull <- function(occurrences, hull_type = "convex", buffer_distance = 50000,
                          concave_distance_lim = 5000, split = FALSE,
                          cluster_method = "hierarchical", split_distance, n_k_means,
                          polygons = NULL, final_projection = NULL, save_shp = FALSE,
                          name, overwrite = FALSE) {
  # testing potential issues
  if (missing(occurrences)) {
    stop("Argument occurrences is necessary to perform the analysis")
  }

  if (dim(occurrences)[2] != 3) {
    stop("occurrences data.frame must have the following columns: \nSpecies, Longitude, and Latitude")
  }
  if (split == TRUE) {
    if (cluster_method == "hierarchical" & missing(split_distance)) {
      stop("Argument 'split_distance' must be defined when hierarchical cluster method is used.")
    }
    if (cluster_method == "k-means" & missing(n_k_means)) {
      stop("Argument 'n_k_means' must be defined when k-means cluster method is used.")
    }
  }

  # erase duplicate records
  occ <- as.data.frame(unique(occurrences))[, 1:3]
  colnames(occ) <- c("Species", "Longitude", "Latitude")

  # make a spatial object from coordinates
  WGS84 <- sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  occ_sp <- sp::SpatialPointsDataFrame(coords = occ[, 2:3], data = occ,
                                       proj4string = WGS84)

  # world map or user map fro creating species range
  if (is.null(polygons)) {
    polygons <- simple_wmap()
  }

  # keeping only records in land
  occ_sp <- occ_sp[polygons, ]

  # project the points
  ECK4 <- sp::CRS("+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  occ_pr <- sp::spTransform(occ_sp, ECK4)

  # project polygons
  polygons <- sp::spTransform(polygons, ECK4)

  # clustering
  if (split == TRUE) {
    occ_pr <- clusters(occ_pr, cluster_method, split_distance, n_k_means)
  }else {
    occ_pr@data <- data.frame(occ_pr@data, clusters = 1)
  }

  ## splitting points
  if (sum(unique(occ_pr@data$clusters)) > 1) {
    occ_prs <- split(occ_pr, occ_pr@data$clusters)
  }else {
    occ_prs <- list(occ_pr)
  }

  # create hulls depending in the user-defined argument (convex, concave, and alpha)
  # and per each group of points if they were clustered
  if (hull_type == "convex" | hull_type == "concave") {
    if (hull_type == "convex") {
      message("Hull type: convex")

      hulls <- list()

      for (i in 1:length(occ_prs)) {
        if (length(occ_prs) > 1) {
          coord <- as.data.frame(occ_prs@coords[[i]]) # spatial point dataframe to data frame keeping only coordinates
        }else {
          coord <- as.data.frame(sp::coordinates(occ_prs[[i]])) # spatial point dataframe to data frame keeping only coordinates
        }

        if (dim(coord)[1] > 2) {
          covexhull <- chull(coord) # convex hull from points
          coord_pol <- coord[c(covexhull, covexhull[1]),] # defining coordinates
          hulls[[i]] <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(coord_pol)), ID = 1))) # into SpatialPolygons
          sp::proj4string(hulls[[i]]) <- AEQD # project
        }else {
          hulls[[i]] <- sp::SpatialPointsDataFrame(coords = coord, data = coord,
                                                   proj4string = AEQD)
        }
      }
    }
    if (hull_type == "concave") {
      message("Hull type: concave")

      hulls <- list()

      for (i in 1:length(occ_prs)) {
        if (length(occ_prs) > 1) {
          coord <- as.data.frame(occ_prs@coords[[i]]) # spatial point dataframe to data frame keeping only coordinates
        }else {
          coord <- as.data.frame(sp::coordinates(occ_prs[[i]])) # spatial point dataframe to data frame keeping only coordinates
        }

        if (dim(coord)[1] > 2) {
          sppoints <- sf::st_multipoint(as.matrix(coord)) #sf_multipoints from multypoints
          sf_points <- sf::st_sf(sf::st_sfc(sppoints, crs = AEQD@projargs))
          concavehull <- concaveman::concaveman(sf_points, length_threshold = concave_distance_lim) # concave hull from points
          hulls[[i]] <- sf::as_Spatial(sf::st_zm(concavehull$polygons)) # into SpatialPolygons
        }else {
          hulls[[i]] <- sp::SpatialPointsDataFrame(coords = coord, data = coord,
                                                   proj4string = AEQD)
        }
      }
    }
  }else {
    stop(paste("'hull_type' is not valid, potential options are:\nconvex or concave"))
  }

  # create a buffer based on a user-defined distance
  hulls_buffer <- list()

  for (i in 1:length(hulls)) {
    hulls_buffer[[i]] <- rgeos::gBuffer(hulls[[i]], width = buffer_distance)
  }

  # union buffered polygons
  hulls_buff_un <- do.call(sp::rbind.SpatialPolygons, c(hulls_buffer, list(makeUniqueIDs = TRUE)))

  # Clipping with the world
  polygons <- suppressWarnings(rgeos::gBuffer(polygons, byid = TRUE, width = 0)) # to avoid topology problems
  polygons <- rgeos::gUnaryUnion(polygons)

  hulls_buff_un <- suppressWarnings(rgeos::gIntersection(hulls_buff_un, polygons,
                                                         byid = TRUE, drop_lower_td = TRUE)) # area of interest

  hulls_buff_un <- raster::disaggregate(hulls_buff_un)

  # calculate areas in km2
  areakm2 <- raster::area(hulls_buff_un) / 1000000
  areackm2 <- sum(areakm2) # total area of the species range

  # adding characteristics to spatial polygons
  species <- as.character(occurrences[1, 1])
  clip_area <- sp::SpatialPolygonsDataFrame(hulls_buff_un, # species range
                                            data = data.frame(species, areakm2),
                                            match.ID = FALSE)

  ## extent of occurrence
  eooc <- eoo(occ_sp@data, polygons)
  eocckm2 <- eooc$area
  extent_occurrence <- eooc$spolydf

  ## area of occupancy
  aooc <- aoo(occ_pr, species)
  aocckm2 <- aooc$area
  area_occupancy <- aooc$spolydf

  # reprojection
  if (is.null(final_projection)) {
    final_projection <- WGS84
  } else {
    final_projection <- sp::CRS(final_projection) # character to projection
  }

  clip_area <- sp::spTransform(clip_area, final_projection)
  extent_occurrence <- sp::spTransform(extent_occurrence, final_projection)
  area_occupancy <- sp::spTransform(area_occupancy, final_projection)
  occ_pr <- sp::spTransform(occ_pr, final_projection)

  # exporting
  if (save_shp == TRUE) {
    message("Writing shapefiles in the working directory.")
    rgdal::writeOGR(clip_area, ".", name, driver = "ESRI Shapefile",
                    overwrite_layer = overwrite)
    rgdal::writeOGR(extent_occurrence, ".", paste(name, "extent_occ", sep = "_"),
                    driver = "ESRI Shapefile", overwrite_layer = overwrite)
    rgdal::writeOGR(area_occupancy, ".", paste(name, "area_occ", sep = "_"),
                    driver = "ESRI Shapefile", overwrite_layer = overwrite)
    rgdal::writeOGR(occ_pr, ".", paste(name, "unique_records", sep = "_"),
                    driver = "ESRI Shapefile", overwrite_layer = overwrite)
  }

  # return results (list or a different object?)
  sp_dat <- data.frame(Species = species, Unique_records = dim(occ_pr)[1],
                       Range_area = areackm2, Extent_of_occurrence = eocckm2,
                       Area_of_occupancy = aocckm2)

  results <- list(Summary = sp_dat, Species_unique_records = occ_pr,
                  Species_range = clip_area, Extent_of_occurrence = extent_occurrence,
                  Area_of_occupancy = area_occupancy)
  return(results)
}
