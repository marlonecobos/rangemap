#' Species distributional ranges based on distinct hull polygons
#'
#' @description rangemap_hull generates a species range polygon for a given species
#' by creating convex or concave hull polygons based on occurrence data.
#' An approach to the species extent of occurrence (using convex hulls) and the
#' area of occupancy according to the IUCN criteria are also generated. Shape files can be saved
#' in the working directory if it is needed.
#'
#' @param occurrences a data.frame containing species occurrences, columns must be:
#' Species, Longitude, and Latitude.
#' @param hull_type (character) type of hull polygons to be created. Available options are: "convex"
#' or "concave" hulls. Default = "convex".
#' @param concave_distance_lim (numeric) distance, in meters, to be passed to the length_threshold
#' parameter of the \code{\link[concaveman]{concaveman}} funcion. Default = 5000. Ignored if hull_type
#' is not "concave".
#' @param buffer_distance (numeric) distance, in meters, to be used for creating the buffer areas
#' around occurrences, default = 50000.
#' @param split (logical) if TRUE a distance (for hierarchical clustering) or a number (for K-means
#' clustering) is used to separate distinct chunks of occurrences. Recommended when the species of
#' interest has a disjunct distribution. Default = FALSE.
#' @param cluster_method (character) name of the method to be used for clustering the occurrences. Options
#' are "hierarchical" and "k-means"; default = "hierarchical". Note that this parameter is ignored when
#' split = FALSE.
#' @param split_distance (numeric) distance in meters that will limit connectivity among
#' hull polygons created with chunks of points separated by long distances. This parameter is used when
#' cluster method = "hierarchical".
#' @param n_k_means (numeric) number of clusters in which the species occurrences will be grouped when
#' using the "k-means" cluster_method.
#' @param polygons (optional) a SpatialPolygon object that will be clipped with the buffer areas
#' to create species ranges based on actual limits. Projection must be Geographic (longitude, latitude).
#' If not defined, a default, simple world map will be used.
#' @param save_shp (logical) if TRUE shapefiles of the species range, extent of occurrence and area of
#' occupancy will be written in the working directory.
#' @param name (character) valid if save_shp TRUE. The name of the shapefile to be exported.
#'
#' @return A named list containing a data.frame with information about the species range, a
#' SpatialPolygon object of the species range in Geographic projection, and the same SpatialPolygon
#' object projected to the Azimuthal equal area projection.
#'
#' @examples
#' if(!require(rgbif)){
#' install.packages("rgbif")
#' library(rgbif)
#' }
#'
#' # getting the data from GBIF
#' species <- name_lookup(query = "Dasypus kappleri",
#' rank="species", return = "data") # information about the species
#'
#' occ_count(taxonKey = species$key[14], georeferenced = TRUE) # testing if keys return records
#'
#' key <- species$key[14] # using species key that return information
#'
#' occ <- occ_search(taxonKey = key, return = "data", limit = 2000) # using the taxon key
#'
#' # keeping only georeferenced records
#' occ_g <- occ[!is.na(occ$decimalLatitude) & !is.na(occ$decimalLongitude),
#'              c("name", "decimalLongitude", "decimalLatitude")]
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
#'                             save_shp = save, name = name)

# Dependencies: sp (SpatialPointsDataFrame, spTransform),
#               raster (buffer, area), maps (map), maptools (map2SpatialPolygons),
#               rgeos (gIntersection, gCentroid),

rangemap_hull <- function(occurrences, hull_type = "convex", buffer_distance = 50000,
                          concave_distance_lim = 5000, split = FALSE, cluster_method = "hierarchical",
                          split_distance, n_k_means, polygons, save_shp = FALSE, name) {
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
  WGS84 <- sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  occ_sp <- sp::SpatialPointsDataFrame(coords = occ[, 2:3], data = occ,
                                       proj4string = WGS84)

  # world map or user map fro creating species range
  if (missing(polygons)) {
    w_map <- maps::map(database = "world", fill = TRUE, plot = FALSE) # map of the world

    w_po <- sapply(strsplit(w_map$names, ":"), function(x) x[1]) # preparing data to create polygon
    polygons <- maptools::map2SpatialPolygons(w_map, IDs = w_po, proj4string = WGS84) # map to polygon
  }

  # keeping only records in land
  occ_sp <- occ_sp[!is.na(sp::over(occ_sp, polygons)), ]

  # project the points using their centriods as reference
  centroid <- rgeos::gCentroid(occ_sp, byid = FALSE)

  AEQD <- sp::CRS(paste("+proj=aeqd +lat_0=", centroid@coords[2], " +lon_0=", centroid@coords[1],
                        " +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs", sep = ""))

  occ_pr <- sp::spTransform(occ_sp, AEQD)

  # project polygons
  polygons <- sp::spTransform(polygons, AEQD)

  # clustering
  if (split == TRUE) {
    if (cluster_method == "hierarchical" | cluster_method == "k-means") {
      #split groups of points based on the split distance

      if (cluster_method == "hierarchical") {
        ## defining a hierarchical cluster method for the occurrences
        cat("\nClustering method: hierarchical\n")

        cluster_method <- hclust(dist(data.frame(rownames = 1:length(occ_pr@data[,1]), x = sp::coordinates(occ_pr)[,1],
                                                 y = sp::coordinates(occ_pr)[,2])), method = "complete")

        ## defining wich points are clustered based on the user-defined distance
        cluster_vector <- cutree(cluster_method, h = split_distance)
      }else {
        cat("\nClustering method: k-means\n")

        set.seed(1) # to get always the same answer with using the same data
        ## identifying clusters from occurrences
        cluster_method <- kmeans(as.matrix(sp::coordinates(occ_pr)), n_k_means)

        # vector for cluster separation
        cluster_vector <- cluster_method$cluster
      }

    }else {
      stop("No valid clustering method has been defined, options are: \n \"hierarchical\" or \"k-means\"")
    }

    ## Join results to occurrences
    occ_pr@data <- data.frame(occ_pr@data, clusters = cluster_vector)
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
      cat("\nHull type: convex\n")

      hulls <- list()

      for (i in 1:length(occ_prs)) {
        if (length(occ_prs) > 1) {
          coord <- as.data.frame(sp::coordinates(occ_prs@coords[[i]])) # spatial point dataframe to data frame keeping only coordinates
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
      cat("\nHull type: concave\n")

      hulls <- list()

      for (i in 1:length(occ_prs)) {
        if (length(occ_prs) > 1) {
          coord <- as.data.frame(sp::coordinates(occ_prs@coords[[i]])) # spatial point dataframe to data frame keeping only coordinates
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
    stop(paste("hull_type is not a valid option, potential options are:",
               "\n\"convex\" or \"concave\""))
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

  # calculate areas in km2
  areakm2 <- raster::area(hulls_buff_un) / 1000000
  areackm2 <- sum(areakm2) # total area of the species range

  ## extent of occurrence
  coord <- as.data.frame(occ[, 2:3]) # spatial point dataframe to data frame keeping only coordinates
  covexhull <- chull(coord) # convex hull from points
  coord_pol <- coord[c(covexhull, covexhull[1]),] # defining coordinates
  covexhull_polygon <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(coord_pol)), ID = 1))) # into SpatialPolygons
  sp::proj4string(covexhull_polygon) <- WGS84 # project
  covexhull_polygon_pr <- sp::spTransform(covexhull_polygon, AEQD) # reproject
  c_hull_extent <- rgeos::gIntersection(covexhull_polygon_pr, polygons,
                                        byid = TRUE, drop_lower_td = TRUE) # area of interest

  eockm2 <- raster::area(c_hull_extent) / 1000000
  eocckm2 <- sum(eockm2) # total area of the species range

  ## area of occupancy
  grid <- raster::raster(ext = raster::extent(occ_pr) + 10000, res = c(2000, 2000), crs = AEQD)
  raster_sp <- raster::rasterize(occ_pr[, 2:3], grid)[[1]] # raster from points
  grid_sp <- as(raster_sp, "SpatialPolygonsDataFrame") # raster to polygon

  aockm2 <- raster::area(grid_sp) / 1000000
  aocckm2 <- sum(aockm2) # area calculation

  # adding characteristics to spatial polygons
  species <- as.character(occurrences[1, 1])
  clip_area <- sp::SpatialPolygonsDataFrame(hulls_buff_un, # species range
                                            data = data.frame(species, areakm2),
                                            match.ID = FALSE)

  extent_occurrence <- sp::SpatialPolygonsDataFrame(c_hull_extent, # extent of occurrence
                                                    data = data.frame(species, eockm2),
                                                    match.ID = FALSE)

  area_occupancy <- sp::SpatialPolygonsDataFrame(grid_sp, # area of occupancy
                                                 data = data.frame(species, aockm2),
                                                 match.ID = FALSE)

  # exporting
  if (save_shp == TRUE) {
    cat("Writing shapefiles in the working directory.")
    rgdal::writeOGR(clip_area, ".", name, driver = "ESRI Shapefile")
    rgdal::writeOGR(extent_occurrence, ".", paste(name, "extent_occ", sep = "_"), driver = "ESRI Shapefile")
    rgdal::writeOGR(area_occupancy, ".", paste(name, "area_occ", sep = "_"), driver = "ESRI Shapefile")
    rgdal::writeOGR(occ_pr, ".", paste(name, "unique_records", sep = "_"), driver = "ESRI Shapefile")
  }

  # return results (list or a different object?)
  sp_dat <- data.frame(occ[1, 1], dim(occ_pr)[1], areackm2, eocckm2, aocckm2) # extent of occ = total area?
  colnames(sp_dat) <- c("Species", "Unique_records", "Range_area", "Extent_of_occurrence", "Area_of_occupancy")

  results <- list(sp_dat, occ_pr, clip_area, extent_occurrence, area_occupancy)
  names(results) <- c("Summary", "Species_unique_records", "Species_range", "Extent_of_occurrence",
                      "Area_of_occupancy")
  return(results)
}


