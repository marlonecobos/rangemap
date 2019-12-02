#' Species distributional ranges based on ecological niche models
#'
#' @description rangemap_enm generates a distributional range for a given species
#' using a continuous raster layer produced with an ecological niche modeling algorithm.
#' This function split the model in suitable and unsuitable areas using a user
#' specified level of omission or a given threshold value. An approach to the species
#' extent of occurrence (using convex hulls) and the area of occupancy according
#' to the IUCN criteria are also generated. Shapefiles can be saved in the working
#' directory if it is needed.
#'
#' @param occurrences a data.frame containing geographic coordinates of species occurrences,
#' columns must be: Species, Longitude, and Latitude. Geographic coordinates must be in decimal
#' degrees. \code{occurrences} may not exist but \code{threshold_value} must be defined.
#' @param model a RasterLayer object that will be binarized using the \code{threshold_value}
#' defined by the user or a value calculated based on an omission level (from 0 - 100) defined in
#' \code{threshold_omission}. If model is projected, this projection must be Geographic (longitude,
#' latitude). If not projected, the Geographic projection will be assigned for the analysis.
#' @param threshold_value (numeric) value used for reclasifying the model. This value will
#' be the lowest considered as suitable for the species and must be inside the range of values
#' present in \code{model}. If defined, \code{threshold_omission} will be ignored. If
#' \code{occurrences} is not defined, this parameter is mandatory.
#' @param threshold_omission (numeric) percentage of occurrence records to be excluded from
#' suitable areas considering their values of suitability in the continuous model (e.g., 0, 5,
#' or 10). Ignored if \code{threshold_value} is provided.
#' @param simplify (logical) if TRUE, polygons of suitable areas will be simplified at a tolerance
#' defined in \code{simplify_level}. Default = FALSE.
#' @param simplify_level (numeric) tolerance at the moment of simplifying polygons created from
#' the suitable areas derived from the ecological niche model. Lower values will produce polygons
#' more similar to the original geometry. Default = 0. If simplify is needed, try numbers between
#' 0 and 1 first. Ignored if \code{simplify} = FALSE.
#' @param polygons (optional) a SpatialPolygon object to adjust created polygons to these limits.
#' Projection must be Geographic (longitude, latitude). If not defined, a default, simple world
#' map will be used.
#' @param final_projection (character) string of projection arguments for resulting Spatial objects.
#' Arguments must be as in the PROJ.4 documentation. See funcion \code{\link[sp]{CRS}} for details.
#' Default = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" = WGS84.
#' @param save_shp (logical) if TRUE, shapefiles of the species range, occurrences, extent of
#' occurrence and area of occupancy will be written in the working directory. Default = FALSE.
#' @param name (character) valid if \code{save_shp} = TRUE. The name of the shapefile to be exported.
#' A suffix will be added to \code{name} depending on the object as follows: species extent of
#' occurrence = "_extent_occ", area of occupancy = "_area_occ", and occurrences = "_unique_records".
#' Default = "range_enm".
#'
#' @return
#' If \code{occurrences} and \code{threshold_omission} are defined, a  named list containing:
#' (1) a data.frame with information about the species range, and SpatialPolygon objects of
#' (2) unique occurrences, (3) species range, (4) extent of occurrence, and (5) area of occurpancy.
#'
#' If instead of the two parameters before mentioned, \code{threshold_value} is provided, the result
#' will be a list of two elements: (1) a data.frame with information about the species range, and
#' (2) a SpatialPolygon object of the species range.
#'
#' @details
#' All resultant Spatial objects in the list of results will be projected to the \code{final_projection}.
#' Areas are calculated in square kilometers using the Azimuthal equal area projection.
#'
#' @usage
#' rangemap_enm(occurrences, model, threshold_value, threshold_omission,
#'     simplify = FALSE, simplify_level = 0, polygons, final_projection,
#'     save_shp = FALSE, name = "range_enm")
#'
#' @export
#'
#' @importFrom raster values extract crs rasterToPolygons area raster extent rasterize
#' @importFrom rgeos gSimplify gCentroid gBuffer gUnaryUnion gIntersection
#' @importFrom sp CRS SpatialPointsDataFrame SpatialPolygonsDataFrame
#' @importFrom sp SpatialPolygons Polygons Polygon proj4string over spTransform
#' @importFrom maps map
#' @importFrom maptools map2SpatialPolygons
#' @importFrom rgdal writeOGR
#'
#' @examples
#' # parameters
#' sp_mod <- raster::raster(list.files(system.file("extdata", package = "rangemap"),
#'                                     pattern = "sp_model.tif", full.names = TRUE))
#' sp_train <- read.csv(list.files(system.file("extdata", package = "rangemap"),
#'                                 pattern = "sp_train.csv", full.names = TRUE))
#' occ_sp <- data.frame("A_americanum", sp_train)
#' thres <- 5
#' save <- TRUE
#' name <- "test"
#'
#' enm_range <- rangemap_enm(occurrences = occ_sp, model = sp_mod,  threshold_omission = thres,
#'                           save_shp = save, name = name)
#'
#' # see the species range in a figure
#' extent <- TRUE
#' occ <- TRUE
#' legend <- TRUE
#' north <- TRUE
#'
#' # creating the species range figure
#' rangemap_fig(enm_range, add_extent = extent, add_occurrences = occ,
#'              legend = legend, northarrow = north, northarrow_position = "topleft")

rangemap_enm <- function(occurrences, model, threshold_value, threshold_omission,
                         simplify = FALSE, simplify_level = 0, polygons,
                         final_projection, save_shp = FALSE, name = "range_enm") {

  # check for errors
  if (missing("model")) {
    stop("model must exist to perform the calculations.")
  }

  if (!missing("threshold_value") | !missing("threshold_omission") | !missing("occurrences")) {
    if (!missing("threshold_value")) {
      binary <- model
      raster::values(binary)[raster::values(binary) < threshold_value] <- 0
      raster::values(binary)[raster::values(binary) >= threshold_value] <- 1
    }else {
      if (!missing("threshold_omission") & !missing("occurrences")) {
        # keeping only coordinates
        occ <- occurrences[, 2:3]

        # threshold value calculation
        o_suit <- na.omit(raster::extract(model, occ))
        o_suit_sort <- sort(o_suit)
        thres <- o_suit_sort[ceiling(length(occ[, 1]) * threshold_omission / 100) + 1]

        # binarization
        binary <- model
        raster::values(binary)[raster::values(binary) < thres] <- 0
        raster::values(binary)[raster::values(binary) >= thres] <- 1
      }else {
        stop(paste("Parameters threshold_omission and occurrences, or threshold_value must be",
                   "\ndefined to perform the calculations."))
      }
    }
  }
  # keeping only areas of presence
  presence <- binary
  raster::values(presence)[raster::values(presence) == 0] <- NA

  # erase duplicate records
  if (!missing(occurrences)) {
    occ <- as.data.frame(unique(occurrences))[, 1:3]
    colnames(occ) <- c("Species", "Longitude", "Latitude")

  }

  # convert raster to polygons
  WGS84 <- sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  if (is.na(model@crs)) {
    raster::crs(presence) <- WGS84@projargs
  }
  enm_range <- raster::rasterToPolygons(presence, dissolve = TRUE)

  # erasing small polygons
  areas <- lapply(enm_range@polygons, function(x) sapply(x@Polygons, function(y) y@area))

  bigpolys <- unlist(lapply(areas, function(x) which(x > 0.1)))

  enm_range@polygons[[1]]@Polygons <- enm_range@polygons[[1]]@Polygons[bigpolys]
  enm_range@polygons[[1]]@plotOrder <- enm_range@polygons[[1]]@plotOrder[enm_range@polygons[[1]]@plotOrder %in% bigpolys]

  slot(enm_range, "polygons") <- lapply(slot(enm_range, "polygons"),
                                        "comment<-", NULL)

  if (simplify == TRUE) {
    enm_range <- suppressWarnings(rgeos::gSimplify(enm_range, tol = simplify_level)) # simplify polygons
  }

  # world map or user map for creating extent of occurrence
  if (missing(polygons)) {
    w_map <- maps::map(database = "world", fill = TRUE, plot = FALSE) # map of the world

    w_po <- sapply(strsplit(w_map$names, ":"), function(x) x[1]) # preparing data to create polygon
    polygons <- maptools::map2SpatialPolygons(w_map, IDs = w_po, proj4string = WGS84) # map to polygon
  }

  # project area and occurrences using the area centriod as reference
  centroid <- suppressWarnings(rgeos::gCentroid(enm_range, byid = FALSE))

  AEQD <- sp::CRS(paste("+proj=aeqd +lat_0=", centroid@coords[2], " +lon_0=", centroid@coords[1],
                        " +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs", sep = ""))

  if (!missing(occurrences)) {
    occ_sp <- sp::SpatialPointsDataFrame(coords = occ[, 2:3], data = occ,
                                         proj4string = WGS84)
    occ_pr <- sp::spTransform(occ_sp, AEQD)
  }

  enm_range_pr <- sp::spTransform(enm_range, AEQD)
  polygons <- sp::spTransform(polygons, AEQD)

  polygons <- suppressWarnings(rgeos::gBuffer(polygons, byid = TRUE, width = 0)) # to avoid topology problems
  polygons <- rgeos::gUnaryUnion(polygons)


  if (missing(occurrences)) {
    # calculate areas in km2
    area <- raster::area(enm_range_pr) / 1000000
    areakm2 <- sum(area) # total area of the species range

    # adding characteristics to spatial polygons
    clip_area <- sp::SpatialPolygonsDataFrame(enm_range_pr, # species range
                                              data = data.frame("Species", area),
                                              match.ID = FALSE)

    # reprojection
    if (missing(final_projection)) {
      final_projection <- WGS84
    } else {
      final_projection <- sp::CRS(final_projection) # character to projection
    }

    clip_area <- sp::spTransform(clip_area, final_projection)

    # exporting
    if (save_shp == TRUE) {
      cat("\nWriting shapefile in the working directory...\n")
      rgdal::writeOGR(clip_area, ".", name, driver = "ESRI Shapefile")
    }

    # return results
    sp_dat <- data.frame("Species", areakm2) # extent of occ = total area?
    colnames(sp_dat) <- c("Species", "Range_area")

    results <- list(sp_dat, clip_area)
    names(results) <- c("Summary", "Species_range")

  }else {
    # calculate areas in km2
    area <- raster::area(enm_range_pr) / 1000000
    areakm2 <- sum(area) # total area of the species range

    ## extent of occurrence
    coord <- as.data.frame(occ[, 2:3]) # spatial point dataframe to data frame keeping only coordinates
    covexhull <- chull(coord) # convex hull from points
    coord_pol <- coord[c(covexhull, covexhull[1]),] # defining coordinates
    covexhull_polygon <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(coord_pol)), ID = 1))) # into SpatialPolygons
    sp::proj4string(covexhull_polygon) <- WGS84 # project
    covexhull_polygon_pr <- sp::spTransform(covexhull_polygon, AEQD) # reproject
    c_hull_extent <- rgeos::gIntersection(polygons, covexhull_polygon_pr, byid = TRUE, drop_lower_td = TRUE) # area of interest

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
    clip_area <- sp::SpatialPolygonsDataFrame(enm_range_pr, # species range
                                              data = data.frame(species, area),
                                              match.ID = FALSE)

    extent_occurrence <- sp::SpatialPolygonsDataFrame(c_hull_extent, # extent of occurrence
                                                      data = data.frame(species, eockm2),
                                                      match.ID = FALSE)

    area_occupancy <- sp::SpatialPolygonsDataFrame(grid_sp, # area of occupancy
                                                   data = data.frame(species, aockm2),
                                                   match.ID = FALSE)

    # reprojection
    if (missing(final_projection)) {
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
      cat("Writing shapefiles in the working directory.")
      rgdal::writeOGR(clip_area, ".", name, driver = "ESRI Shapefile")
      rgdal::writeOGR(extent_occurrence, ".", paste(name, "extent_occ", sep = "_"), driver = "ESRI Shapefile")
      rgdal::writeOGR(area_occupancy, ".", paste(name, "area_occ", sep = "_"), driver = "ESRI Shapefile")
      rgdal::writeOGR(occ_pr, ".", paste(name, "unique_records", sep = "_"), driver = "ESRI Shapefile")
    }

    # return results (list or a different object?)
    sp_dat <- data.frame(occ[1, 1], dim(occ_pr)[1], areakm2, eocckm2, aocckm2) # extent of occ = total area?
    colnames(sp_dat) <- c("Species", "Unique_records", "Range_area", "Extent_of_occurrence", "Area_of_occupancy")

    results <- list(sp_dat, occ_pr, clip_area, extent_occurrence, area_occupancy)
    names(results) <- c("Summary", "Species_unique_records", "Species_range", "Extent_of_occurrence",
                        "Area_of_occupancy")
  }

  return(results)
}
