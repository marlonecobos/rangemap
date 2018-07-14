#' Species distributional ranges based on political boundaries
#'
#' @description rangemap_bound generates a species range polygon for a given species
#' by considering all the polygons of political entities in which the species has
#' been detected. An approach to the species extent of occurrence (using convex hulls) and the
#' area of occupancy according to the IUCN criteria are also generated. Shape files can be saved
#' in the working directory if it is needed.
#'
#' @param occurrences a data.frame containing species occurrences, columns must be:
#' Species, Longitude, and Latitude. If not defined, adm_areas must be defined and areas specified
#' in there will be used as part of the species range.
#' @param adm_areas (character) a vector of names of administrative areas known to be occupied by
#' the species, names depend on the boundary_level selected. Check \code{\link{adm_area_names}} data
#' documentation for an idea of how to define names in this parameter. If not defined, occurrences
#' must exist.
#' @param country_code (character) vector of country codes that will be considered when
#' creating the species range. If country selection is required to be only based on occurrences,
#' including neighbor countries is not necessary. Use \code{\link{rangemap_explore}} for a preview
#' of potential countries involved in the analysis. Codes follow the ISO-3166-1 norm as in function
#' \code{\link[raster]{getData}}. If not defined, polygons must be included. Ignored if polygons
#' are included.
#' @param boundary_level (numeric) the level of the administrative boundaries from 0 to 5 (0 is
#' the country and higher values equal finer divisions), default = 0. Ignored if polygons are
#' included.
#' @param polygons (optional) a SpatialPolygonDataFrame object that will be used instead of boundaries
#' especified in country_code to create species ranges based on overlapping of species records
#' with these layer as well as names defined in adm_areas. Projection must be Geographic (longitude,
#' latitude). If adm_areas is defined, polygons must have, as part of data, a fied (column) named
#' "adm_areas" for selecting extra areas based on the names of defined administrative areas.
#' If defined, arguments country_code and boundary level will be ignored.
#' @param kept_data (logical) if TRUE and polygons is not defined, data dowloaded from the GADM data
#' base will be kept in the working directory. Useful if all or the part of the downloaded polygons
#' for the creation of an species range area will be used in posterior analyses because those polygons
#' will not be downloaded again and time will be saved. Default = FALSE.
#' @param dissolve (logical) if TRUE distint polygons selected as part of the species range will
#' be disolved for creating simpler polygons, default = FALSE. The dissolving process may take some
#' more time, especially if species with broad distributions are being analyzed.
#' @param save_shp (logical) if TRUE shapefiles of the species range, extent of occurrence and area of
#' occupancy will be written in the working directory, default = FALSE.
#' @param name (character) valid if save_shp TRUE. The name of the shapefile to be exported.
#'
#' @return A named list containing a data.frame with information about the species range, a
#' SpatialPolygon object of the species range in Geographic projection, and the same SpatialPolygon
#' object projected to the Azimuthal equal area projection.
#'
#' @details Boundaries for countries defined in country_code are loaded using the
#' \code{\link[raster]{getData}} funcion.
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
#'             c("name", "decimalLongitude", "decimalLatitude")]
#'
#' # checking which countries may be involved in the analysis
#' rangemap_explore(occurrences = occ_g)
#'
#' level <- 0
#' adm <- "Ecuador" # Athough no record is on this country, we know it is in Ecuador
#' dissolve <- FALSE
#' save <- TRUE
#' name <- "test"
#' countries <- c("PER", "BRA", "COL", "VEN", "ECU", "GUF", "GUY", "SUR", "BOL")
#'
#' bound_range <- rangemap_bound(occurrences = occ_g, adm_areas = adm, country_code = countries,
#'                               boundary_level = level, dissolve = dissolve, save_shp = save, name = name)

# Dependencies: maps (map),
#               maptools (map2SpatialPolygons),
#               raster (area, rasterize, extent, getData),
#               rgdal (writeOGR),
#               rgeos (gIntersection, gCentroid, gBuffer, gUnaryUnion),
#               sp (SpatialPointsDataFrame, spTransform, SpatialPolygonsDataFrame,
#                   CRS, over, Polygons, Polygon, SpatialPolygons, proj4string)

rangemap_bound <- function(occurrences, adm_areas, country_code, boundary_level = 0, polygons,
                           kept_data = FALSE, dissolve = FALSE, save_shp = FALSE, name) {
  # testing potential issues
  if (!missing(polygons)) {
    if (class(polygons) != "SpatialPolygonsDataFrame")
    stop("If defined, polygons must be a SpatialPolygonsDataFrame object. If adm_areas",
         "\nis defined, one of the fields (columns) of polygons data must be named \"adm_areas\"",
         "\nand contain names of administrative areas for selection.", sep = "")
  }

  if (missing(occurrences) & missing(adm_areas)) {
    stop("Occurrences and/or adm_areas must exist to perform the analysis.")
  }else {
    if (!missing(occurrences) & missing(adm_areas)) {
      if (dim(occurrences)[2] != 3) {
        stop("occurrences data.frame must have the following columns: \nSpecies, Longitude, and Latitude.")
      }
    }
    if (!missing(occurrences) & !missing(adm_areas)) {
      if (dim(occurrences)[2] != 3) {
        rm("occurrences")
        warning(paste("occurrences data.frame does not have required arrangment:",
                      "\nSpecies, Longitude, and Latitude.", " Species range will be",
                      "\ncreated using adm_areas only.", sep = ""))
      }
      if (!missing(polygons)) {
        if(sum("adm_areas" %in% names(polygons@data)) == 0) {
          stop("Data of polygons does not contain a field (column) named \"adm_names\", see help.")
        }
      }
    }

    if (!missing(adm_areas)) {
      if (!missing(polygons)) {
        if(sum("adm_areas" %in% names(polygons@data)) == 0) {
          stop("Data of polygons does not contain a field (column) named \"adm_names\", see help.")
        }

        polynam <- polygons@data
        a_a_names <- as.vector(unique(polynam[, "adm_areas"]))

        if (sum(adm_areas %in% a_a_names) != length(adm_areas)) {
          warning(paste("Not all of the administrative areas defined in adm_areas coincide",
                        "\nwith the available names for administrative areas in polygons,",
                        "\nonly those that coincide will be used.", sep = ""))
        }

        if (sum(adm_areas %in% a_a_names) == 0 & missing(occurrences)) {
          stop(paste("None of the administrative areas defined in adm_areas coincides",
                     "\nwith the available names for administrative areas in polygons.",
                     sep = ""))
        }

        if (sum(adm_areas %in% a_a_names) == 0 & !missing(occurrences)) {
          rm("adm_areas")
          warning(paste("None of the administrative areas defined in adm_areas coincides",
                        "\nwith the available names for administrative areas in polygons.",
                        "\nSpecies range will be created using occurrences only.",
                        sep = ""))
        }
      }else{
        a_names <- paste("NAME", boundary_level, sep = "_")
        a_a_names <- as.vector(unique(adm_area_names[adm_area_names$ISO3 %in% country_code,
                                                     a_names]))

        if (sum(adm_areas %in% a_a_names) != length(adm_areas)) {
          warning(paste("Not all of the administrative areas defined in adm_areas coincide",
                        "\nwith the available names for that level in the countries listed",
                        "\nin country_code.", sep = ""))
        }

        if (sum(adm_areas %in% a_a_names) == 0 & missing(occurrences)) {
          stop(paste("None of the administrative areas defined in adm_areas coincides",
                     "\nwith the available names for that level in the countries listed",
                     "\nin country_code.", sep = ""))
        }

        if (sum(adm_areas %in% a_a_names) == 0 & !missing(occurrences)) {
          rm("adm_areas")
          warning(paste("None of the administrative areas defined in adm_areas coincides",
                        "\nwith the available names for that level in the countries listed",
                        "\nin country_code. Species range will be created using occurrences only.",
                        sep = ""))
        }
      }
    }
  }

  # initial projection
  WGS84 <- sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

  # world map or user map fro creating species range
  if (missing(polygons)) {
    bounds <- list()
    for (i in 1:length(country_code)){
      bounds[[i]] <- raster::getData('GADM', country = country_code[i], level = boundary_level)
    }
    polygon <- do.call("rbind", bounds)
    polygon@data$OBJECTID <- 1:length(polygon@data$OBJECTID)

    a_names <- ifelse(boundary_level == 0, "NAME_ENGLISH",
                      paste("NAME", boundary_level, sep = "_"))

    polygon@data[, !names(polygon@data) %in% c("OBJECTID", a_names)] <- NULL # erase columns
    names(polygon@data) <- c("OBJECTID", "adm_names")

    if (kept_data == FALSE) {
      # erasing rds files in working directory
      erase_rds <- list.files(path = ".", pattern = "^GADM_", full.names = TRUE)
      unlink(erase_rds)
    }
  }

  # project polygons
  polygons <- sp::spTransform(polygon, WGS84)

  # anlysis
  if (!missing(occurrences)) {
    # erase duplicate records
    occ <- as.data.frame(unique(occurrences))[, 1:3]
    colnames(occ) <- c("Species", "Longitude", "Latitude")

    # make a spatial object from coordinates
    occ_sp <- sp::SpatialPointsDataFrame(coords = occ[, 2:3], data = occ,
                                         proj4string = WGS84)

    # keeping only records in land
    occ_sp <- occ_sp[!is.na(sp::over(occ_sp, as(polygons, "SpatialPolygons"))), ]

    # centriods of points as reference
    centroid <- rgeos::gCentroid(occ_sp, byid = FALSE)
  }else {
    centroid <- rgeos::gCentroid(polygons, byid = FALSE)
  }

  # new projection
  AEQD <- sp::CRS(paste("+proj=aeqd +lat_0=", centroid@coords[2], " +lon_0=", centroid@coords[1],
                        " +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs", sep = ""))

  # reproject polygons
  polygons <- sp::spTransform(polygons, AEQD)

  #selecting polygons
  if (!missing(occurrences) | !missing(adm_areas)) {
    if (!missing(occurrences) & !missing(adm_areas)) {
      # reproject occurrences
      occ_pr <- sp::spTransform(occ_sp, AEQD)

      # select polygons that overlap with points
      boundaries <- polygons[!is.na(sp::over(polygons, as(occ_pr, "SpatialPoints"))), ]

      # select polygons by names
      boundaries1 <- polygons[polygons@data$adm_names %in% adm_areas, ]

      # combining boundaries
      boundaries <- polygons[polygons@data$adm_names %in%
                               as.vector(unique(c(boundaries@data$adm_names,
                                                  boundaries1@data$adm_names))), ]
    }
    if (!missing(occurrences) & missing(adm_areas)) {
      # reproject occurrences
      occ_pr <- sp::spTransform(occ_sp, AEQD)

      # select polygons that overlap with points
      boundaries <- polygons[!is.na(sp::over(polygons, as(occ_pr, "SpatialPoints"))), ]
    }

    if (!missing(adm_areas) & missing(occurrences)) {
      # select polygons by names
      boundaries <- polygons[polygons@data$adm_names %in% adm_areas, ]
    }
  }

  # disolve
  if (dissolve == TRUE) {
    cat("\nDissolving polygons, please wait...\n")
    polygons@data$union_field <- rep("Union", length(polygons@data[, 1])) # new field for union
    polygons <- rgeos::gUnaryUnion(polygons, id = polygons@data$union_field) # now dissolve

    boundaries@data$union_field <- rep("Union", length(boundaries@data[, 1])) # new field for union
    boundaries <- rgeos::gUnaryUnion(boundaries, id = boundaries@data$union_field) # now dissolve
    boundaries <- raster::disaggregate(boundaries)
  }

  # calculate areas in km2
  rangekm2 <- raster::area(boundaries) / 1000000
  areakm2 <- sum(rangekm2) # total area of the species range

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
  boundaries <- sp::SpatialPolygonsDataFrame(boundaries, # species range
                                             data = data.frame(species, rangekm2),
                                             match.ID = FALSE)

  extent_occurrence <- sp::SpatialPolygonsDataFrame(c_hull_extent, # extent of occurrence
                                                    data = data.frame(species, eockm2),
                                                    match.ID = FALSE)

  area_occupancy <- sp::SpatialPolygonsDataFrame(grid_sp, # area of occupancy
                                                 data = data.frame(species, aockm2),
                                                 match.ID = FALSE)

  # exporting
  if (save_shp == TRUE) {
    cat("\nWriting shapefiles in the working directory...\n")
    rgdal::writeOGR(boundaries, ".", name, driver = "ESRI Shapefile")
    rgdal::writeOGR(extent_occurrence, ".", paste(name, "extent_occ", sep = "_"), driver = "ESRI Shapefile")
    rgdal::writeOGR(area_occupancy, ".", paste(name, "area_occ", sep = "_"), driver = "ESRI Shapefile")
    rgdal::writeOGR(occ_pr, ".", paste(name, "unique_records", sep = "_"), driver = "ESRI Shapefile")
  }

  # return results (list or a different object?)
  sp_dat <- data.frame(occ[1, 1], dim(occ_pr)[1], areakm2, eocckm2, aocckm2) # extent of occ = total area?
  colnames(sp_dat) <- c("Species", "Unique_records", "Range_area", "Extent_of_occurrence", "Area_of_occupancy")

  results <- list(sp_dat, occ_pr, boundaries, extent_occurrence, area_occupancy)
  names(results) <- c("Summary", "Species_unique_records", "Species_range", "Extent_of_occurrence",
                      "Area_of_occupancy")
  return(results)
}
