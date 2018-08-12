## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- message=FALSE, warning=FALSE---------------------------------------
# devtools would help us to install rangemap from GitHub
if(!require(devtools)){
  install.packages("devtools")
}

# installing rangemap if needed
if(!require(rangemap)){
  devtools::install_github("marlonecobos/rangemap")
  library(rangemap)
}

# rgibif would help us to get some species occurrence data in our example
if(!require(rgbif)){
  install.packages("rgbif")
  library(rgbif)
}

## ---- cache=TRUE---------------------------------------------------------
# getting the data from GBIF
species <- name_lookup(query = "Ictinia mississippiensis",
                       rank="species", return = "data") # information about the species

species$key # to see all the keys we can try

# key 4 was the one with georeferenced occurrences
occ_count(taxonKey = species$key[9], georeferenced = TRUE) 
key <- species$key[9] # using species key that return information

occ <- occ_search(taxonKey = key, return = "data") # using the taxon key

# keeping only species name, and coordinates
occ_g <- occ[!is.na(occ$decimalLatitude) & !is.na(occ$decimalLongitude),
            c("name", "decimalLongitude", "decimalLatitude")]


## ---- warning=FALSE, cache=TRUE------------------------------------------
# checking which countries may be involved in the analysis
rangemap_explore(occurrences = occ_g)

## ---- cache=TRUE---------------------------------------------------------
# Defining parameters
dist <- 300000 # buffer distance in meters

# Now we can create the species range based on buffers
buff_range <- rangemap_buff(occurrences = occ_g, buffer_distance = dist)

## ---- cache=TRUE---------------------------------------------------------
# Defining parameters
level <- 1 # level of detail for administrative areas, 1 = states
dissolve <- FALSE # make this TRUE if you want polygons with no admininstrative limits
countries <- c("USA", "MEX") # ISO names of countries involved in the analysis

# Now we can create the species range based on administrative areas
bound_range <- rangemap_bound(occurrences = occ_g, country_code = countries,
                              boundary_level = level, dissolve = dissolve)

## ---- cache=TRUE---------------------------------------------------------
# Defining parameters
dist <- 200000
hull <- "concave"

# Now we can create the species range based on administrative areas
hull_range <- rangemap_hull(occurrences = occ_g, hull_type = hull, buffer_distance = dist)

## ---- fig.show='hold', warning=FALSE-------------------------------------
rangemap_fig(buff_range, add_occurrences = TRUE, zoom = 2, northarrow = TRUE,
             range_color = "transparent")
rangemap_fig(buff_range, zoom = 2)
rangemap_fig(bound_range, zoom = 2)
rangemap_fig(hull_range, zoom = 2, legend = TRUE, legend_position = "bottomleft")

