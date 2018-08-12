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

# installing rangemap
devtools::install_github("marlonecobos/rangemap")
library(rangemap)

# rgibif would help us to get some species occurrence data in our example
if(!require(rgbif)){
install.packages("rgbif")
library(rgbif)
}

## ---- cache=TRUE---------------------------------------------------------
# getting the data from GBIF
species <- name_lookup(query = "Capra pyrenaica",
                       rank="species", return = "data") # information about the species

species$key # to see all the keys we can try

# key 4 was the one with georeferenced occurrences
occ_count(taxonKey = species$key[4], georeferenced = TRUE) 
key <- species$key[4] # using species key that return information

occ <- occ_search(taxonKey = key, return = "data") # using the taxon key

# keeping only species name, and coordinates
occ_g <- occ[!is.na(occ$decimalLatitude) & !is.na(occ$decimalLongitude),
            c("name", "decimalLongitude", "decimalLatitude")]


## ---- fig.show='hold', warning=FALSE, cache=TRUE-------------------------
# checking which countries may be involved in the analysis
rangemap_explore(occurrences = occ_g)
rangemap_explore(occurrences = occ_g, show_countries = TRUE)

## ---- eval=FALSE---------------------------------------------------------
#  help(rangemap_fig)

## ------------------------------------------------------------------------
level <- 0 # level of detail for administrative areas
dissolve <- FALSE # make this TRUE if you want polygons with no admininstrative limits
countries <- c("ESP", "PRT", "FRA") # ISO names of countries involved in the analysis

## ---- cache=TRUE---------------------------------------------------------
bound_range <- rangemap_bound(occurrences = occ_g, country_code = countries,
                              boundary_level = level, dissolve = dissolve)

## ---- eval=FALSE---------------------------------------------------------
#  save <- TRUE # to save the results
#  name <- "test" # name of the results
#  
#  bound_range <- rangemap_bound(occurrences = occ_g,  country_code = countries,
#                                boundary_level = level, dissolve = dissolve,
#                                save_shp = save, name = name)

## ---- eval=FALSE---------------------------------------------------------
#  help(rangemap_fig)

## ---- warning=FALSE------------------------------------------------------
rangemap_fig(bound_range) 

## ------------------------------------------------------------------------
extent <- TRUE # adds the extent of occurrence of the species to the figure
occ <- TRUE # adds the occurrence records of the species to the figure
legend <- TRUE # adds a legend to the figure
leg_pos <- "topleft" # position of the legend in the figure
north <- TRUE # adds a north arrow to the figure
n_pos <- "bottomleft" # position of the north arrow

## ---- warning=FALSE------------------------------------------------------
rangemap_fig(bound_range, add_extent = extent, add_occurrences = occ, 
             legend = legend, legend_position = leg_pos, 
             northarrow = north, northarrow_position = n_pos)

