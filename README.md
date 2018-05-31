README
================
May 16, 2018

-   [Package description](#package-description)
-   [Installing the package](#installing-the-package)

<br>

### Package description

The **rangemap** R package presents various tools to create species range maps based on occurrence data, statistics, and distinct shapefiles. Other tools of this package can be used to analyze environmental characteristics of the species ranges and to create high quality figures of these maps.

<br>

### Installing the package

**rangemap** is in a GitHub repository and can be installed and/or loaded using the following code (make sure to have Internet connection).

``` r
# Installing and loading packages
if(!require(devtools)){
    install.packages("devtools")
    library(devtools)
}

if(!require(rangemap)){
    devtools::install_github("marlonecobos/rangemap")
    library(rangemap)
}
```
