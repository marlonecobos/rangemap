## Resubmission 0.1.17
Resubmission of version 0.1.17. In this version I have made the 
following changes based on CRAN suggestions and issues detected:

* Corrected the canonical URL of the CRAN page for the package in README.
* Modify function rangemap_boundaries and helper to make them fail gracefully if
any of the download processes fail.
* Fixed issue in function eoo when points were widespread.
* Added arguments in main functions to make the obtention of extent of 
occurrence and area of occupancy optional.

**Comments on NOTEs**

* A NOTE may be produced because names of countries in one of the elements in data 
are marked UTF-8 strings; however, the names are correct and we consider they should stay.
* Another NOTE appears because some of the words in the Description
field in DESCRIPTION are considered mis-spelled. However, the words are correct.


## Test environments
* Microsoft Windows Server 2019 10.0.17763 (on GitHub actions) R-release
* ubuntu-20.04 LTS (local) R-release
* ubuntu-20.04 LTS (on GitHub actions) R-release
* ubuntu-20.04 LTS (on GitHub actions) R-devel


## R CMD check results
There were no ERRORs

There were no WARNINGs

There were no NOTEs


## Downstream dependencies
There are currently no downstream dependencies for this package. 


<br>
<hr>

## Submission 0.1.17
This is the first submission of version 0.1.17. In this version I have made the 
following changes based on CRAN suggestions and issues detected:

* Modify function rangemap_boundaries and helper to make them fail gracefully if
any of the download processes fail.
* Fixed issue in function eoo when points were widespread.
* Added arguments in main functions to make the obtention of extent of 
occurrence and area of occupancy optional.

**Comments on NOTEs**

* A NOTE may be produced because names of countries in one of the elements in data 
are marked UTF-8 strings; however, the names are correct and we consider they should stay.
* Another NOTE appears because some of the words in the Description
field in DESCRIPTION are considered mis-spelled. However, the words are correct.


## Test environments
* Microsoft Windows Server 2019 10.0.17763 (on GitHub actions) R-release
* ubuntu-20.04 LTS (local) R-release
* ubuntu-20.04 LTS (on GitHub actions) R-release
* ubuntu-20.04 LTS (on GitHub actions) R-devel


## R CMD check results
There were no ERRORs

There were no WARNINGs

There were no NOTEs


## Downstream dependencies
There are currently no downstream dependencies for this package. 


<br>
<hr>

## Resubmission 0.1.16
This is a resubmission. In this version I have made the following changes based 
on CRAN reviewer suggestions:

* Changed a link that appear invalid in the documentation of country_codes.
* Made changes in vignettes to reduce overall checktime to less than 10 minutes.

**Comments on NOTEs**

* A NOTE may be produced because names of countries in one of the elements in data 
are marked UTF-8 strings; however, the names are correct and we consider they should stay.
* Another NOTE appears because some of the words in the Description
field in DESCRIPTION are considered mis-spelled. However, the words are correct.


## Test environments
* local Windows 10, R 4.0.3
* Microsoft Windows Server 2019 10.0.17763 (on GitHub actions) R-release
* Mac OS 10.13.6 (on R-hub) R-release
* ubuntu-20.04 LTS (on GitHub actions) R-release
* ubuntu-20.04 LTS (on GitHub actions) R-devel


## R CMD check results
There were no ERRORs

There were no WARNINGs

There were no NOTEs


## Downstream dependencies
There are currently no downstream dependencies for this package. 


<br>
<hr>

## Resubmission 0.1.15
This is a resubmission. In this version I have made the following changes based 
on CRAN reviewer suggestions:

* Implemented argument 'verbose' for all functions that print messages. Now, these 
messages can be suppressed easily.
* To avoid an arbitrary definition of seed in a function where reproducibility 
is desired (clusters in helpers.R), an extra argument (set_seed) was added to 
the function to allow explicit control on such seed.
* Added vignettes to the package created by the author added to the list of authors.

**Comments on NOTEs**

* A NOTE may be produced because names of countries in one of the elements in data 
are marked UTF-8 strings; however, the names are correct and we consider they should stay.
* Another NOTE appears because some of the words in the Description
field in DESCRIPTION are considered mis-spelled. However, the words are correct.
* All links work, despite a note related to some of such links.


## Test environments
* local Windows 10, R 4.0.3
* Microsoft Windows Server 2019 10.0.17763 (on GitHub actions) R-release
* Mac OS 10.13.6 (on R-hub) R-release
* ubuntu-20.04 LTS (on GitHub actions) R-release
* ubuntu-20.04 LTS (on GitHub actions) R-devel


## R CMD check results
There were no ERRORs

There were no WARNINGs

There were no NOTEs


## Downstream dependencies
There are currently no downstream dependencies for this package. 


<br>
<hr>

## Resubmission 0.1.12
This is a resubmission. In this version I have made the following changes:

* Excluded Author field, as Authors@R contains more complete information and 
these two fields were not identical.

**Comments on NOTEs**

* A NOTE may be produced because names of countries in one of the elements in data are marked UTF-8 strings; however, the names are correct and we consider they should stay.
* Another NOTE appears because it considers some of the words in the Description
field in DESCRIPTION. However, the words are correct.


## Test environments
* local windows 10, R 4.0.1
* windows-x86_64-devel (on rhub) R-devel
* macos-highsierra-release-cran (on rhub) 4.0.0
* ubuntu 16.04.6 LTS (on travis) 4.0.0


## R CMD check results
There were no ERRORs

There were no WARNINGs

NOTEs:

There were two NOTEs:

* Found 7734 marked UTF-8 strings.
* Possibly mis-spelled words in DESCRIPTION:
    Hijmans, IUCN, al, et


## Downstream dependencies
There are currently no downstream dependencies for this package. 


<br>
<hr>


## First submission 0.1.12
This is the first submission.

**Comments**

* A NOTE may be produced because names of countries in one of the elements in data
are marked UTF-8 strings; however, the names are correct and we consider they 
should stay. 


## Test environments
* local windows 10, R 4.0.1
* windows-x86_64-devel (on rhub) R-devel
* macos-highsierra-release-cran (on rhub) 4.0.0
* ubuntu 16.04.6 LTS (on travis) 4.0.0


## R CMD check results
There were no ERRORs

There were no WARNINGs

NOTEs:

There was one NOTE (windows-x86_64-devel, macos-highsierra):

* Found 7734 marked UTF-8 strings.


## Downstream dependencies
There are currently no downstream dependencies for this package. 

