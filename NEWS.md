geoknife 1.6.9
==========
* fixed documentation with duplicate "..." parameter.

geoknife 1.6.8
==========
* fix cran test failure when web services unavailable.
* migrate to use of an AWS-based instance of the GDP web service.

geoknife 1.6.7
==========
* fixed issue with names from simplegeoms created from data.frames
* corrected url for daymet service
* fixed a bug with progress bars failing or reporting more than 100% complete.
* removed wcs subset references
* replaced dataset in geotiff vignette
* progress bar is now optional
* reorganized package files
* removed sp from tests

geoknife 1.6.6
==========
* Modified error handling when Geo Data Portal is offline.

geoknife 1.6.5
==========
* Updated schema for jobs that use geoknife generated geometry
* Migrated package to use github actions for CI

geoknife 1.6.4
==========
* Improved error handling for failed processing jobs
* Fixed bug with supported CRS