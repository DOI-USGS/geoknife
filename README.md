`geoknife`
=====
[![Build status](https://ci.appveyor.com/api/projects/status/0iacmg82mp50426o/branch/master)](https://ci.appveyor.com/project/jread-usgs/geoknife/branch/master)
[![Build Status](https://travis-ci.org/USGS-R/geoknife.svg)](https://travis-ci.org/USGS-R/geoknife)
[![Coverage Status](https://coveralls.io/repos/USGS-R/geoknife/badge.svg)](https://coveralls.io/r/USGS-R/geoknife)
Tools for geo-web processing of gridded data via the [Geo Data Portal](http://cida.usgs.gov/gdp/ "Geo Data Portal"). `geoknife` slices up gridded data according to overlap with irregular features, such as watersheds, lakes, points, etc. The result is subsetted data in plain text, NetCDF, geotiff or other formats.
<p align="center">
  <img src="http://usgs-r.github.io/images/geoknife.png" alt="GDP" align="center">
</p>



###Installing `geoknife`
install this package using

	install.packages("geoknife", 
        repos = c("http://owi.usgs.gov/R", "http://cran.us.r-project.org"),
        dependencies = TRUE)

###`geoknife` overview
The `geoknife` package was created to support web-based geoprocessing of large gridded datasets according to their overlap with landscape (or aquatic/ocean) features that are often irregularly shaped. `geoknife` creates data access and subsequent geoprocessing requests for the USGS's Geo Data Portal to carry out on a web server. The results of these requests are available for download after the processes have been completed. This type of workflow has three main advantages: 1) it allows the user to avoid downloading large datasets, 2) it avoids reinventing the wheel for the creation and optimization of complex geoprocessing algorithms, and 3) computing resources are dedicated elsewhere, so `geoknife` operations do not have much of an impact on a local computer. 

`geoknife` interacts with a remote server to figure out what types of processing capabilities are available, in addition to seeing what types of geospatial features are already available to be used as an area of interest (commonly, these are user-uploaded shapefiles). Because communication with web resources are central to `geoknife` operations, users must have an active internet connection. 

The main elements of setting up and carrying out a `geoknife` 'job' are include defining the processing algorithm that will be used, choosing an area of interest, filling out the details for the job details (including the dataset to be used; these details are called 'process inputs'), and sending off the job request (see documentation for `setFeature<-`, `setProcessInputs<-`, and `startProcess` for more information).

###`geoknife` Functions (as of v0.4.1)
| Function       | Title           |
| ------------- |:-------------|
| `bufferPoint` | Create linear ring from point |
| `checkProcess`  | Check status of processing request |
| `geoknife` | Create geoknife object |
| `getAlgorithms` | Get processing algorithms |
| `getAttributes` | Get attributes from a shapefile at a web location |
| `getDataIDs` | Find variables from dataset |
| `getValues` | get values from a shapefile at a web location |
| `isError` | Convenience function for GDP process state |
| `isRunning` | Convenience function for GDP process state |
| `isSuccessful` | Convenience function for GDP process state |
| `loadOutput` | parse process output into R environment |
| `parseTimeseries` | parse timeseries file into R environment |
| `printProcessInputs` | Print out the process request xml for diagnostic purposes |
| `setAlgorithm<-` | Set processing algorithm |
| `setFeature<-` | Set feature geometry for processing |
| `setProcessInputs<-` | Set inputs for web processing |
| `setWFS<-` | Set web feature service location |
| `setWPS<-` | Set web processing service location |
| `startProcess` | Submit a GDP web processing request |

##What libraries does `geoknife` need?
This version requires `httr`, `jsonlite`, `lubridate` and `XML`. All of these packages are available on CRAN, and will be installed automatically when using the `install.packages()` instructions above.

##Disclaimer
This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the [official USGS copyright policy](http://www.usgs.gov/visual-id/credit_usgs.html#copyright/ "official USGS copyright policy")

Although this software program has been used by the U.S. Geological Survey (USGS), no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."
