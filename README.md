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

The main elements of setting up and carrying out a `geoknife` 'job' (`geojob`) include defining the feature of interest (the `stencil` argument in the `geoknife` function), the gridded web dataset to be processed (the `fabric` argument in the `geoknife` function), and the the processing algorithm parameters (the `knife` argument in the `geoknife` function). The status of the `geojob` can be checked with `check`, and output can be loaded into a data.frame with `loadOutput`. 

###What can `geoknife` do?
#####define a stencil that represents the geographic region to slice out of the data
```R
library(geoknife)
# from a single point
stencil <- simplegeom(c(-89, 46.23))
   # -- or --
# from a collection of named points
stencil <- simplegeom(data.frame(
              'point1' = c(-89, 46), 
              'point2' = c(-88.6, 45.2)))
   # -- or --
#for a state from a web available dataset
stencil <- webgeom('state:NH')
```
#####define a fabric that represents the underlying data
```R
# from the prism dataset:
fabric <- webdata('prism')
   # -- or --
# explicitly define webdata from a list:
fabric <- webdata(list(
            times = as.POSIXct(c('1895-01-01','1899-01-01')),
            url = 'http://cida.usgs.gov/thredds/dodsC/prism',
            variables = 'ppt'))
# modify the times field:
times(fabric) <- as.POSIXct(c('1990-01-01','2005-01-01'))
```
#####create the processing job that will carry out the subsetting/summarization task
```R
job <- geoknife(stencil, fabric)

# use existing convienence functions to check on the job:
check(job)
running(job)
error(job)
successful(job)

# wait a little for the process to complete
Sys.sleep(5)
if (successful(job)){
   # -- load up the output and plot it --
   data <- loadOutput(job)
   plot(data[,1:2], ylab = variables(fabric))
}
```

###`geoknife` Functions (as of v0.6.2)
| Function       | Title           |
| ------------- |:-------------|
| `geoknife` | slice up gridded data according to overlap with feature(s) |
| `algorithm` | the algorithm of a `webprocess` |
| `attribute` | the attribute of an `webgeom` |
| `bufferPoint` | create linear ring from a point |
| `check` | check status of `geojob` |
| `error` | convenience  function for state of `geojob` |
| `running` | convenience  function for state of `geojob` |
| `successful` | convenience  function for state of `geojob` |
| `start` | start a `geojob` |
| `geom` | the geom of a `webgeom` | 
| `id` | the process id of a `geojob` |
| `values` | the values of a `webgeom` | 
| `loadOutput` | load the output of a completed `geojob` into data.frame |
| `variables` | the variables for a `webdata` object |
| `times` | the times of a `webdata` object |
| `url` | the url of a `webdata`, `webgeom`, `geojob`, or `webprocess` |
| `variables` | the variables of a `webdata` |
| `version` | the version of a `webgeom` or `webdata` |
| `xml` | the xml of a `geojob` |

###`geoknife` classes (as of v0.6.2)
| Class       | Title           |
| ------------- |:-------------|
| `simplegeom` | a simple geometric class. Extends `sp::SpatialPolygons` |
| `webgeom` | a web feature service geometry |
| `webprocess` | a web processing service |
| `webdata` | web data |
| `geojob` | a geo data portal processing job |

##What libraries does `geoknife` need?
This version requires `httr`, `jsonlite`, `lubridate` and `XML`. All of these packages are available on CRAN, and will be installed automatically when using the `install.packages()` instructions above.

##Disclaimer
This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the [official USGS copyright policy](http://www.usgs.gov/visual-id/credit_usgs.html#copyright/ "official USGS copyright policy")

Although this software program has been used by the U.S. Geological Survey (USGS), no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."
