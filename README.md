`geoknife`
=====

Tools for geo-web processing of gridded data via the [Geo Data Portal](http://cida.usgs.gov/gdp/ "Geo Data Portal"). `geoknife` slices up gridded data according to overlap with irregular features, such as watersheds, lakes, points, etc. The result is subsetted data in plain text, NetCDF, geotiff or other formats.

![alt tag](http://github.usgs-r.io/images/geoknife.png)


###Installing `geoknife`
install this package using

	install.packages("geoknife", 
        repos = c("http://usgs-r.github.com", "http://cran.us.r-project.org"),
        dependencies = TRUE, type = "both")

###`geoknife` overview (as of v0.2.2)
The `geoknife` package was created support web-based geoprocessing of large gridded datasets according to their overlap with landscape (or aquatic/ocean) features that are often irregularly shaped. `geoknife` creates data access and subsequent geoprocessing requests for the USGS's Geo Data Portal to carry out on a web server. The results of these requests are available for download after the process(es) have been completed. This type of workflow has three main advantages: 1) it allows the user to avoid downloading large datasets, and 2) it avoids reinventing the wheel for the creation and optimization of complex geoprocessing algorithms, 3) computing resources are dedicated elsewhere, so `geoknife` operations do not have much of an impact on a local computer. 

`geoknife` interacts with a remote server to figure out what types of processing capabilities are available, in addition to seeing what types of geospatial features are already available to be used as an area of interest (commonly user-uploaded shapefiles). Because communication with web resources are central to `geoknife` operations, users must have an active internet connection. 

The main elements of setting up and carrying out a `geoknife` 'job' are include defining the processing algorithm that will be used, choosing an area of interest, filling out the details for the job details (including the dataset to be used; these details are called 'process inputs'), and sending off the job request (see documentation for `setFeature<-`, `setProcessInputs<-`, and `startProcess` for more information).

###`geoknife` Functions (as of v0.2.2)
| Function       | Title           |
| ------------- |:-------------|
| `bufferPoint` | Create linear ring from point |
| `checkProcess`  | Check status of processing request |
| `geoknife` | Create geoknife object |
| `getAlgorithms` | Get processing algorithms |
| `getAttributes` | Get attributes from a shapefile at a web location |
| `getDataIDs` | Find variables from dataset |
| `printProcessInputs` | Print out the process request xml for diagnostic purposes |
| `setAlgorithm<-` | Set processing algorithm |
| `setFeature<-` | Set feature geometry for processing |
| `setProcessInputs<-` | Set inputs for web processing |
| `setWFS<-` | Set web feature service location |
| `setWPS<-` | Set web processing service location |
| `startProcess` | Submit a GDP web processing request |

##What libraries does `geoknife` need?
This version requires `RCurl`, and `XML`. Both of these packages are available on CRAN, and will be installed automatically when using the `install.packages()` instructions above.

##Disclaimer
This software is in the public domain because it contains materials that originally came from the United States Geological Survey, an agency of the United States Department of Interior. For more information, see the [official USGS copyright policy](http://www.usgs.gov/visual-id/credit_usgs.html#copyright/ "official USGS copyright policy")

This software is provided "AS IS".