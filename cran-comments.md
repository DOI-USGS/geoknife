## Resubmission
This is a resubmission. In this version I have:

* Added a new output type to data subsets (geotiff)

* Modified the GET and POST requests to work correctly with the release and dev version of the httr package

* improved documentation

* Fixed the CITATION file DOI, which was improperly formatted

### a note about geoknife URLs:
this package has a numer of service URLs that can be found in code, 
in help text, and in vignettes. Some of these URLs will return 404s 
without the proper POST body 
(e.g., http://cida.usgs.gov/gdp/process/WebProcessingService). These 
should be ignored for the purpose of testing valid urls. 