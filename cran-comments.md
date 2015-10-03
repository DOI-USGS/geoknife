## Resubmission
This is a resubmission. In this version I have:

* Converted the DESCRIPTION title to title case.

* Modified the Description to begin with "Process..."

* removed a long running web service call from the vignette, 
which can sometimes fail.

* issue with no vignette index was resolved

### a note about geoknife URLs:
this package has a numer of service URLs that can be found in code, 
in help text, and in vignettes. Some of these URLs will return 404s 
without the proper POST body 
(e.g., http://cida.usgs.gov/gdp/process/WebProcessingService). These 
should be ignored for the purpose of testing valid urls. 