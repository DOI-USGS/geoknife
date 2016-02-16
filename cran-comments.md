## Resubmission
This is a resubmission. In this version I have:

* Added a new vignette (plotting geotiffs)

* Modified the GET and POST requests to capture a warning message from the new CRAN version of the httr package

* Added a new feature that allows users to turn on a verbose mode for web requests, and also set defaults for a number of variables. 

* Removed urls in the vignette that cause 404s to url checks (as they are urls that receive POST bodies, not GET/HEAD requests)
