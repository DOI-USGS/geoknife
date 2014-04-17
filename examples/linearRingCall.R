require(geoknife)

# create geoknife object w/ defaults
geoknife <- geoknife()
# give this geoknife object a linear ring as the feature of interest (will be adding multiple rings in the future, but...)
linearRing = bufferPoint(c(-111.48,36.95))
setFeature(geoknife) <- list(LinearRing=linearRing)

# get a list of available processing algorithms
getAlgorithms(geoknife)

# set processing algorithm to feature weighted grid statistics (unweighted will likely fail, because the ring won't intersect the centroids)
setAlgorithm(geoknife) <- getAlgorithms(geoknife)[4] # feature weighted

# set the post inputs for the processing dataset
setProcessInputs(geoknife) <- list('DATASET_ID'='Downward_longwave_radiation_flux_surface',
                                        'DATASET_URI'='dods://igsarm-cida-thredds1.er.usgs.gov:8081/qa/thredds/dodsC/nldas/best',
                                        'TIME_START'='2010-01-01T00:00:00Z',
                                        'TIME_END'='2010-01-01T23:00:00Z',
                                        'DELIMITER'='TAB')

# print it out so you know what's up
geoknife 

# kick off your request
geoknife <- startProcess(geoknife)

status.geoknife  <-  checkProcess(geoknife)

cat('checking status of GDP request. Large complex requests take longer to process.\n')
repeat{
	if (!is.null(status.geoknife$URL) | status.geoknife$status!=""){
	    break
	  }
  cat('checking process...\n')
  Sys.sleep(10)
  if (is.null(status.geoknife$URL)){
    status.geoknife  <-  checkProcess(geoknife)
  }
}

if (status.geoknife$status=='Process successful'){
	cat(paste(status.geoknife$status,'\nDownload available at: ',status.geoknife$URL,sep=''))
} else {
	cat(status.geoknife$status)
}

