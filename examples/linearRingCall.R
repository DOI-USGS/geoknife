require(rGDP)

# create rGDP object w/ defaults
rGDP <- rGDP()
# give this rGDP object a linear ring as the feature of interest (will be adding multiple rings in the future, but...)
rGDP <- setFeature(rGDP,list(LinearRing=c(-111.48,36.95, -111.48, 36.92, -111.47, 36.93,-111.47, 36.95,-111.48,36.95)))

# get a list of available processing algorithms
getAlgorithms(rGDP)

# set processing algorithm to feature weighted grid statistics (unweighted will likely fail, because the ring won't intersect the centroids)
rGDP <- setAlgorithm(rGDP,getAlgorithms(rGDP)[4]) # feature weighted

# set the post inputs for the processing dataset
rGDP <-  setPostInputs(rGDP,list('DATASET_ID'='Downward_longwave_radiation_flux_surface',
                                        'DATASET_URI'='dods://igsarm-cida-thredds1.er.usgs.gov:8081/qa/thredds/dodsC/nldas/best',
                                        'TIME_START'='2010-01-01T00:00:00Z',
                                        'TIME_END'='2010-01-01T23:00:00Z',
                                        'DELIMITER'='TAB'))

# print it out so you know what's up
rGDP 

# kick off your request
rGDP <- executePost(rGDP)

status.rGDP  <-  checkProcess(rGDP)

cat('checking status of GDP request. Large complex requests take longer to process.\n')
repeat{
	if (!is.null(status.rGDP$URL) | status.rGDP$status!=""){
	    break
	  }
  cat('checking process...\n')
  Sys.sleep(10)
  if (is.null(status.rGDP$URL)){
    status.rGDP  <-  checkProcess(rGDP)
  }
}

if (status.rGDP$status=='Process successful'){
	cat(paste(status.rGDP$status,'\nDownload available at: ',status.rGDP$URL,sep=''))
} else {
	cat(status.rGDP$status)
}

