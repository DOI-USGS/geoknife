#'Print out the process request xml for diagnostic purposes
#'
#'function for printing processXML \code{geoknife} object. 
#'
#'@param .Object a \code{geoknife} object.
#'@keywords methods
#'@export
#'@examples gk <- geoknife() # create geoknife object
#'linearRing = bufferPoint(c(-111.48,36.95))
#'setFeature(gk) <- list(LinearRing=linearRing)
#'
#'algorithm <- list("Area Grid Statistics (weighted)"=
#'"gov.usgs.cida.gdp.wps.algorithm.FeatureWeightedGridStatisticsAlgorithm")
#'setAlgorithm(gk) <- algorithm
#'setProcessInputs(gk) <- list("DATASET_URI" = "dods://hydro1.sci.gsfc.nasa.gov:80/dods/NLDAS_FORA0125_H.002")
#'printProcessInputs(gk)
printProcessInputs  <-	function(.Object){
  # FAILS when process inputs are NULL...
  requestXML	<-	suppressWarnings(processInputsToXML(.Object))
  print(requestXML)
  
  # needs a valid algorithm
}