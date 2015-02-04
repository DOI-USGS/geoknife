#'@title set inputs for web processing
#'
#'@details method for setting the (non-feature related) post inputs of the \code{geoknife} object. 
#'
#'@param .Object a \code{geoknife} object.
#'@param value a list of valid processInputs.
#'@return An \code{geoknife} object with updated postInputs.
#'@docType methods
#'@keywords methods
#'@examples 
#'\dontrun{
#'gk <- geoknife() # create geoknife object
#'algorithm <- list("Area Grid Statistics (weighted)"=
#'                    "gov.usgs.cida.gdp.wps.algorithm.FeatureWeightedGridStatisticsAlgorithm")
#'setAlgorithm(gk) <- algorithm
#' # set the post inputs for the processing dataset
#' setProcessInputs(gk) <- list('DATASET_ID'='Downward_longwave_radiation_flux_surface',
#'                                        'DATASET_URI'='dods://hydro1.sci.gsfc.nasa.gov:80/dods/NLDAS_FORA0125_H.002',
#'                                        'TIME_START'='2010-01-01T00:00:00Z',
#'                                        'TIME_END'='2010-01-01T23:00:00Z',
#'                                        'DELIMITER'='TAB')
#'gk # print geoknife object contents
#'}
#'@author Jordan S. Read
#'@export
setGeneric(name="setProcessInputs<-",def=function(.Object,value){standardGeneric("setProcessInputs<-")})


# '@rdname setProcessInputs-methods
# '@aliases setProcessInputs,geoknife-method  
setReplaceMethod(f = "setProcessInputs",signature = "geoknife",
                 definition = function(.Object,value){
                   
                   if (!is.list(value)){stop(paste0(value,' needs to be a valid list'))}
                   #if (!all(names(value) %in% names(.Object@processInputs))){
                    # stop(paste0(value,' needs names supported by processInputs'))
                   #}
                   
                   if ("empty" %in% names(.Object@algorithm)){
                     stop('an algorithm must be chosen before setting processInputs')
                   }
                   for (i in seq_len(length(names(value)))){
                     .Object@processInputs[names(value[i])]	<-	value[[i]]
                   }
                   
                   .Object	<-	dodsReplace(.Object)
                   
                   return(.Object)
                 })

dodsReplace	<-	function(.Object){
  # checks for dods or opendap, and replaces
  
  
  if ("DATASET_URI" %in% names(.Object@processInputs) & 
        !is.null(.Object@processInputs$DATASET_URI)) {
    
    uri	<-	.Object@processInputs$DATASET_URI
    if (grepl('dodsC',uri)){
      uri	<-	gsub('http', 'dods', uri)
    }
    if (grepl('opendap',uri)){
      uri	<-	gsub('http', 'opendap', uri)
    }
    
    .Object@processInputs	<-	setList(.Object@processInputs,list('DATASET_URI'=uri))
  }
  return(.Object)
}