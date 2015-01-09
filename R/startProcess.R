#'@details startProcess is a method for submitting a web processing request for the \code{geoknife} object.
#'
#'@param .Object a \code{geoknife} object ot be used to formulate GDP process request.
#'@return An \code{geoknife} object with an active GDP process request.
#'
#'@docType methods
#'@keywords startProcess
#'@description Start process for \code{geoknife}
#'@title Submit a GDP web processing request
#'@seealso \code{checkProcess}
#'@export
#'@examples 
#'gk <- geoknife() # create geoknife object
#'gk # print geoknife object
#'
#'linearRing = bufferPoint(c(-111.48,36.95))
#'setFeature(gk) <- list(LinearRing=linearRing)
#'
#' #get a list of available processing algorithms
#'algs <- getAlgorithms(gk)
#'
#' #set processing algorithm to feature weighted grid statistics (unweighted will likely fail, because the ring won't intersect the centroids)
#'setAlgorithm(gk) <- algs[4] # feature weighted
#'
#' # set the post inputs for the processing dataset
#'setProcessInputs(gk) <- list('DATASET_ID'='Prcp',
#'                                        'DATASET_URI'='dods://cida.usgs.gov/thredds/dodsC/gmo/GMO_w_meta.ncml',
#'                                        'TIME_START'='2010-01-01T00:00:00Z',
#'                                        'TIME_END'='2010-05-01T00:00:00Z',
#'                                        'DELIMITER'='TAB')
#'gk # print geoknife object contents
#'
#' # kick off your request
#'gk <- startProcess(gk)
#'gk # display contents

setGeneric(name="startProcess",def=function(.Object){standardGeneric("startProcess")})

# '@rdname startProcess-methods
# '@aliases startProcess,geoknife-method
setMethod(f = "startProcess",signature = "geoknife",definition = function(.Object){
	
	requestXML	<-	toString.XMLNode(xmlDoc(suppressWarnings(processInputsToXML(.Object))))
	myheader	<-	c(Connection="close", 
	          			'Content-Type' = "application/xml")#text/xml?
	
	data	<-	 getURL(url = .Object@WPS_URL,
	               postfields=requestXML,
	               httpheader=myheader,
	               verbose=FALSE)		
	xmltext 	<-	xmlTreeParse(data, asText = TRUE,useInternalNodes=TRUE)
	response	<-	xmlRoot(xmltext)
	responseNS	<-	xmlNamespaceDefinitions(response, simplify = TRUE)  
	processID	<-	xmlGetAttr(response,"statusLocation")
	
	.Object	<-	setProcessID(.Object,processID)
	return(.Object)
})
