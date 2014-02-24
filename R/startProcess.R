#'@details startProcess is a method for submitting a web processing request for the \code{geoknife} object.
#'
#'@param \code{geoknife} object ot be used to formulate GDP process request.
#'@return An \code{geoknife} object with an active GDP process request.
#'
#'@docType methods
#'@keywords startProcess
#'@description Start process for \code{geoknife}
#'@title Submit a GDP web processing request
#'@seealso \code{checkProcess}
#'@export

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
