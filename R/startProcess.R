#'@details startProcess is a method for submitting a web processing request for the \code{rGDP} object.
#'
#'@param \code{rGDP} object ot be used to formulate GDP process request.
#'@return An \code{rGDP} object with an active GDP process request.
#'
#'@docType methods
#'@keywords startProcess
#'@description Start process for \code{rGDP}
#'@title Submit a GDP web processing request
#'@seealso \code{checkProcess}
#'@export

setGeneric(name="startProcess",def=function(.Object){standardGeneric("startProcess")})

# '@rdname startProcess-methods
# '@aliases startProcess,rGDP-method
setMethod(f = "startProcess",signature = "rGDP",definition = function(.Object){
	
	requestXML	<-	toString.XMLNode(xmlDoc(suppressWarnings(postInputsToXML(.Object))))
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
