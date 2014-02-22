
#'executePost
#'
#'method for executing the \code{rGDP} object.
#'
#'@param \code{rGDP} object ot be used to formulate GDP process request.
#'@return An \code{rGDP} object.
#'@docType methods
#'@keywords executePost
#'@export
setGeneric(name="executePost",def=function(.Object){standardGeneric("executePost")})

# '@rdname executePost-methods
# '@aliases executePost,rGDP-method
setMethod(f = "executePost",signature = "rGDP",definition = function(.Object){
	
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
