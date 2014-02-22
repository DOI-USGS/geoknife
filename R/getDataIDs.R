#'@details getDataIDs is a method for finding dataset variables from a
#' \code{rGDP} object with a valid dataset URI. Use setPostInputs to set a dataset URI.
#'
#'@param \code{rGDP} object with a valid dataset URI.
#'@param cachedResponse an optional input to allow cached response. Default as FALSE
#'@return list of dataset IDs for the \code{rGDP} dataset URI.
#'@docType methods
#'@description Find variables from \code{rGDP} dataset
#'@title Find variables from dataset
#'@keywords getDataIDs
#'@seealso \code{setPostInputs}
#'@export
setGeneric(name="getDataIDs",def=function(.Object,cachedResponse){standardGeneric("getDataIDs")})

# '@rdname getDataIDs-methods
# '@aliases getDataIDs,rGDP-method
setMethod(f = "getDataIDs",signature="rGDP",
	definition = function(.Object,cachedResponse){
		
		if (missing(cachedResponse)){
			cachedResponse=FALSE
		}
			if ("DATASET_URI" %in% names(.Object@postInputs)){
				algorithm	<-	.Object@dataList
				requestXML	<-	generateRequest(.Object, algorithm,cachedResponse)
				url = .Object@UTILITY_URL
				responseXML	<-	genericExecute(url,requestXML)
			} else {
				stop('must have a DATASET_URI set as a postInput')
			}
			# get complex data
			cData	<-	xmlValue(getNodeSet(responseXML, "//ns:LiteralData")[[1]])
			cDataXML	<-	xmlInternalTreeParse(cData)
			dataIDs	<-	sapply(getNodeSet(cDataXML,"//gdp:name"),xmlValue)
			return(dataIDs)
	})