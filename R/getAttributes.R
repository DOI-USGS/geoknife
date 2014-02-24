#'@title get attributes from a shapefile at a web location
#'
#'@details a \code{geoknife} method for finding attribute names for a given shapefile at a valid WFS endpoint. 
#'
#'@param \code{geoknife} object with a valid WFS url.
#'@param a valid shapefile name.
#'@return list of attributes for the given shapefile at the \code{geoknife} WFS url.
#'@docType methods
#'@keywords getAttributes
#'@import XML
#'@import RCurl
#'@examples gk<- geoknife() # create geoknife object
#'shps <- getShapefiles(gk) # get shapefile names
#'getAttributes(gk,shapefile=shps[2])
#'@export
setGeneric(name="getAttributes",def=function(.Object,shapefile){standardGeneric("getAttributes")})

# '@rdname getAttributes-methods
# '@aliases getAttributes,geoknife-method	
setMethod(f = "getAttributes",signature="geoknife",
	definition = function(.Object,shapefile){
		parentKey	<-	"element"
		childKey	<-	"maxoccurs"
		processURL	<-	paste(c(.Object@WFS_URL,'?service=WFS&version=',
			.Object@WFS_DEFAULT_VERSION,'&request=DescribeFeatureType',
			'&typename=',shapefile),collapse="")
		attributes	<-	parseXMLattributes(processURL,parentKey,childKey)
		return(attributes)
	})