#'@title get attributes from a shapefile at a web location
#'
#'@details a \code{geoknife} method for finding attribute names for a given shapefile at a valid WFS endpoint. 
#'
#'@param .Object a \code{geoknife} object with a valid WFS url.
#'@param shapefile a valid shapefile name.
#'@return list of attributes for the given shapefile at the \code{geoknife} WFS url.
#'@docType methods
#'@keywords getAttributes
#'@author Jordan S. Read
#'@import XML
#'@importFrom httr GET
#'@examples 
#'gk<- geoknife() # create geoknife object
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
		URL <- sprintf('%s?service=WFS&version=%s&request=DescribeFeatureType&typename=%s',
                   .Object@WFS_URL, .Object@WFS_DEFAULT_VERSION, shapefile)
    describeDoc <- GET(URL)
		attributes	<-	unique(parseXMLattributes(describeDoc,parentKey,childKey))
		return(attributes)
	})