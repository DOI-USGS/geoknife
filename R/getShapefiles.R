#'@title get shapefiles from a web location
#'
#'@details a \code{geoknife} method for finding shapefile names at a valid WFS endpoint.
#'
#'@param \code{geoknife} object with a valid WFS url.
#'@return list of shapefiles for the \code{geoknife} WFS url.
#'@docType methods
#'@keywords methods
#'@examples gk<- geoknife() # create geoknife object
#'getShapefiles(gk) # display shapefile names
#'@import XML
#'@import RCurl
#'@author Jordan S. Read
#'@export
setGeneric(name="getShapefiles",def=function(.Object){standardGeneric("getShapefiles")})

# '@rdname getShapefiles-methods
# '@aliases getShapefiles,geoknife-method	
setMethod(f = "getShapefiles",signature="geoknife",
	definition = function(.Object){
		parentKey 	<-	"featuretypelist"
		childKey	<-	"featuretype"
		processURL	<-	paste(c(.Object@WFS_URL,'?service=WFS&version=',
			.Object@WFS_DEFAULT_VERSION,'&request=GetCapabilities'),collapse="")
		shapefiles	<-	parseXMLnodes(processURL,parentKey,childKey)
		return(shapefiles)
	})