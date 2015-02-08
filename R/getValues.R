#'@title get values from a shapefile at a web location
#'
#'@details a \code{geoknife} method for finding value names for a given shapefile at a valid WFS endpoint. 
#'
#'@param .Object a \code{geoknife} object with a valid WFS url.
#'@param shapefile a valid shapefile name.
#'@param attribute a valid attribute name for the shapefile.
#'@return list of values for the given shapefile attribute at the \code{geoknife} WFS url.
#'@docType methods
#'@keywords methods
#'@import XML
#'@import RCurl
#'@examples 
#'gk<- geoknife() # create geoknife object
#'shps <- getShapefiles(gk) # get shapefile names
#'atts <- getAttributes(gk,shapefile=shps[2])
#'getValues(gk,shapefile=shps[2],attribute=atts[1])
#'@author Jordan S. Read
#'@export
setGeneric(name="getValues",def=function(.Object,shapefile,attribute){standardGeneric("getValues")})

# '@rdname getValues-methods
# '@aliases getValues,geoknife-method
setMethod(f = "getValues",signature="geoknife",
	definition = function(.Object,shapefile,
		attribute){
		processURL	<-	paste(c(.Object@WFS_URL,'?service=WFS&version=',
			.Object@WFS_DEFAULT_VERSION,'&request=GetFeature',
			'&info_format=text%2Fxml&typename=',shapefile,
			'&propertyname=',attribute),collapse="")
		values	<-	unique(parseXMLvalues(processURL,attribute))
		return(values)
	})
