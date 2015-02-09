#'@title get shapefiles from a web location
#'
#'@details a \code{geoknife} method for finding shapefile names at a valid WFS endpoint.
#'
#'@param .Object a \code{geoknife} object with a valid WFS url.
#'@return list of shapefiles for the \code{geoknife} WFS url.
#'@docType methods
#'@keywords methods
#'@examples 
#'gk<- geoknife() # create geoknife object
#'getShapefiles(gk) # display shapefile names
#'@import XML
#'@importFrom httr GET
#'@author Jordan S. Read
#'@export
setGeneric(name="getShapefiles",def=function(.Object){standardGeneric("getShapefiles")})

# '@rdname getShapefiles-methods
# '@aliases getShapefiles,geoknife-method	
setMethod(f = "getShapefiles",signature="geoknife",
	definition = function(.Object){
		parentKey <- "featuretypelist"
		childKey <- "featuretype"
		processURL <- sprintf('%s?service=WFS&version=%s&request=GetCapabilities',.Object@WFS_URL, .Object@WFS_DEFAULT_VERSION)
    getCapsDoc <- GET(processURL)
		shapefiles <- parseXMLnodes(getCapsDoc,parentKey,childKey)
		return(shapefiles)
	})