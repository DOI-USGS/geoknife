#'@title set web feature service location
#'
#'@details method for setting the web feature service (WFS) endpoint for a \code{geoknife} object. 
#'
#'@param \code{geoknife} object.
#'@param a Web Feature Service (WFS) endpoint.
#'@return An \code{geoknife} object.
#'@docType methods
#'@keywords setWFS
#'@examples gk <- geoknife() # create geoknife object
#'WFS <- 'https://www.sciencebase.gov/catalogMaps/mapping/ows/51b0f5e5e4b030b51983cda1'
#'setWFS(gk) <- WFS
#'@export
setGeneric(name="setWFS<-",def=function(.Object,value){standardGeneric("setWFS<-")})

# '@rdname setWFS-methods
# '@aliases setWFS,geoknife-method
setReplaceMethod(f = "setWFS",signature="geoknife",
	definition = function(.Object,value){
		wfs	<-	gsub('https', 'http', value)
		.Object@WFS_URL	<-	wfs
		return(.Object)
	})