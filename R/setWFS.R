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
#'gk <- setWFS(gk,WFS)

#'@export
setGeneric(name="setWFS",def=function(.Object,wfs){standardGeneric("setWFS")})

# '@rdname setWFS-methods
# '@aliases setWFS,geoknife-method
setMethod(f = "setWFS",signature="geoknife",
	definition = function(.Object,wfs){
		wfs	<-	gsub('https', 'http', wfs)
		.Object@WFS_URL	<-	wfs
		return(.Object)
	})