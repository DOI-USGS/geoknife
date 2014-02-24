#'@title set web processing service location
#'
#'@details method for setting the web processing service (WPS) endpoint for a \code{geoknife} object. 
#'
#'@param \code{geoknife} object.
#'@param a Web Processing Service (WPS) endpoint.
#'@return An \code{geoknife} object.
#'@docType methods
#'@keywords setWPS
#'@examples gk <- geoknife() # create geoknife object
#'WPS <- 'http://cida-eros-gdp2qa.er.usgs.gov/gdp/process/WebProcessingService'
#'gk <- setWPS(gk,WPS)
#'@export
setGeneric(name="setWPS",def=function(.Object,wps){standardGeneric("setWPS")})

# '@rdname setWPS-methods
# '@aliases setWPS,geoknife-method
setMethod(f = "setWPS",signature="geoknife",
	definition = function(.Object,wps){
		wps	<-	gsub('https', 'http', wps)
		.Object@WPS_URL	<-	wps
		return(.Object)
	})
