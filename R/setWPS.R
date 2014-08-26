#'@title set web processing service location
#'
#'@details method for setting the web processing service (WPS) endpoint for a \code{geoknife} object. 
#'
#'@param .Object a \code{geoknife} object.
#'@param value a Web Processing Service (WPS) endpoint.
#'@return An \code{geoknife} object.
#'@docType methods
#'@keywords setWPS
#'@examples gk <- geoknife() # create geoknife object
#'WPS <- 'http://cida-eros-gdp2qa.er.usgs.gov:8080/gdp-utility-wps/WebProcessingService'
#'setWPS(gk) <- WPS
#'@export
setGeneric(name="setWPS<-",def=function(.Object,value){standardGeneric("setWPS<-")})

# '@rdname setWPS-methods
# '@aliases setWPS,geoknife-method
setReplaceMethod(f = "setWPS",signature="geoknife",
	definition = function(.Object,value){
		wps	<-	gsub('https', 'http', value)
		.Object@WPS_URL	<-	wps
		return(.Object)
	})
