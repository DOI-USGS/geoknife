#'@importClassesFrom sp SpatialPointsDataFrame SpatialPolygonsDataFrame
setUrl <- function(.Object, values){
  if (length(values) != 1){
    stop('url must be a single character string')
  }
  .Object@url <- values

  return(.Object)
}

setClass(
  Class = "webgeom",
  representation = representation(
    url = "character",
    geom = "character",
    attribute = "character",
    IDs = "character",
    wfs_version = "character",
    WFS_NAMESPACE = "character",
    GML_NAMESPACE = "character",
    GML_SCHEMA_LOCATION = "character")
)


setMethod("initialize", signature = "webgeom", 
          function(
            .Object,
            url, geom = as.character(NA), 
            attribute = as.character(NA), IDs = as.character(NA),wfs_version)
            {
              
            if (missing(url)){
              url = "http://cida.usgs.gov/gdp/geoserver/wfs"
            }
            
            if (missing(wfs_version)){
              wfs_version = '1.1.0'
            }
            
            .Object@url= url
            .Object@geom = geom
            .Object@attribute = attribute
            .Object@IDs = IDs
            .Object@wfs_version = wfs_version
            .Object@GML_NAMESPACE = 'http://www.opengis.net/gml'
            .Object@WFS_NAMESPACE   = 'http://www.opengis.net/wfs'
            .Object@GML_SCHEMA_LOCATION = 'http://schemas.opengis.net/gml/3.1.1/base/feature.xsd'
            return(.Object)
          })

#' create webgeom object
#' @description A class representing a web dataset.
#'
#' @slot url value of type \code{"character"}, the web location for the dataset
#' @slot variable value of type \code{"character"}, the variable(s) for data
#'
#' @return the webgeom object representing a dataset and parameters
#' @author Jordan S Read
#' @rdname webgeom-methods
#' @export

setGeneric("webgeom", function(...) {
  standardGeneric("webgeom")
})

#'@rdname webgeom-methods
#'@aliases url<-,webgeom-method
#'@export
setGeneric(name="url<-",def=function(.Object, values){standardGeneric("url<-")})

setMethod(f = "url<-",signature = "webgeom", definition = function(.Object, values){
  setURL(.Object, values)}
  )


#' @rdname webgeom-methods
#' @aliases webgeom,webgeom-method
setMethod("webgeom", signature(), function(...) {
  ## create new webgeom object
  webgeom <- new("webgeom",...)
  return(webgeom)
})



#'@rdname webgeom-methods
#'@aliases XML,webgeom-method
#'@export
setGeneric(name="XML",def=function(.Object){standardGeneric("XML")})

setMethod(f = "XML",signature = "webgeom", definition = function(.Object){
  return(xmlTreeParse('<wps:Reference xlink:href="http://cida.usgs.gov/gdp/geoserver/wfs">
<wps:Body>
<wfs:GetFeature service="WFS" version="1.1.0" outputFormat="text/xml; subtype=gml/3.1.1" xmlns:wfs="http://www.opengis.net/wfs" xmlns:ogc="http://www.opengis.net/ogc" xmlns:gml="http://www.opengis.net/gml" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.opengis.net/wfs ../wfs/1.1.0/WFS.xsd">
<wfs:Query typeName="draw:junker_do_1">
<wfs:PropertyName>the_geom</wfs:PropertyName>
<wfs:PropertyName>ID</wfs:PropertyName>
</wfs:Query>
</wfs:GetFeature>
</wps:Body>
</wps:Reference>'))
  }
)

#'@rdname webdata-methods
#'@aliases url<-,webdata-method
#'@export
setGeneric(name="url<-",def=function(.Object, values){standardGeneric("url<-")})

setMethod(f = "url<-",signature = "webdata", definition = function(.Object, values){
  setURL(.Object, values)}
)
#'@rdname webgeom-methods
#'@aliases url<-,webgeom-method
setMethod(f = "url<-",signature = "webgeom", definition = function(.Object, values){
  setURL(.Object, values)}
)

#'@rdname webdata-methods
#'@aliases url,webdata-method
#'@export
setGeneric(name="url",def=function(.Object){standardGeneric("url")})

setMethod(f = "url",signature = "webdata",
          definition = function(.Object){
            return(.Object@url)
          })
setMethod(f = "url",signature = "webgeom",
          definition = function(.Object){
            return(.Object@url)
          })


