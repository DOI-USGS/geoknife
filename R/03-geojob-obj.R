setClass(
  Class = "geojob",
  representation = representation(
    WFS_URL="character",WPS_URL="character",
    algorithm="list",
    processInputs="list",feature="list",processID="character",
    WPS_DEFAULT_VERSION="character",WFS_DEFAULT_VERSION="character",
    WPS_DEFAULT_NAMESPACE="character",OWS_DEFAULT_NAMESPACE="character",
    WPS_SCHEMA_LOCATION="character",XSI_SCHEMA_LOCATION="character",
    GML_SCHEMA_LOCATION="character",DRAW_SCHEMA_LOCATION="character",
    WFS_NAMESPACE="character",OGC_NAMESPACE="character",
    GML_NAMESPACE="character",DRAW_NAMESPACE="character",
    SMPL_NAMESPACE="character",UPLD_NAMESPACE="character",
    CSW_NAMESPACE="character",XLINK_NAMESPACE="character",
    XSI_NAMESPACE="character",UTILITY_URL="character",
    UPLOAD_URL="character",algorithms="list",
    upload="character",dataList="character",
    timeList="character",emailK="character")
)


setMethod(f="initialize",signature="geojob",
          definition=function(.Object){
            default_WFS = 'http://cida.usgs.gov/gdp/geoserver/wfs'
            default_WPS = 'http://cida.usgs.gov/gdp/process/WebProcessingService'
            default_alg = list()
            default_post = list(empty=NULL)
            default_feat= list(
              FEATURE_COLLECTION=NULL,
              ATTRIBUTE=NULL,
              GML=NA,
              LinearRing=NA)
            # class properties: **PRIVATE**
            .Object@WPS_DEFAULT_VERSION = '1.0.0'
            .Object@WFS_DEFAULT_VERSION = '1.1.0'
            .Object@WPS_DEFAULT_NAMESPACE='http://www.opengis.net/wps/1.0.0'
            .Object@OWS_DEFAULT_NAMESPACE='http://www.opengis.net/ows/1.1'
            # *schema definitions
            .Object@WPS_SCHEMA_LOCATION = 'http://schemas.opengis.net/wps/1.0.0/wpsExecute_request.xsd'
            .Object@XSI_SCHEMA_LOCATION = 'http://www.opengis.net/wfs ../wfs/1.1.0/WFS.xsd'
            .Object@GML_SCHEMA_LOCATION = 'http://schemas.opengis.net/gml/3.1.1/base/feature.xsd'
            .Object@DRAW_SCHEMA_LOCATION = 'http://cida.usgs.gov/climate/derivative/xsd/draw.xsd'
            # *namesspace definitions
            .Object@WFS_NAMESPACE   = 'http://www.opengis.net/wfs'
            .Object@OGC_NAMESPACE   = 'http://www.opengis.net/ogc'
            .Object@GML_NAMESPACE   = 'http://www.opengis.net/gml'
            .Object@DRAW_NAMESPACE  = 'gov.usgs.cida.gdp.draw'
            .Object@SMPL_NAMESPACE  = 'gov.usgs.cida.gdp.sample'
            .Object@UPLD_NAMESPACE  = 'gov.usgs.cida.gdp.upload'
            .Object@CSW_NAMESPACE   = 'http://www.opengis.net/cat/csw/2.0.2'
            .Object@XLINK_NAMESPACE = 'http://www.w3.org/1999/xlink'
            .Object@XSI_NAMESPACE   = 'http://www.w3.org/2001/XMLSchema-instance'
            
            .Object@UTILITY_URL = 'http://cida.usgs.gov/gdp/utility/WebProcessingService'
            .Object@UPLOAD_URL  = 'http://cida.usgs.gov/gdp/geoserver/'
            
            # *list of utilities available to this module
            .Object@upload      = 'gov.usgs.cida.gdp.wps.algorithm.filemanagement.ReceiveFiles'
            .Object@dataList    = 'gov.usgs.cida.gdp.wps.algorithm.discovery.ListOpendapGrids'
            .Object@timeList    = 'gov.usgs.cida.gdp.wps.algorithm.discovery.GetGridTimeRange'
            .Object@emailK      = 'gov.usgs.cida.gdp.wps.algorithm.communication.EmailWhenFinishedAlgorithm'
            
            # public variables (available via print method)	
            .Object@WFS_URL	<-	default_WFS
            .Object@WPS_URL <- default_WPS
            .Object@algorithm	<-	default_alg
            .Object@processInputs	<-	default_post
            .Object@feature	<-	default_feat
            .Object@processID	<-	'<no active job>'
            
            
            return(.Object)
          })

#' create geojob object
#' @description A class representing a geoknife job (\code{geojob}).
#'
#' @return the geojob object
#' @author Jordan S Read
#' @rdname geojob-methods
#' @export
setGeneric("geojob", function(...) {
  standardGeneric("geojob")
})

#'@rdname geojob-methods
#'@aliases geojob,geojob-method
setMethod("geojob", signature(), function(...) {
  ## create new geojob object
  geojob <- new("geojob",...)
  return(geojob)
})


