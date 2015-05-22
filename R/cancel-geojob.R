#'@title cancel a geo-web processing request
#'@details cancel is a method for cancelling a geo-web processing request.
#'
#'@param .Object a \linkS4class{geojob} object with an active geo-web processing request.
#'@return A \linkS4class{geojob} object with no active job
#'
#'@docType methods
#'@keywords methods
#'@description Cancel process for \code{geojob}
#'@seealso \code{check}, \code{start}
#'@aliases cancel
#'@docType methods
#'@export
#'@rdname cancel-methods
#'@examples 
#'wd <- webdata('prism')
#'wg <- webgeom('state::New Hampshire')
#'wp <- webprocess()
#'gj <- geojob()
#'xml(gj) <- XML(wg, wd, wp)
#'url(gj) <- url(wp)
#'\dontrun{
#'gj <- start(gj)
#'gj <- cancel(gj)
#'}
setGeneric(name="cancel",def=function(.Object){standardGeneric("cancel")})

#'@rdname cancel-methods
#'@export
setMethod(f = "cancel",signature(.Object = "geojob"),definition = function(.Object){
  setJobState()
  .Object@id <- "<no active job>"
  return(.Object)
})

#'@rdname cancel-methods
#'@export
setMethod(f = "cancel",signature(.Object = "missing"),definition = function(.Object){
  setJobState()
})