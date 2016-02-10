#' download output from geojob
#' 
#' download the result of a processing job to a local destination. 
#' 
#'@param .Object a \code{\link{geojob}} that has completed
#'@param destination a file destination. If missing, a temp directory will be used
#'@param ... additional arguments passed to \code{\link[httr]{write_disk}}, such as overwrite = TRUE
#'@return the file handle
#'@author Jordan S Read
#'@rdname download
#'@importFrom httr write_disk progress
#'@export
setGeneric("download", function(.Object, destination, ...) {
  standardGeneric("download")
})

#'@rdname download
#'@aliases download
setMethod("download", signature("geojob",'missing'), function(.Object, destination, ...) {

  status <- check(.Object)
  if (is.null(status)) stop('geojob must be completed for download. See "check(geojob)"')

  filename <- strsplit(status$URL, '?id=')[[1]][2]
  destination = file.path(tempdir(), filename)
  
  download(.Object, destination)
})

#'@rdname download
#'@aliases download
setMethod("download", signature("geojob",'character'), function(.Object, destination, ...) {
  
  status <- check(.Object)
  gGET(url=status$URL, write_disk(destination, ...))
  return(destination)
})
