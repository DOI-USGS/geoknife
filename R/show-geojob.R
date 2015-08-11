setMethod("show", "geojob", function(object){
  cat('An object of class "geojob":\n')
  cat('url:',url(object),'\n')
  cat('xml:\n', xml(object),'\n')
  cat(getPackageName(), 'version:', as.character(object@package.version),'\n')
  cat('algorithm version:', as.character(object@algorithm.version),'\n')
  cat('id:', id(object))
})