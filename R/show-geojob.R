setMethod("show", "geojob", function(object){
  cat('An object of class "geojob":\n')
  cat('url:',url(object),'\n')
  cat('xml:\n', xml(object),'\n')
  cat('geoknife version:', as.character(version(object)),'\n')
  cat('id:', id(object))
})