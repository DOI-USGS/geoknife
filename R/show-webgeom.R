setMethod("show", "webgeom", function(object){
  cat('An object of class "webgeom":\n')
  cat('url:',url(object),'\n')
  cat('geom:',geom(object),'\n')
  cat('attribute:',object@attribute,'\n')
  cat('values:', paste(values(object), collapse=', '),'\n')
  cat('wfs version:', version(object))
})