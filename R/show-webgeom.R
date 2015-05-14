setMethod("show", "webgeom", function(object){
  cat('An object of class "webgeom":\n')
  cat('geom:',geom(object),'\n')
  cat('attribute:',object@attribute,'\n')
  cat('values:', values(object),'\n')
  cat('version:', version(object))
})