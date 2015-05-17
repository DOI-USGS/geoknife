setMethod("show", "webdata", function(object){
  cat('An object of class "webdata":\n')
  cat('times:',as.character(times(object)),'\n')
  cat('url:',url(object),'\n')
  cat('variables:', variables(object))
})