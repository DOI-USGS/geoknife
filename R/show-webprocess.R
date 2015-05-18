setMethod("show", "webprocess", function(object){
  cat('An object of class "webprocess":\n')
  cat('url:',url(object),'\n')
  cat('algorithm:', names(algorithm(object)),'>\n')
  cat('version:', version(object),'\n')
  cat('required inputs: {currently in development}', '\n')#object@processInputs
})