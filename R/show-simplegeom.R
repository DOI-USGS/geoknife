setMethod("show", "simplegeom", function(object){
  cat('An object of class "simplegeom":\n')
  print(object@sp)
})