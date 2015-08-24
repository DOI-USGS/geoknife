setMethod("show", "datagroup", function(object){
  cat('An object of class "datagroup":\n')
  for (i in 1:length(object@group)){
    cat(paste0('[',i,'] '))
    cat(object@group[[i]]$title,'\n')
    cat('  url:',object@group[[i]]$url,'\n')
  }
  
})

