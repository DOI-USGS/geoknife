setMethod("show", "datagroup", function(object){
  cat('An object of class "datagroup":\n')
  for (i in 1:length(object@group)){
    cat(paste0('[',i,'] '))
    cat(dt@group[[i]]$title,'\n')
    cat('  url:',dt@group[[i]]$url,'\n')
  }
  
})

