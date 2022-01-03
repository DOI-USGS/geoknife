setMethod("show", "geojob", function(object){
  cat('An object of class "geojob":\n')
  cat('url:',url(object),'\n')
  cat('xml:\n', xml(object),'\n')
  cat(getPackageName(), 'version:', object@package.version,'\n')
  cat('algorithm version:', object@algorithm.version,'\n')
  cat('id:', id(object))
})

setMethod("show", "datagroup", function(object){
  cat('An object of class "datagroup":\n')
  for (i in 1:length(object@group)){
    cat(paste0('[',i,'] '))
    cat(object@group[[i]]$title,'\n')
    cat('  url:',object@group[[i]]$url,'\n')
  }
  
})

setMethod("show", "webdata", function(object){
  cat('An object of class "webdata":\n')
  cat(paste0('times: ',
             strftime(times(object)[1] ,format = "%Y-%m-%dT%H:%M:%SZ", tz='UTC'),
             ', ', strftime(times(object)[2] ,format = "%Y-%m-%dT%H:%M:%SZ", tz='UTC'),'\n'))
  cat('url:',url(object),'\n')
  cat('variables:', paste(variables(object), collapse=', '))
})

setMethod("show", "webgeom", function(object){
  cat('An object of class "webgeom":\n')
  cat('url:',url(object),'\n')
  cat('geom:',geom(object),'\n')
  cat('attribute:',object@attribute,'\n')
  cat('values:', paste(values(object), collapse=', '),'\n')
  cat('wfs version:', version(object))
})

setMethod("show", "webprocess", function(object){
  cat('An object of class "webprocess":\n')
  cat('url:',url(object),'\n')
  cat('algorithm:', names(algorithm(object)),'\n')
  cat('web processing service version:', version(object),'\n')
  cat('process inputs: \n')
  inputs <- inputs(object)
  inputs <- inputs[!paste0('.',names(inputs)) %in% pkg.env$private.funs]
  for (i in 1:length(inputs)){
    cat(paste0('   ',names(inputs)[i],": ",inputs[[i]], '\n'))
  }
  cat('wait:', object@wait, '\n')
  cat('email:', object@email, '\n' )
})