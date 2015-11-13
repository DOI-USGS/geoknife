setMethod("show", "webdata", function(object){
  cat('An object of class "webdata":\n')
  cat(paste0('times: ',
    strftime(times(object)[1] ,format = "%Y-%m-%dT%H:%M:%SZ", tz='UTC'),
    ', ', strftime(times(object)[2] ,format = "%Y-%m-%dT%H:%M:%SZ", tz='UTC'),'\n'))
  cat('url:',url(object),'\n')
  cat('variables:', paste(variables(object), collapse=', '))
})