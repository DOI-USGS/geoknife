setMethod("show", "webprocess", function(object){
  cat('An object of class "webprocess":\n')
  cat('url:',url(object),'\n')
  cat('algorithm:', names(algorithm(object)),'\n')
  cat('version:', version(object),'\n')
  cat('process inputs: \n')
  inputs <- inputs(object)
  for (i in 1:length(inputs)){
    cat(paste0('   ',names(inputs)[i],": ",inputs[[i]], '\n'))
  }
  cat('wait:', object@wait, '\n')
  cat('email:', object@email, '\n' )
})