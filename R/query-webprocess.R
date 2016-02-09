#'@rdname query
#'@aliases query,webprocess-method  
setMethod(f = "query",signature("webprocess",'character'),
          definition = function(.Object, field, ...){
            input_list <- list(...)
            if (field == 'algorithms'){
              url <- sprintf('%s?service=WPS&version=%s&request=GetCapabilities',url(.Object), version(.Object))
            } else {
              stop('field ', field, ' not supported.')
            }
            input_list[['xml']] <- gcontent(gGET(url))
            values <- do.call(paste0('parseXML',field), input_list)
            return(values)
          })
