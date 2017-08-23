
#'@rdname query
#'@aliases query,webgeom-method  
setMethod(f = "query",signature("webgeom",'character'),
          definition = function(.Object, field, ...){
            input_list <- list(...)
            
            if (field == 'geoms'){
              
              url <- sprintf('%s?service=WFS&version=%s&request=GetCapabilities',url(.Object), version(.Object))
            } else if (field == 'attributes'){
              if (any(is.na(geom(.Object)))){
                stop('cannot query for attribute w/o geom specified.')
              }
              
              url <- sprintf('%s?service=WFS&version=%s&request=DescribeFeatureType&typename=%s',
                             url(.Object), version(.Object), geom(.Object))
            } else if (field == 'values'){
              if (is.na(.Object@attribute)){
                stop('cannot query for value w/o attribute specified.')
              }
              url <- sprintf('%s?service=WFS&version=%s&request=GetFeature&typename=%s&propertyname=%s',
                             url(.Object), version(.Object), geom(.Object), .Object@attribute)
              input_list[['key']] <- sprintf("%s/*[local-name()='%s']", geom(.Object), .Object@attribute)
            } else {
              stop('field ', field, ' not supported.')
            }
            # Note sure what this is doing. Need to investigate.
            input_list[['xml']] <- gcontent(gGET(url))
            values <- unique(do.call(paste0('parseXML',field), input_list))
            return(values)
          })