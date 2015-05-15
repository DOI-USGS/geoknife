#'@title query webgeom for various fields
#'
#'@details a \code{webgeom} method for finding shapefile names at a valid WFS endpoint.
#'
#'@param .Object a \code{webgeom} object.
#'@param field a plural parameter name for fields in webgeom (e.g., 'geoms', 'attributes','values')
#'@param ... additional arguments passed to methods (e.g., rm.duplicates = TRUE)
#'@return a character vector of values corresponding to the query field specified
#'@keywords methods
# '@import XML
#'@examples
#'wg <- webgeom()
#'query(wg, 'geoms')
#'geom(wg) <- "derivative:CONUS_States"
#'query(wg, 'attributes')
#'attribute(wg) <- 'STATE'
#'query(wg, 'values', rm.duplicates = TRUE)
#'@importFrom httr GET verbose
#'@author Jordan S. Read
#'@export
setGeneric(name="query",def=function(.Object, field, ...){standardGeneric("query")})

# '@rdname getShapefiles-methods
# '@aliases getShapefiles,geoknife-method  
setMethod(f = "query",signature("webgeom",'character'),
          definition = function(.Object, field, ...){
            input_list <- list(...)
            
            if (field == 'geoms'){
              
              url <- sprintf('%s?service=WFS&version=%s&request=GetCapabilities',url(.Object), version(.Object))
            } else if (field == 'attributes'){
              if (any(is.na(geom(.Object)))){
                stop('cannot query for attribute w/o geom specified.')
              }
              warning('function in development')
              
              url <- sprintf('%s?service=WFS&version=%s&request=DescribeFeatureType&typename=%s',
                             url(.Object), version(.Object), geom(.Object))
            } else if (field == 'values'){
              if (is.na(.Object@attribute)){
                stop('cannot query for value w/o attribute specified.')
              }
              url <- sprintf('%s?service=WFS&version=%s&request=GetFeature&typename=%s&propertyname=%s',
                             url(.Object), version(.Object), geom(.Object), .Object@attribute)
              input_list[['key']] = geom(.Object)
            } else {
              stop('field ', field, ' not supported.')
            }
            input_list[['xml']] <- content(GET(url))
            values <- do.call(paste0('parseXML',field), input_list)
            return(values)
          })