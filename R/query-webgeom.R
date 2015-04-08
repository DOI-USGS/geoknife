#'@title query webgeom for various fields
#'
#'@details a \code{webgeom} method for finding shapefile names at a valid WFS endpoint.
#'
#'@param .Object a \code{webgeom} object.
#'@param field a plural parameter name in webgeom (e.g., 'geoms')
#'@param ... additional arguments passed to GET
#'@return a character vector of values corresponding to the query field specified
#'@keywords methods
# '@import XML
#'@examples
#'wg <- webgeom()
#'query(wg, 'geoms')
#'@importFrom httr GET verbose
#'@author Jordan S. Read
#'@export
setGeneric(name="query",def=function(.Object, field, ...){standardGeneric("query")})

# '@rdname getShapefiles-methods
# '@aliases getShapefiles,geoknife-method  
setMethod(f = "query",signature("webgeom",'character'),
          definition = function(.Object, field, ...){
            if (field == 'geoms'){
              parentKey <- "featuretypelist"
              childKey <- "featuretype"
              url <- sprintf('%s?service=WFS&version=%s&request=GetCapabilities',url(.Object), version(.Object))
            } else if (field == 'attributes'){
              if (any(is.na(c(geom(.Object))))){
                stop('cannot query for attribute w/o geom specified.')
              }
              parentKey  <-	"element"
              childKey	<-	"maxoccurs"
              url <- sprintf('%s?service=WFS&version=%s&request=DescribeFeatureType&typename=%s',
                             url(.Object), version(.Object), geom(.Object))
            } else if (field == 'IDs'){
              url <- sprintf('%s?service=WFS&version=%s&request=GetFeature&typename=%s&propertyname=%s',
                             url(.Object), version(.Object), geom(.Object), .Object@attribute)
            } else {
              stop('field ', field, ' not supported.')
            }
            
            
            getCapsDoc <- GET(url, verbose())
            values <- parseXMLnodes(getCapsDoc,parentKey,childKey, rm.duplicates = TRUE)
            return(values)
          })