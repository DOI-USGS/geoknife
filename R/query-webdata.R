#'@importFrom httr GET verbose
#'@author Jordan S. Read
#'@rdname query
#'@aliases query,webdata-method  
#'@export
setGeneric(name="query",def=function(.Object, field, ...){standardGeneric("query")})


#'@title query webdata for various fields
#'
#'@details a method for finding possible values for a given field
#'
#'@param .Object a \code{webdata}, \code{webgeom}, or \code{webprocess} object.
#'@param field a plural parameter name for fields in .Object (e.g., 'variables', 'times')
#'@param ... additional arguments passed to methods
#'@return a character vector of values corresponding to the query field specified
#'@aliases query,webdata-method  
#'@keywords methods
#'@examples
#'fabric <- webdata('prism')
#'query(fabric, 'variables')
#'wg <- webgeom()
#'query(wg, 'geoms')
#'geom(wg) <- "derivative:CONUS_States"
#'query(wg, 'attributes')
#'attribute(wg) <- 'STATE'
#'query(wg, 'values', rm.duplicates = TRUE)
#'@rdname query
#'@aliases query,webdata-method  
setMethod(f = "query",signature("webdata",'character'),
          definition = function(.Object, field, ...){
            field <- match.arg(field, c('variables','times'))
            args <- list(fabric = .Object, ...)
            values <- do.call(paste0(field,"_query"), args)
            return(values)
          }
)
#'@rdname query 
#'@aliases query,webdata-method  
setMethod(f = "query",signature("webdata",'missing'),
          definition = function(.Object, field, ...){
            stop('specify a field to query against for webdata object')
          }
)