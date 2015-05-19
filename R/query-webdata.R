#'@importFrom httr GET verbose
#'@author Jordan S. Read
#'@export
setGeneric(name="query",def=function(.Object, field, ...){standardGeneric("query")})


#'@title query webdata for various fields
#'
#'@details a \code{webdata} method for finding shapefile names at a valid WFS endpoint.
#'
#'@param .Object a \code{webdata} object.
#'@param field a plural parameter name for fields in webdata (e.g., 'variables', 'times')
#'@param ... additional arguments passed to methods, e.g., a \code{webdprocess} object
#'@return a character vector of values corresponding to the query field specified
#'@rdname query-webdata
#'@aliases query,webdata-method  
#'@keywords methods
#'@examples
#'fabric <- webdata('prism')
#'query(fabric, 'variables')
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
setMethod(f = "query",signature("webdata",'missing'),
          definition = function(.Object, field, ...){
            stop('specify a field to query against for webdata object')
          }
)