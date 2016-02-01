#'@importFrom httr GET verbose
#'@author Jordan S. Read
#'@rdname query
#'@aliases query,webdata-method  
#'@export
setGeneric(name="query",def=function(.Object, field, ...){standardGeneric("query")})


#' query webdata for various fields
#'
#' a method for finding possible values for a given field
#'
#' @param .Object a \code{webdata}, \code{webgeom}, or \code{webprocess} object.
#' @param field a plural parameter name for fields in .Object (e.g., 'variables', 'times')
#' @param ... additional arguments passed to methods
#' @return a character vector of values corresponding to the query field specified
#' @aliases query,webdata-method  
#' @keywords methods
#' @examples
#' fabric <- webdata('prism')
#' query(fabric, 'variables')
#' wg <- webgeom()
#' query(wg, 'geoms')
#' geom(wg) <- "derivative:CONUS_States"
#' query(wg, 'attributes')
#' attribute(wg) <- 'STATE'
#' query(wg, 'values', rm.duplicates = TRUE)
#' @rdname query
#' @aliases query,webdata-method  
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
#'@rdname query
#'@aliases query,webdata-method  
setMethod(f = "query",signature("character",'missing'),
          definition = function(.Object, field, ...){
            field <- match.arg(.Object, c('webdata'))
            values <- do.call(paste0(field,"_query"), list(...))
            return(values)
          }
)

#' @importFrom httr content_type_xml
webdata_query <- function(csw_url = 'https://www.sciencebase.gov/catalog/item/54dd2326e4b08de9379b2fb1/csw'){
  request = '<csw:GetRecords xmlns:csw="http://www.opengis.net/cat/csw/2.0.2" service="CSW" version="2.0.2" resultType="results" outputSchema="http://www.isotc211.org/2005/gmd" maxRecords="1000">
    <csw:Query typeNames="csw:Record">
    <csw:ElementSetName>full</csw:ElementSetName>
    </csw:Query>
    </csw:GetRecords>'
  xpath <- '//srv:containsOperations/srv:SV_OperationMetadata/srv:connectPoint/gmd:CI_OnlineResource/gmd:linkage/gmd:URL'
  parentxpath <- paste0(xpath,paste(rep('/parent::node()[1]',6), collapse='')) #/parent::node()[1]
  namespaces = c('srv','gmd','gco')
 
  response <- gcontent(gPOST(url = csw_url, body = request, content_type_xml()))
  urls <- lapply(getNodeSet(response, xpath, namespaces = namespaces), xmlValue)
  abstracts = sapply(getNodeSet(response, paste0(parentxpath,'/gmd:abstract'), namespaces = namespaces), xmlValue)
  titles = sapply(getNodeSet(response, paste0(parentxpath,'/gmd:citation/gmd:CI_Citation/gmd:title/gco:CharacterString'), namespaces = namespaces), xmlValue)
  group = list()
  sort.ix <- sort(titles, index.return = TRUE)$ix
  
  for (i in 1:length(urls)){
    group[[i]] <- list(title = titles[sort.ix[i]], url=urls[[sort.ix[i]]], abstract = abstracts[sort.ix[i]])
  }
  
  types = unname(sapply(getNodeSet(response, parentxpath, namespaces = namespaces), xmlAttrs))
  
  # removing all non-OPeNDAP endpoints
  group[which(substr(types[sort.ix],1,7) != "OPeNDAP")] <- NULL
  return(datagroup(group))
}