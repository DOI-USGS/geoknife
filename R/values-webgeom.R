#' the values of a webgeom
#' 
#' The values of a webgeom are the values of the attributes used in the geometries. 
#' For example, if the webgeom's "geom" field is a feature collection containing states 
#' and counties, and the "attributes" are the states, then the values would be the 
#' specific states. 
#' 
#' @param .Object a \linkS4class{webgeom} object
#' @param value a values
#' @rdname values
#'
#' @examples 
#' wg <- webgeom('state::Wisconsin')
#' values(wg)
#' values(wg) <- c('Wisconsin','New Hampshire')
#' @aliases
#' values
#' values<-
#' @export
setGeneric(name="values<-",def=function(.Object, value){standardGeneric("values<-")})

#' @aliases values
#' @rdname values
#' @export
setGeneric(name="values",def=function(.Object){standardGeneric("values")})


#'@aliases values
#'@rdname values
setMethod(f = "values<-",signature(.Object = "webgeom"), definition = function(.Object, value){
  .Object@values <- as.character(value)
  if(is.na(value[1])){
    .Object@GML_IDs <- as.character(NA)
  } else {
    .Object@GML_IDs <- fetchGML_IDs(.Object)
  }
  
  return(.Object)})

#'@aliases values
#'@rdname values
setMethod(f = "values",signature="webgeom",
          definition = function(.Object){
            return(.Object@values)
          }
)

#' @title fetch GML_IDs from WFS
#' @description fetch GML_IDs from WFS when geom, attribute, and values are specified
#' @param .Object a webgeom object
#' @keywords internal 
fetchGML_IDs <- function(.Object){
  url <- sprintf('%s?service=WFS&version=%s&request=GetFeature&typename=%s&MAXFEATURES=10000&propertyname=%s',
                 url(.Object), version(.Object), geom(.Object), .Object@attribute)
  ns_geom <- strsplit(geom(.Object), ":")[[1]][1]
  response <- gGET(url=url)
  xml <- gcontent(response)
  value_path <- sprintf('//gml:featureMembers/%s/%s:%s', geom(.Object), ns_geom, .Object@attribute)
  node_sets <- getNodeSet(xml, paste0(value_path,'/parent::node()'))
  node_df <- do.call(rbind, lapply(node_sets, function(x) data.frame(
    value=xmlValue(x), id=xmlAttrs(x)[['id']], stringsAsFactors=FALSE)))
  gml_id <- node_df[node_df$value %in% values(.Object), 'id']
  return(gml_id)  
}
