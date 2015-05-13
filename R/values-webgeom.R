#'@title the values of an object
#'@param .Object a \linkS4class{webgeom} object
#'@param value a values
#'@rdname values
#'@aliases
#'values
#'values<-
#'@export
setGeneric(name="values<-",def=function(.Object, value){standardGeneric("values<-")})

#'@aliases values
#'@rdname values
#'@export
setGeneric(name="values",def=function(.Object){standardGeneric("values")})


#'@aliases values
#'@rdname values
setMethod(f = "values<-",signature(.Object = "webgeom"), definition = function(.Object, value){
  .Object@values <- value
  if(is.na(value)){
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
#' @importFrom httr GET content
#' @keywords internal 
fetchGML_IDs <- function(.Object){
  url <- sprintf('%s?service=WFS&version=%s&request=GetFeature&typename=%s&propertyname=%s',
                 url(.Object), version(.Object), geom(.Object), .Object@attribute)
  ns_geom <- strsplit(geom(.Object), ":")[[1]][1]
  response <- GET(url)
  xml <- content(response)
  value_path <- sprintf('//gml:featureMembers/%s/%s:%s', geom(.Object), ns_geom, .Object@attribute)
  value_names <- sapply(getNodeSet(xml,paste0(value_path, '/node()[1]')), 
                        FUN = function(x) xmlValue(x)[1])
  match_id <- which(value_names == values(.Object)) # CHECK ORDER!!!
  gml_id = getNodeSet(xml,paste0(value_path,'/parent::node()[1]/@gml:id'))[[match_id]][['id']]
  return(gml_id)  
}
