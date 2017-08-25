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

  return(.Object)})

#'@aliases values
#'@rdname values
setMethod(f = "values",signature="webgeom",
          definition = function(.Object){
            return(.Object@values)
          }
)

# knife = NULL for backward compatibility
wfsFilterFeatureXML <- function(.Object, knife=NULL, match.case = TRUE){
  match.case.char <- ifelse(match.case, 'true','false')
  
  wfs_list <- list()
  
  wfs_list["service"] <- "WFS"
  wfs_list["version"] <- version(.Object)
  wfs_list["schema_location"] <- paste(c(.Object@WFS_NAMESPACE,
                                         pkg.env$SCHEMA_LOCATIONS[['WPS_SCHEMA_LOCATION']]),
                                       collapse=" ")
  wfs_list["wfs"] <- .Object@WFS_NAMESPACE
  wfs_list["gml"] <- .Object@GML_NAMESPACE
  wfs_list["ows"] <- pkg.env$NAMESPACES[['ows']]
  wfs_list["ogc"] <- pkg.env$NAMESPACES[['ogc']]
  wfs_list["xsi"] <- pkg.env$NAMESPACES[['xsi']]
  
  wfs_list["query_typename"] <- geom(.Object)
  
  wfs_list["attribute_property"] <- .Object@attribute
  
  properties <- list()
  for (val in values(.Object)){
    properties <- c(properties,
                    list(list(match_case = match.case.char,
                              property_name = .Object@attribute,
                              property_literal = val)))
  }
  
  wfs_list["properties"] <- list(properties)
  
  return(whisker::whisker.render(readLines(system.file(
    "templates/getfeature_template.xml", package = "geoknife")), 
    wfs_list))
}
