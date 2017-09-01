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
    gmlID <- fetchGML_IDs(.Object)
    if(is.null(gmlID)){
      stop('fetchGML_IDs returned a NULL; the value name you supplied is likely invalid for this feature',
            call. = FALSE)
    }
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

#' @title fetch GML_IDs from WFS
#' @description fetch GML_IDs from WFS when geom, attribute, and values are specified
#' @param .Object a webgeom object
#' @keywords internal 
fetchGML_IDs <- function(.Object){
  response <- suppressWarnings(gPOST(url=url(.Object), body=wfsFilterFeatureXML(.Object)))
  xml <- gcontent(response)
  ns_geom <- strsplit(geom(.Object), ":")[[1]][1]
  value_path <- sprintf('//gml:featureMembers/%s/%s:%s', geom(.Object), ns_geom, .Object@attribute)
  node_sets <- xml2::xml_find_all(xml, paste0(value_path,'/parent::node()'))
  gml_id <- unname(unlist(lapply(node_sets, function(x) return(xml2::xml_attrs(x)['id']))))
  return(gml_id)  
}
