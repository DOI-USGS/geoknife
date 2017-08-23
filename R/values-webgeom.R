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

wfsFilterFeatureXML <- function(.Object, knife=webprocess(), match.case = TRUE){
  match.case.char <- ifelse(match.case, 'true','false')
  top <- newXMLNode(name='wfs:GetFeature',
                    attrs=c('service'="WFS",'version'= version(.Object),
                            'xsi:schemaLocation' = paste(c(.Object@WFS_NAMESPACE,knife@WPS_SCHEMA_LOCATION),collapse=" ")),
                    namespaceDefinitions=c('ogc' = knife@OGC_NAMESPACE,
                                           'wfs' = .Object@WFS_NAMESPACE,
                                           'xsi' = knife@XSI_NAMESPACE,
                                           'gml' = .Object@GML_NAMESPACE,
                                           'ows' = knife@OWS_NAMESPACE))
  q <- newXMLNode('wfs:Query', parent = top, attrs = c(typeName=geom(.Object)))
  newXMLNode('ogc:PropertyName', parent = q, newXMLTextNode(.Object@attribute))
  f <- newXMLNode('ogc:Filter', parent = q) # skipping namespace
  Or <- newXMLNode('ogc:Or', parent = f) 
  for (val in values(.Object)){
    newXMLNode('ogc:PropertyIsEqualTo', parent=Or, attrs = c('matchCase'=match.case.char), 
               .children = list(
                 newXMLNode('ogc:PropertyName', newXMLTextNode(.Object@attribute)),
                 newXMLNode('ogc:Literal', newXMLTextNode(val))
               ))
  }
  
  return(suppressWarnings(toString.XMLNode(top)))
}

#' @title fetch GML_IDs from WFS
#' @description fetch GML_IDs from WFS when geom, attribute, and values are specified
#' @param .Object a webgeom object
#' @keywords internal 
fetchGML_IDs <- function(.Object){
  response <- suppressWarnings(gPOST(url=url(.Object), body=wfsFilterFeatureXML(.Object)))
  xml <- gcontent_xml2(response)
  ns_geom <- strsplit(geom(.Object), ":")[[1]][1]
  value_path <- sprintf('//gml:featureMembers/%s/%s:%s', geom(.Object), ns_geom, .Object@attribute)
  node_sets <- xml2::xml_find_all(xml, paste0(value_path,'/parent::node()'))
  gml_id <- unname(unlist(lapply(node_sets, function(x) return(xml2::xml_attrs(x)['id']))))
  return(gml_id)  
}
