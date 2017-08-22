#' @import xml2
algorithmVersion <- function(knife){
  getCaps <- gGET(url(knife), query = list(
    'service' = 'WPS', 'version' = version(knife),'request' = 'DescribeProcess', 'identifier'=algorithm(knife)[[1]]))
  doc <- gcontent_xml2(getCaps)
  version <- xml_attrs(xml_find_all(doc,'//ProcessDescription', 
                                 ns = pkg.env$NAMESPACES)[[1]])[['processVersion']]
  return(version)
}
