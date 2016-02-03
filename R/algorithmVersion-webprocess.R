#' @importFrom XML xmlAttrs
algorithmVersion <- function(knife){
  getCaps <- gGET(url(knife), query = list(
    'service' = 'WPS', 'version' = version(knife),'request' = 'DescribeProcess', 'identifier'=algorithm(knife)[[1]]))
  doc <- gcontent(getCaps)
  
  version <- xmlAttrs(getNodeSet(doc,'//ProcessDescription')[[1]])[['processVersion']]
  return(version)
}
