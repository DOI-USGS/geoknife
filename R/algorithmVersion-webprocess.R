#' @importFrom XML xmlAttrs
algorithmVersion <- function(knife){
  getCaps <- GET(url(knife), query = list(
    'service' = 'WPS', 'version' = version(knife),'request' = 'DescribeProcess', 'identifier'=algorithm(knife)[[1]]))
  doc	<-	htmlParse(getCaps,isURL=FALSE, useInternalNodes = TRUE)
  
  version <- xmlAttrs(getNodeSet(doc,'//processdescription')[[1]])[['wps:processversion']]
  return(version)
}
