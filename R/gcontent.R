#' drop in replacement for httr switching to xml2 from XML
#' 
#' reverts to old parsing pre v1.1.0 for httr
#' 
#' @param response the result of httr::GET(url)
#' @keywords internal
#' @importFrom XML xmlParse
gcontent <- function(response){
  XML::xmlParse(iconv(readBin(response$content, character()), from = "UTF-8", to = "UTF-8"))
}
