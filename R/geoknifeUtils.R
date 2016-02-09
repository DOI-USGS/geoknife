#' @importFrom curl curl_version
#' @importFrom utils packageVersion
geoknifeUserAgent <- function() {
  versions <- c(
    libcurl = curl::curl_version()$version,
    `r-curl` = as.character(utils::packageVersion("curl")),
    httr = as.character(utils::packageVersion("httr")),
    geoknife = as.character(utils::packageVersion("geoknife"))
  )
  c(`User-Agent`=paste0(names(versions), "/", versions, collapse = " "))
}
#'@importFrom httr verbose
gverbose <- function(){
  if (gconfig('verbose'))
    httr::verbose()
  else 
    NULL
}

#'@importFrom httr GET add_headers verbose
gGET <- function(url, ...){
  # I don't like doing this, but the GDP response comes back as `content-type`="text/xml" instead of 
  # `content-type`="text/xml; charset=UTF-8" so we get a charset warning message
  
  suppressWarnings(httr::GET(url=url, gverbose(), ..., add_headers(geoknifeUserAgent())))
  
}

#'@importFrom httr POST add_headers
gPOST <- function(url, config = list(), ...){
  suppressWarnings(httr::POST(url=url, config=config, gverbose(), ..., add_headers(geoknifeUserAgent())))
}

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
