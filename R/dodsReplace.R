#' @importFrom httr HEAD headers
dodsReplace  <-	function(uri){
  # checks for dods or opendap, and replaces
  
  if (substr(uri, 1, 4) == 'dods')
    return(uri)
  
  dods.serve <- httr::headers(httr::HEAD(uri))$`xdods-server`
  if (!is.null(dods.serve)){
    uri	<-	gsub('http', 'dods', uri)
  }
   
  return(uri)
}