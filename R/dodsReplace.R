dodsReplace  <-	function(uri){
  # checks for dods or opendap, and replaces
  if (grepl('dodsC',uri)){
    uri	<-	gsub('http', 'dods', uri)
  }
  if (grepl('opendap',uri)){
    uri	<-	gsub('http', 'opendap', uri)
  }
   
  return(uri)
}