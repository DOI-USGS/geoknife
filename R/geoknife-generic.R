.onAttach <- function(libname, pkgname) {
  packageStartupMessage("This information is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The information has not received final approval by the U.S. Geological Survey (USGS) and is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the information. Although this software program has been used by the USGS, no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.")
}

missing_msg <- function() stop('stencil and fabric, OR job must be supplied')

#'@title geoknife 
#'@param stencil a \linkS4class{webgeom}, \linkS4class{simplegeom}, or any type 
#'that can be coerced into \linkS4class{simplegeom}.
#'@param fabric a dataset. A \linkS4class{webdata} or any type that 
#'can be coerced into \linkS4class{webdata}
#'@param ... additional arguments passed to \code{new} \linkS4class{webprocess}. 
#'Can also be used to modify the \code{knife} argument, if it is supplied.
#'@param knife (optional) a \linkS4class{webprocess} object
#'@param emailComplete FALSE by default. \code{character} of valid email to 
#'send email for failed or completed process. NOT IMPLEMENTED
#'@return and object of class \linkS4class{geojob}
#'@rdname geoknife-methods
#'@docType methods
#'@aliases
#'geoknife
#'@examples
#'job <- geoknife(stencil = c(-89,42), fabric = 'prism')
#'check(job)
#'@export
geoknife <- function(stencil, fabric, ..., knife = webprocess(...), emailComplete = FALSE){
  
  if (!missing(knife) & !missing(...)){
    # if a knife is specified, pass in additional args through ... to modify. 
    knife <- initialize(knife, ...)
  }

  if (!is(stencil, "webgeom")){
    stencil <- as(stencil, Class = "simplegeom")
  }
  fabric <- as(fabric, Class = "webdata")
  geojob <- geojob()
  xml(geojob) <- XML(stencil, fabric, knife)
  url(geojob) <- url(knife)
  
  geojob <- start(geojob)
  return(geojob)
}



setProcessID	<-	function(.Object,processID){
	.Object@processID	<-	processID
	return(.Object)
}



#'@importFrom httr POST content_type_xml
genericExecute	<-	function(url,requestXML){

	response <-	POST(url,content_type_xml(),
                  body = requestXML)		

	return(response)

}

#'@importFrom XML htmlParse getNodeSet
parseXMLnodes	<-	function(xml,parentKey,childKey,key="name"){
	doc	<-	htmlParse(xml, useInternalNodes = TRUE)
  xpath <- sprintf('//%s/%s',parentKey,childKey)
  if (!is.na(key)){
    xpath = paste0(xpath,'/',key)
  }
  nodes <- getNodeSet(doc, xpath)
	values	<-	sapply(nodes,xmlValue)
	return(values)
}
#'@importFrom XML htmlParse getNodeSet
parseXMLattributes	<-	function(xmlURL,parentKey,childKey,key="name", rm.duplicates = FALSE){
	doc	<-	htmlParse(xmlURL,useInternalNodes = TRUE)
	nodes	<-	getNodeSet(doc,paste(c("//",parentKey,"[@",childKey,"]"),collapse=""))
	# will error if none found
	values	<-	list()
	for (i in 1:length(nodes)){
		values[[i]]	<-	xmlGetAttr(nodes[[i]],key)
	}
	values	<-	unlist(values[values != "the_geom" & values != ""])
  if (rm.duplicates){
    values = unique(values)
  }
	return(values)
}
#'@importFrom XML htmlParse getNodeSet
parseXMLvalues	<-	function(xmlURL,key,  rm.duplicates = FALSE){
	doc	<-	htmlParse(xmlURL,useInternalNodes = TRUE)
	nodes	<-	getNodeSet(doc,paste(c("//",tolower(key)),collapse=""))
	# will error if none found
	values	<-	sapply(nodes,xmlValue)
  if (rm.duplicates){
    values = unique(values)
  }
	return(values)
}
