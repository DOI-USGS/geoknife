
#'@title geoknife 
#'@param stencil a \linkS4class{webgeom}, \linkS4class{simplegeom}, or any type 
#'that can be coerced into \linkS4class{simplegeom}.
#'@param fabric a dataset. A \linkS4class{webdata} or any type that 
#'can be coerced into \linkS4class{webdata}
#'@param ... additional arguments passed to \code{new} \linkS4class{webprocess}. 
#'Can also be used to modify the \code{knife} argument, if it is supplied.
#'@param knife (optional) a \linkS4class{webprocess} object
#'@param waitUntilFinished FALSE by default. Should \code{geoknife} check job
#'(and keep R occupied) until it is complete? 
#'@param emailComplete NULL by default. \code{character} of valid email address to 
#'notify for failed or completed process. NOT IMPLEMENTED
#'@return and object of class \linkS4class{geojob}
#'@rdname geoknife-methods
#'@details
#'The \code{stencil} argument is akin to cookie cutter(s), which specify how the dataset is 
#'to be subsampled spatially. Supported types are all geometric in nature, be they collections 
#'of points or polygons. Because geoporcessing operations require a non-zero area for \code{stencil}, 
#'if points are used (i.e., the different point collections that can be used in \linkS4class{simplegeom}), 
#'there is a negligle automatic point buffer applied to each point to result in a non-zero area. 
#'
#'Naming of the components of the \code{stencil} will impact the formatting of the result returned by 
#'the geoknife processing job (the \linkS4class{geojob})
#'
#'geoknife will check the class of the stencil argument, and if stencil's class is not
#'\linkS4class{webgeom}, it will attempt to coerce the object into a \linkS4class{simplegeom}. 
#'If no coercion method exists, geoknife will fail. 
#'
#'The \code{fabric} argument is akin to the dough or fabric that will be subset with the \code{stencil} 
#'argument. At present, this is a web-available gridded dataset that meets a variety of formatting restrictions. 
#'Several quick start methods for creating a \linkS4class{webdata} object (only \linkS4class{webdata} or 
#'an type that can be coerced into \linkS4class{webdata} are valid arguments for \code{fabric}).
#'
#'@docType methods
#'@aliases
#'geoknife
#'@examples
#'job <- geoknife(stencil = c(-89,42), fabric = 'prism')
#'check(job)
#'
#'#-- set up geoknife to email user when the process is complete
#'\dontrun{
#'
#' job <- geoknife(webgeom("state::NH"), fabric = 'prism', emailComplete = 'fake.email@@gmail.com')
#' 
#'}
#'@export
geoknife <- function(stencil, fabric, ..., knife = webprocess(...), waitUntilFinished = FALSE, emailComplete = NULL){
  
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
  
  if (!is.null(emailComplete)) {
    email(geojob, emailComplete, knife)
  }
  
  if (waitUntilFinished){
    waitUntilFinished(geojob)
  }
  return(geojob)
}



# is this used anymore??? seems id(geojob) does this.
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
