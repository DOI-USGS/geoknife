.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Although this software program has been used by the U.S. Geological Survey (USGS), no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.")
}

#'@title geoknife-fcn
#'@param stencil a geometric feature (the "cookie-cutter"). Many input types are supported. [list them]
#'@param fabric a dataset. Can be \code{\link{webdata}} or recognized shortname (e.g., 'prism')
#'@param knife (optional) a \code{\link{webprocessing}} object
#'@param job (optional) a \code{\link{geojob}} object
#'@param ... additional arguments passed on to process. 
#'@rdname geoknife-function
#'@export
setGeneric(name="geoknife",def=function(stencil, fabric, knife, job, ...){standardGeneric("geoknife")})

setMethod("geoknife", signature = c("webgeom", "webdata", "webprocess","missing"), 
          definition = function(stencil, fabric, knife, job, ...) {
            cat('all objects are good\n')
            return('!geojob!')
          }
)

setMethod("geoknife", signature = c("ANY", "ANY", "webprocess","missing"), 
          definition = function(stencil, fabric, knife, job, ...) {
            cat('basic, all items can be butchered into objects\n')
            webgeom <- butcher(stencil, webgeom())
            webdata <- butcher(fabric, webdata())
            geojob <- geoknife(webgeom, webdata, knife, ...)
            return(geojob)
          }
)

setMethod("geoknife", signature = c("ANY", "ANY", "geojob","missing"), 
          definition = function(stencil, fabric, knife, job, ...) {
            cat('basic, all items can be butchered into objects\n')
            webgeom <- butcher(stencil, webgeom())
            webdata <- butcher(fabric, webdata())
            webprocess <- butcher(knife, webprocess())
            geojob <- geoknife(webgeom, webdata, webprocess, ...)
            return(geojob)
          }
)

setMethod("geoknife", signature = c("ANY", "ANY", "missing", "missing"), 
          definition = function(stencil, fabric, knife, job, ...) {
            webgeom <- butcher(stencil, webgeom())
            webdata <- butcher(fabric, webdata())
            geojob <- geoknife(webgeom, webdata, webprocess(), ...)
            return(geojob)
            cat('basic, all objects can be parsed. missing webprocess\n')
          }
)

setMethod("geoknife", signature = c("missing", "missing", "missing", "geojob"), 
          definition = function(stencil, fabric, knife, job, ...) {
            cat('basic, all objects will come from geojob\n')
            webgeom <- butcher(job, webgeom())
            webdata <- butcher(job, webdata())
            webprocess <- butcher(job, webprocess())
            geojob <- geoknife(webgeom, webdata, webprocess, ...)
            return(geojob)
          }
)

setMethod("geoknife", signature = c("ANY", "missing", "missing", "missing"), 
          definition = function(stencil, fabric, knife, job, ...) missing_msg()
)
setMethod("geoknife", signature = c("missing", "ANY", "missing", "missing"), 
          definition = function(stencil, fabric, knife, job, ...) missing_msg()
)
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










