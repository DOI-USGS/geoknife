#'@importFrom XML newXMLNode
#'@export
#'
setMethod(f = "XML",signature = "webprocess", definition = function(.Object){
  # private function for geoknife that turns geoknife object into process input xml
  
  top    <-	newXMLNode(name='wps:Execute',attrs=c('service'="WPS",'version'=.Object@WPS_VERSION,
                                                  'xsi:schemaLocation'=paste(c(.Object@WPS_NAMESPACE,.Object@WPS_SCHEMA_LOCATION),collapse=" ")),
                       namespaceDefinitions=c('wps'=.Object@WPS_NAMESPACE,'ows'=.Object@OWS_NAMESPACE,
                                              'xlink'=.Object@XLINK_NAMESPACE,'xsi'=.Object@XSI_NAMESPACE))#, 'draw' = .Object@DRAW_NAMESPACE)) 
  
  
  id	<-	newXMLNode("ows:Identifier",newXMLTextNode(.Object@algorithm),parent=top)
  di	<-	newXMLNode("wps:DataInputs",parent=top)
  addChildren(top,c(id,di))
  
  for (i in 1:length(.Object@processInputs)){
    postNm	<-	names(.Object@processInputs[i])
    postVl	<-	.Object@processInputs[postNm]
    if (!is.na(postVl)){
      
      num.vl	<-	length(unlist(postVl))
      for (j in 1:num.vl){
        postVl <- unlist(.Object@processInputs[postNm])[[j]]
        
        if (is.null(postVl)) postVl = 'nine_nine_nine_nine_fail'#stop(postNm, ' cannot be NULL. it is required')
        inEL	<-	newXMLNode("wps:Input",parent=di)
        addChildren(di,inEL)
        
        inIdEL   <- newXMLNode("ows:Identifier",newXMLTextNode(postNm),parent=inEL)
        addChildren(inEL,inIdEL)
        
        inDatEL  <- newXMLNode("wps:Data")
        addChildren(inEL,inDatEL);
        
        litDatEL	<-	newXMLNode('wps:LiteralData',newXMLTextNode(postVl))
        addChildren(inDatEL,litDatEL)
      }
    }
  }
  
  # complex data
  inEL	<-	newXMLNode("wps:Input")
  addChildren(di,inEL)
  
  inIdEL   <- newXMLNode('ows:Identifier',newXMLTextNode('FEATURE_COLLECTION'))
  addChildren(inEL,inIdEL)
  top <- addResponse(.Object, top)
  return(top)
})

addResponse <- function(.Object, xmlNodes){
  top <- xmlNodes
  resForm  <-	newXMLNode('wps:ResponseForm')
  addChildren(top,resForm)
  
  resDoc	<-	newXMLNode('wps:ResponseDocument',attrs=c('storeExecuteResponse'='true','status'='true'))
  addChildren(resForm,resDoc)
  
  #if text/tab-separated-values"
  if (!is.null(.Object@processInputs$DELIMITER) && .Object@processInputs$DELIMITER=="TAB"){
    resOut  <-	newXMLNode('wps:Output',attrs=c('asReference'='true','mimeType'='text/tab-separated-values'))
  } else {
    resOut  <-	newXMLNode('wps:Output',attrs=c('asReference'='true'))
  }
  
  addChildren(resDoc,resOut)
  
  outID	<-	newXMLNode('ows:Identifier',newXMLTextNode('OUTPUT'))
  addChildren(resOut,outID)
  requestXML <-	top
}

addWebgeom <- function(.Object, xmlNodes){
  featureXpath <- '//wps:DataInputs/wps:Input/ows:Identifier'
  dataElementIndx <- which(sapply(getNodeSet(xmlNodes,featureXpath),xmlValue) == "FEATURE_COLLECTION")
  inEL <- getNodeSet(xmlNodes,featureXpath )[[dataElementIndx]]
  inDatEL  <- newXMLNode('wps:Reference',attrs=c("xlink:href"=url(.Object)))
  addChildren(inEL,inDatEL)
  
  bodyEL   <-	newXMLNode('wps:Body')
  addChildren(inDatEL,bodyEL)
  
  featEL   <-	newXMLNode('wfs:GetFeature',attrs=c("service"="WFS",
                                                  "version"=.Object@wfs_version,
                                                  "outputFormat"="text/xml; subtype=gml/3.1.1"),
                         namespaceDefinitions=c("wfs"=.Object@WFS_NAMESPACE,
                                                "gml"=.Object@GML_NAMESPACE))
  addChildren(bodyEL,featEL)
  queryEL  <-	newXMLNode('wfs:Query',attrs=c("typeName"=as.character(.Object@geom)))
  addChildren(featEL,queryEL)
  propNmEL <-	newXMLNode('wfs:PropertyName',newXMLTextNode('the_geom'))
  addChildren(queryEL,propNmEL)
  propNmEL<-	newXMLNode('wfs:PropertyName',newXMLTextNode(.Object@attribute))
  addChildren(queryEL,propNmEL)
  if (!is.na(.Object@IDs)){
    filterEL	<-	newXMLNode('ogc:Filter')
    addChildren(queryEL,filterEL)
    gmlObEL	<-	newXMLNode('ogc:GmlObjectId',attrs=c('gml:id'=.Object@IDs))
    addChildren(filterEL,gmlObEL)
  }
  retun(xmlNodes)
}
addRing <- function(.Object, xmlNodes){
  
  inDatEL  <-	newXMLNode('wps:Data')
  addChildren(inEL,inDatEL)
  # parameterize this...
  compDatEL	<-	newXMLNode('wps:ComplexData',attrs=c("mimeType"="text/xml",#,"encoding"="UTF-8",
                                                    "schema"="http://schemas.opengis.net/gml/3.1.1/base/feature.xsd")) # schema needed?
  addChildren(inDatEL,compDatEL)
  
  gmlFeatEL	<-	newXMLNode('gml:featureMembers',
                          namespaceDefinitions=c(
                            'gml'="http://www.opengis.net/gml"),
                          attrs=c(
                            "xsi:schemaLocation"=paste(c(.Object@DRAW_NAMESPACE,.Object@DRAW_SCHEMA_LOCATION),collapse=' '))
  )
  addChildren(compDatEL,gmlFeatEL)
  # loop this section for multiple polygons:
  
  
  if (is.list(.Object@feature[['LinearRing']])){
    lng	<-	length(.Object@feature[['LinearRing']])
    lst	<-	TRUE
  } else {
    lng	<-	1
    lst	<-	FALSE
  }
  for (j in 1:lng){
    
    gmlBoxEL  <-	newXMLNode('draw:poly',attrs=c("gml:id"=paste("poly.",j,sep='')))
    
    addChildren(gmlFeatEL,gmlBoxEL) 
    
    
    gmlGeomEL  <-	newXMLNode('draw:the_geom')
    
    if (lst){
      ring.val	<-	paste(.Object@feature[['LinearRing']][[j]],collapse=" ")
      drawID <- names(.Object@feature[['LinearRing']])[j]
    } else {
      ring.val	<-	paste(.Object@feature[['LinearRing']],collapse=" ")
      drawID <- paste0('poly.',j)
    }
    
    
    
    drawName  <-	newXMLNode('draw:ID', newXMLTextNode(drawID))
    addChildren(gmlBoxEL,gmlGeomEL, drawName)
    
    gmlPolyEL	<-	newXMLNode('gml:MultiSurface',
                            attrs=c("srsDimension"="2",
                                    "srsName"="EPSG:4326")
    )
    addChildren(gmlGeomEL,gmlPolyEL)
    
    gmlPmEL  <-	newXMLNode('gml:surfaceMember')
    addChildren(gmlPolyEL,gmlPmEL)
    
    gmlPgEL	<-	newXMLNode('gml:Polygon',attrs=c("srsDimension"="2"))
    addChildren(gmlPmEL,gmlPgEL)
    
    gmlExEL	<-	newXMLNode('gml:exterior')
    addChildren(gmlPgEL,gmlExEL)
    
    gmlLrEL	<-	newXMLNode('gml:LinearRing',attrs=c("srsDimension"="2"))
    addChildren(gmlExEL,gmlLrEL)
    
    
    
    gmlPosEL	<-	newXMLNode('gml:posList',newXMLTextNode(ring.val))
    addChildren(gmlLrEL,gmlPosEL)
  }
  
}
#'@export
processInputsToXML	<-	function(.Object){
	
	# private function for geoknife that turns geoknife object into process input xml
	
  top <- XML(.Object) # webprocess to XML
	# use WFS or LinearRing?
	if (.Object@feature$LinearRing=='hidden' || is.na(.Object@feature$LinearRing)){
	  addWebgeom(...)
		
	} else {
	  addRing(...)
		
	}
		
	resForm	<-	newXMLNode('wps:ResponseForm')
	addChildren(top,resForm)
	
	resDoc	<-	newXMLNode('wps:ResponseDocument',attrs=c('storeExecuteResponse'='true','status'='true'))
	addChildren(resForm,resDoc)
	
  #if text/tab-separated-values"
	if (!is.null(.Object@processInputs$DELIMITER) && .Object@processInputs$DELIMITER=="TAB"){
	  resOut  <-	newXMLNode('wps:Output',attrs=c('asReference'='true','mimeType'='text/tab-separated-values'))
	} else {
	  resOut  <-	newXMLNode('wps:Output',attrs=c('asReference'='true'))
	}
	
	addChildren(resDoc,resOut)
	
	outID	<-	newXMLNode('ows:Identifier',newXMLTextNode('OUTPUT'))
	addChildren(resOut,outID)
	requestXML <-	top
	return(requestXML)
}