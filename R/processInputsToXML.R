#'@rdname XML-method
#'@aliases XML,webgeom-method
#'@export
setGeneric(name="XML",def=function(stencil, fabric, knife){standardGeneric("XML")})

#' XML from set of objects
#' 
#' Extract important parts of stencil, fabric, and knife into POST XML
#' 
#'@param stencil a \code{\link{webdata}} OR \code{\link{simplegeom}} object
#'@param fabric a \code{\link{webdata}} object
#'@param knife a \code{\link{webprocess}} object
#'@return XML as ?string?
#'
#'@examples
#'wd <- webdata('prism',times = as.POSIXct(c('2001-01-01','2002-02-05')))
#'wg <- webgeom('state::Wisconsin')
#'\dontrun{
#'XML(wg, wd, webprocess())
#'sg <- simplegeom(c(-89,45))
#'XML(sg, wd, webprocess())
#'}
#'@rdname XML-method
#'@importFrom XML newXMLNode newXMLTextNode addChildren toString.XMLNode xmlChildren<- xmlValue<-
#'@export
setMethod(f = "XML",signature = c("ANY","webdata","webprocess"), 
          definition = function(stencil, fabric, knife){
            #stencil can be webgeom OR simplegeom 
            
  knife <- .setProcessInputs(webprocess = knife, stencil = stencil, fabric = fabric)
  top <- newXMLNode(name='wps:Execute',
                    attrs=c('service'="WPS",'version'= version(knife),
                            'xsi:schemaLocation' = paste(c(knife@WPS_NAMESPACE,knife@WPS_SCHEMA_LOCATION),collapse=" ")),
                    namespaceDefinitions=c('wps' = knife@WPS_NAMESPACE,
                                           'ows' = knife@OWS_NAMESPACE,
                                           'ogc' = knife@OGC_NAMESPACE,
                                           'xlink' = knife@XLINK_NAMESPACE,
                                           'xsi' = knife@XSI_NAMESPACE))
  
  id	<-	newXMLNode("ows:Identifier",newXMLTextNode(knife@algorithm),parent=top)
  di	<-	newXMLNode("wps:DataInputs",parent=top)
  
  for (i in 1:length(knife@processInputs)){
    postNm	<-	names(knife@processInputs[i])
    postVl	<-	knife@processInputs[postNm]
    if (!is.na(postVl)){
      
      num.vl	<-	length(unlist(postVl))
      for (j in 1:num.vl){
        postVl <- unlist(knife@processInputs[postNm])[[j]]
        
        if (is.null(postVl)) stop(postNm, ' cannot be NULL. it is required')
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
  top <- addResponse(knife, top)
  top <- suppressWarnings(addGeom(stencil, xmlNodes = top))
  return(suppressWarnings(toString.XMLNode(top)))
})

addResponse <- function(.Object, xmlNodes){
  top <- xmlNodes
  resForm  <-	newXMLNode('wps:ResponseForm')
  addChildren(top,resForm)
  
  resDoc	<-	newXMLNode('wps:ResponseDocument',attrs=c('storeExecuteResponse'='true','status'='true'))
  addChildren(resForm,resDoc)

  #if text/tab-separated-values" or output_type
  if (!is.null(.Object@processInputs$DELIMITER) && .Object@processInputs$DELIMITER=="TAB"){
    resOut  <-	newXMLNode('wps:Output',attrs=c('asReference'='true','mimeType'='text/tab-separated-values'))
  } else if (!is.null(.Object@processInputs$OUTPUT_TYPE) && .Object@processInputs$OUTPUT_TYPE=="geotiff") {
    resOut  <-  newXMLNode('wps:Output',attrs=c('asReference'='true','mimeType'='application/zip'))
  } else {
    resOut  <-	newXMLNode('wps:Output',attrs=c('asReference'='true'))
  }
  
  addChildren(resDoc,resOut)
  
  outID	<-	newXMLNode('ows:Identifier',newXMLTextNode('OUTPUT'))
  addChildren(resOut,outID)
  requestXML <-	top
}

findIdentifierNode <- function(xmlNodes, name){
  featureXpath <- '//wps:DataInputs/wps:Input/ows:Identifier'
  dataElementIndx <- which(sapply(getNodeSet(xmlNodes,featureXpath),xmlValue) == name)
  geomNode <- getNodeSet(xmlNodes,paste0(featureXpath,'/parent::node()[1]') )[[dataElementIndx]] 
  return(geomNode)
}

setGeneric(name="addGeom",def=function(stencil, xmlNodes){standardGeneric("addGeom")})

#'@importFrom XML newXMLNode addChildren
setMethod(f = "addGeom",signature = c("webgeom","ANY"), 
          definition = function(stencil, xmlNodes){
            

  inEL <- findIdentifierNode(xmlNodes, 'FEATURE_COLLECTION')
  # reference is a sibling of the FEATURE_COLLECTION ID
  inDatEL  <- newXMLNode('wps:Reference',attrs=c("xlink:href"=url(stencil)))
  addChildren(inEL,inDatEL) # see if we can do this as a method to webgeom
  
  bodyEL   <-	newXMLNode('wps:Body')
  addChildren(inDatEL,bodyEL)
  
  featEL   <-	newXMLNode('wfs:GetFeature',attrs=c("service"="WFS",
                                                  "version"=version(stencil),
                                                  "outputFormat"="text/xml; subtype=gml/3.1.1"),
                         namespaceDefinitions=c("wfs"=stencil@WFS_NAMESPACE,
                                                "gml"=stencil@GML_NAMESPACE))
  addChildren(bodyEL,featEL)
  queryEL  <-	newXMLNode('wfs:Query',attrs=c("typeName"=as.character(stencil@geom)))
  addChildren(featEL,queryEL)
  propNmEL <-	newXMLNode('wfs:PropertyName',newXMLTextNode('the_geom'))
  addChildren(queryEL,propNmEL)
  propNmEL<-	newXMLNode('wfs:PropertyName',newXMLTextNode(stencil@attribute))
  addChildren(queryEL,propNmEL)
  if (!is.na(stencil@GML_IDs[1])){
    filterEL	<-	newXMLNode('ogc:Filter')
    addChildren(queryEL,filterEL)
    for (i in 1:length(stencil@GML_IDs)){
      newXMLNode('ogc:GmlObjectId',attrs=c('gml:id'=stencil@GML_IDs[i]), parent = filterEL)  
    }
  }
  
  return(xmlNodes)
})

setMethod(f = "addGeom",signature = c("ANY","ANY"),
          definition = function(stencil, xmlNodes){
            stencil <- simplegeom(stencil)
            return(addGeom(stencil, xmlNodes))
})

#'@importFrom sp coordinates
setMethod(f = "addGeom",signature = c("simplegeom","ANY"),
          definition = function(stencil, xmlNodes){
            
  filterID <- .FEATURE_ATTRIBUTE_NAME(stencil)
  inEL <- findIdentifierNode(xmlNodes, 'FEATURE_COLLECTION')
  inDatEL  <-	newXMLNode('wps:Data')
  addChildren(inEL,inDatEL)
  # parameterize this...
  compDatEL	<-	newXMLNode('wps:ComplexData',
                          attrs=c("mimeType"="text/xml",#,"encoding"="UTF-8",
                                  "schema"="http://schemas.opengis.net/gml/3.1.1/base/feature.xsd")) # schema needed?
  addChildren(inDatEL,compDatEL)
  
  gmlFeatEL <- newXMLNode('gml:featureMembers',
                          namespaceDefinitions=c(
                            'gml'="http://www.opengis.net/gml",
                            'draw'= stencil@DRAW_NAMESPACE),
                          attrs=c(
                            "xsi:schemaLocation"=paste(c(stencil@DRAW_NAMESPACE,stencil@DRAW_SCHEMA),collapse=' '))
  )
  addChildren(compDatEL,gmlFeatEL)
  # loop this section for multiple polygons:
  
  geom <- stencil@sp
  
  for (j in seq_along(geom)){
    
    gmlBoxEL  <-	newXMLNode('draw:poly',attrs=c("gml:id"=sprintf("poly.%s",j)))
    
    addChildren(gmlFeatEL,gmlBoxEL) 
    gmlGeomEL  <-	newXMLNode('draw:the_geom')
    
    ringCoords <- geom@polygons[[j]]@Polygons[[1]]@coords
    
    ring = vector(length = 2*nrow(ringCoords))
    ring[seq(from = 1,by = 2,length.out = nrow(ringCoords))] <- ringCoords[,2]
    ring[seq(from = 2,by = 2,length.out = nrow(ringCoords))] <- ringCoords[,1]
    ring.val	<-	paste(ring,collapse = ' ')
    drawID <- geom@polygons[[j]]@ID

    drawName  <-	newXMLNode(paste0('draw:',filterID), newXMLTextNode(drawID))
    addChildren(gmlBoxEL,gmlGeomEL, drawName)
    
    gmlPolyEL	<-	newXMLNode('gml:MultiSurface',
                            attrs=c("srsDimension"="2",
                                    "srsName"="urn:x-ogc:def:crs:EPSG:4326") # FROM CRS in the future!!!
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
  return(xmlNodes)
})