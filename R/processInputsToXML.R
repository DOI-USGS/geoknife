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
  
  browser()
  
  whisker_list <- list(identifier = knife@algorithm)
  
  input_list <- list() # unnamed list for {{#inputs}}
  
  for (i in 1:length(knife@processInputs)){
    input_identifier	<-	names(knife@processInputs[i])
    input_literal_data <- knife@processInputs[input_identifier]
    
    if (!is.na(input_literal_data)){
      
      data_element_list <- list() # unnamed list for {{#data_elements}}
      
      for (j in 1:length(unlist(input_literal_data))){
        input_literal_data_element <- unlist(knife@processInputs[input_identifier])[[j]]
        
        if (is.null(input_literal_data_element)) stop(input_identifier, ' cannot be NULL. it is required')
        
        data_element_list <- c(data_element_list,
                               list(list(input_literal_data_element = input_literal_data_element)))
      }
    }
    
    input_list <- c(input_list,
                    list(list(input_identifier = input_identifier,
                              data_elements = data_element_list)))
    
  }
  
  whisker_list["inputs"] <- list(input_list)
  
  return(whisker::whisker.render(readLines(system.file("extdata/execute_template.xml", package = "geoknife")), whisker_list))
  
  # # complex data
  # inEL	<-	newXMLNode("wps:Input")
  # addChildren(di,inEL)
  # 
  # inIdEL   <- newXMLNode('ows:Identifier',newXMLTextNode('FEATURE_COLLECTION'))
  # addChildren(inEL,inIdEL)
  # top <- addResponse(knife, top)
  # top <- suppressWarnings(addGeom(stencil, xmlNodes = top))
  # return(suppressWarnings(toString.XMLNode(top)))
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
  # We are passing XML package objects around while reading/writing so keeping 
  # the readers that depend on a parsed object for now.
  dataElementIndx <- which(sapply(XML::getNodeSet(xmlNodes,featureXpath),XML::xmlValue) == name)
  geomNode <- XML::getNodeSet(xmlNodes,paste0(featureXpath,'/parent::node()[1]') )[[dataElementIndx]] 
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