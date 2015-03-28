# used for generic post document creation
#'@importFrom XML newXMLNode newXMLTextNode addChildren toString.XMLNode xmlDoc
generateRequest  <-	function(.Object, algorithm,cachedResponse='false'){
  
  top    <-	newXMLNode(name='wps:Execute',attrs=c('service'="WPS",'version'=.Object@WPS_DEFAULT_VERSION,
                                                  'xsi:schemaLocation'=paste(c(.Object@WPS_DEFAULT_NAMESPACE,.Object@WPS_SCHEMA_LOCATION),collapse=" ")),
                       namespaceDefinitions=c('wps'=.Object@WPS_DEFAULT_NAMESPACE,'ows'=.Object@OWS_DEFAULT_NAMESPACE,
                                              'xlink'=.Object@XLINK_NAMESPACE,'xsi'=.Object@XSI_NAMESPACE))
  
  id	<-	newXMLNode("ows:Identifier",newXMLTextNode(algorithm),parent=top) #algorithm gov.usgs.cida.gdp.wps.algorithm.discovery.ListOpendapGrids
  di	<-	newXMLNode("wps:DataInputs",parent=top)
  addChildren(top,c(id,di))
  
  wi	<-	newXMLNode("wps:Input",parent=di)
  addChildren(di,c(wi))
  
  oi	<-	newXMLNode("ows:Identifier",newXMLTextNode('catalog-url'),parent=wi)
  wd	<-	newXMLNode("wps:Data",parent=wi)
  addChildren(wi,c(oi,wd))
  
  wld	<-	newXMLNode("wps:LiteralData",newXMLTextNode(.Object@processInputs$DATASET_URI),parent=wd)
  addChildren(wd,wld)
  
  wi	<-	newXMLNode("wps:Input",parent=di)
  addChildren(di,wi)
  
  owi	<-	newXMLNode("ows:Identifier",newXMLTextNode('allow-cached-response'),parent=wi)
  wpd	<-	newXMLNode("wps:Data",parent=wi)
  addChildren(wi,c(owi,wpd))
  
  if (!cachedResponse){
    allowCachedResponse	<-	'false'
  } else {
    allowCachedResponse	<-	'true'
  }
  
  wpLd<-	newXMLNode("wps:LiteralData",newXMLTextNode(allowCachedResponse),parent=wpd)
  addChildren(wpd,wpLd)
  
  wrf	<-	newXMLNode("wps:ResponseForm",parent=top)
  wrd	<-	newXMLNode("wps:RawDataOutput",attrs=c("mimeType"="application/json"), parent=wrf)
  owi	<-	newXMLNode("ows:Identifier",newXMLTextNode("result_as_json"),parent=wrd)
  addChildren(top,wrf)
  requestXML	<-	toString.XMLNode(xmlDoc(top))
  
  return(requestXML)
}

