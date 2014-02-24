processInputsToXML	<-	function(.Object){
	
	# private function for rGDP that turns rGDP object into process input xml
	
	top    <-	newXMLNode(name='wps:Execute',attrs=c('service'="WPS",'version'=.Object@WPS_DEFAULT_VERSION,
		'xsi:schemaLocation'=paste(c(.Object@WPS_DEFAULT_NAMESPACE,.Object@WPS_SCHEMA_LOCATION),collapse=" ")),
		namespaceDefinitions=c('wps'=.Object@WPS_DEFAULT_NAMESPACE,'ows'=.Object@OWS_DEFAULT_NAMESPACE,
		'xlink'=.Object@XLINK_NAMESPACE,'xsi'=.Object@XSI_NAMESPACE)) 
	
	
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

	# use WFS or LinearRing?
	if (.Object@feature$LinearRing=='hidden' || is.na(.Object@feature$LinearRing)){
		
		inDatEL  <- newXMLNode('wps:Reference',attrs=c("xlink:href"=.Object@WFS_URL))
		addChildren(inEL,inDatEL)

		bodyEL   <-	newXMLNode('wps:Body')
		addChildren(inDatEL,bodyEL)

		featEL   <-	newXMLNode('wfs:GetFeature',attrs=c("service"="WFS",
			"version"=.Object@WFS_DEFAULT_VERSION,
			"outputFormat"="text/xml; subtype=gml/3.1.1",
			"xsi:schemaLocation"=.Object@XSI_SCHEMA_LOCATION),
			namespaceDefinitions=c("wfs"=.Object@WFS_NAMESPACE,
			"ogc"=.Object@OGC_NAMESPACE,
			"gml"=.Object@GML_NAMESPACE,
			"xsi"=.Object@XSI_NAMESPACE))
		addChildren(bodyEL,featEL)
		queryEL  <-	newXMLNode('wfs:Query',attrs=c("typeName"=as.character(.Object@feature['FEATURE_COLLECTION'])))
		addChildren(featEL,queryEL)
		propNmEL <-	newXMLNode('wfs:PropertyName',newXMLTextNode('the_geom'))
		addChildren(queryEL,propNmEL)
		propNmEL<-	newXMLNode('wfs:PropertyName',newXMLTextNode(.Object@feature['ATTRIBUTE']))
		addChildren(queryEL,propNmEL)
		if (any(names(.Object@feature)=='GML') & !is.na(.Object@feature['GML'])){
			filterEL	<-	newXMLNode('ogc:Filter')
			addChildren(queryEL,filterEL)
			gmlObEL	<-	newXMLNode('ogc:GmlObjectId',attrs=c('gml:id'=.Object@feature['GML']))
			addChildren(filterEL,gmlObEL)
		}
	} else {
		inDatEL	<-	newXMLNode('wps:Data')
		addChildren(inEL,inDatEL)
		# parameterize this...
		compDatEL	<-	newXMLNode('wps:ComplexData',attrs=c("mimeType"="text/xml",#,"encoding"="UTF-8",
			"schema"="http://schemas.opengis.net/gml/3.1.1/base/feature.xsd")) # schema needed?
		addChildren(inDatEL,compDatEL)
		
		gmlFeatEL	<-	newXMLNode('gml:featureMembers',namespaceDefinitions=c('gml'="http://www.opengis.net/gml"),
			attrs=c("xsi:schemaLocation"=paste(c(.Object@DRAW_NAMESPACE,.Object@DRAW_SCHEMA_LOCATION),collapse=' ')))
		addChildren(compDatEL,gmlFeatEL)
		
		gmlBoxEL	<-	newXMLNode('gml:box',attrs=c("gml:id"="box.1"))
		addChildren(gmlFeatEL,gmlBoxEL) 
		
		gmlGeomEL	<-	newXMLNode('gml:the_geom') 
		addChildren(gmlBoxEL,gmlGeomEL)
		
		gmlPolyEL	<-	newXMLNode('gml:MultiPolygon',attrs=c("srsDimension"="2","srsName"="http://www.opengis.net/gml/srs/epsg.xml#4326"))
		addChildren(gmlGeomEL,gmlPolyEL)
		
		gmlPmEL	<-	newXMLNode('gml:polygonMember')
		addChildren(gmlPolyEL,gmlPmEL)
		
		gmlPgEL	<-	newXMLNode('gml:Polygon')
		addChildren(gmlPmEL,gmlPgEL)
		
		gmlExEL	<-	newXMLNode('gml:exterior')
		addChildren(gmlPgEL,gmlExEL)
		
		gmlLrEL	<-	newXMLNode('gml:LinearRing')
		addChildren(gmlExEL,gmlLrEL)
		
		gmlPosEL	<-	newXMLNode('gml:posList',newXMLTextNode(paste(.Object@feature[['LinearRing']],collapse=" ")))
		addChildren(gmlLrEL,gmlPosEL)
	}
		
	resForm	<-	newXMLNode('wps:ResponseForm')
	addChildren(top,resForm)
	
	resDoc	<-	newXMLNode('wps:ResponseDocument',attrs=c('storeExecuteResponse'='true','status'='true'))
	addChildren(resForm,resDoc)
	
	resOut	<-	newXMLNode('wps:Output',attrs=c('asReference'='true'))
	addChildren(resDoc,resOut)
	
	outID	<-	newXMLNode('ows:Identifier',newXMLTextNode('OUTPUT'))
	addChildren(resOut,outID)
	requestXML <-	top
	return(requestXML)
}