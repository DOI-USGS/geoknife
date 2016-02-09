#'@rdname variables_query-method
#'@aliases variables_query,webdata-method
#'@export
setGeneric(name="variables_query",def=function(fabric, knife){standardGeneric("variables_query")})

#' variables query
#' 
#' find possible variables in a webdata datset
#' 
#' 
#'@param variables
#'
#'@rdname variables_query-method
#'@aliases variables_query,webdata-method
#'@keywords internal
#'@importFrom XML newXMLNode addChildren toString.XMLNode xmlChildren<- xmlValue<- xmlParseString
#'@export
setMethod(f = "variables_query",signature = c("webdata","missing"), 
          definition = function(fabric, knife){
            knife <- webprocess()
            variables_query(fabric, knife)
          })

#'@rdname variables_query-method
#'@aliases variables_query,webdata-method
#'@keywords internal
#'@importFrom XML newXMLNode addChildren toString.XMLNode xmlChildren<- xmlValue<- xmlParseString
#'@export
setMethod(f = "variables_query",signature = c("webdata","webprocess"), 
          definition = function(fabric, knife){
            
            root <- newXMLNode(name='wps:Execute',
                              attrs=c('service'="WPS",'version'= version(knife),
                                      'xsi:schemaLocation' = paste(c(knife@WPS_NAMESPACE,knife@WPS_SCHEMA_LOCATION),collapse=" ")),
                              namespaceDefinitions=c('wps' = knife@WPS_NAMESPACE,
                                                     'ows' = knife@OWS_NAMESPACE,
                                                     'ogc' = knife@OGC_NAMESPACE,
                                                     'xlink' = knife@XLINK_NAMESPACE,
                                                     'xsi' = knife@XSI_NAMESPACE))
            
            newXMLNode("ows:Identifier",newXMLTextNode(fabric@dataList),parent=root)
            di <- newXMLNode("wps:DataInputs",parent=root)
            wps_in <- newXMLNode("wps:Input",parent=di)
            newXMLNode("ows:Identifier", newXMLTextNode('catalog-url'), parent = wps_in)
            wps_data <- newXMLNode("wps:Data",parent=wps_in)
            newXMLNode("wps:LiteralData", newXMLTextNode(url(fabric)), parent = wps_data)
            
            wps_in <- newXMLNode("wps:Input",parent=di)
            newXMLNode("ows:Identifier", newXMLTextNode('allow-cached-response'), parent = wps_in)
            wps_data <- newXMLNode("wps:Data",parent=wps_in)
            newXMLNode("wps:LiteralData", newXMLTextNode('false'), parent = wps_data)
            
            rf <- newXMLNode("wps:ResponseForm", parent = root)
            rd <- newXMLNode("wps:RawDataOutput", parent = rf)
            newXMLNode("ows:Identifier", newXMLTextNode('result_as_xml'), parent = rd)
            response <- genericExecute(knife@UTILITY_URL,toString.XMLNode(root))
        
            # will error if none found
            values	<-	sapply(getNodeSet(gcontent(response),'//gdp:shortname'),xmlValue)
            return(values)
          })