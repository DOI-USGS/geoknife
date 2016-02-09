#'@rdname times_query-method
#'@aliases times_query,webdata-method
#'@export
setGeneric(name="times_query",def=function(fabric, knife){standardGeneric("times_query")})

#' times query
#' 
#' query a webdata object for the time range
#' 
#' @rdname times_query-method
#' @aliases times_query,webdata-method
#' @keywords internal
#' @importFrom XML newXMLNode addChildren toString.XMLNode xmlChildren<- xmlValue<- xmlParseString
#' @export
setMethod(f = "times_query",signature = c("webdata","missing"), 
          definition = function(fabric, knife){
            knife <- webprocess()
            times_query(fabric, knife)
          })

#'@rdname times_query-method
#'@aliases times_query,webdata-method
#'@keywords internal
#'@importFrom XML newXMLNode addChildren toString.XMLNode xmlChildren<- xmlValue<- xmlParseString
#'@export
setMethod(f = "times_query",signature = c("webdata","webprocess"), 
          definition = function(fabric, knife){
            
            if (is.na(variables(fabric)[1])) stop('variables cannot be NA for fabric argument')
            if (length(variables(fabric)) > 1) warning('variables is > 1, using ', variables(fabric)[1], ' only')    
            
            root <- newXMLNode(name='wps:Execute',
                               attrs=c('service'="WPS",'version'= version(knife),
                                       'xsi:schemaLocation' = paste(c(knife@WPS_NAMESPACE,knife@WPS_SCHEMA_LOCATION),collapse=" ")),
                               namespaceDefinitions=c('wps' = knife@WPS_NAMESPACE,
                                                      'ows' = knife@OWS_NAMESPACE,
                                                      'ogc' = knife@OGC_NAMESPACE,
                                                      'xlink' = knife@XLINK_NAMESPACE,
                                                      'xsi' = knife@XSI_NAMESPACE))
            
            newXMLNode("ows:Identifier",newXMLTextNode(fabric@timeList ),parent=root)
            di <- newXMLNode("wps:DataInputs",parent=root)
            wps_in <- newXMLNode("wps:Input",parent=di)
            newXMLNode("ows:Identifier", newXMLTextNode('catalog-url'), parent = wps_in)
            wps_data <- newXMLNode("wps:Data",parent=wps_in)
            newXMLNode("wps:LiteralData", newXMLTextNode(url(fabric)), parent = wps_data)
            
            wps_in <- newXMLNode("wps:Input",parent=di)
            newXMLNode("ows:Identifier", newXMLTextNode('grid'), parent = wps_in)
            wps_data <- newXMLNode("wps:Data",parent=wps_in)
            newXMLNode("wps:LiteralData", newXMLTextNode(variables(fabric)), parent = wps_data)
            
            wps_in <- newXMLNode("wps:Input",parent=di)
            newXMLNode("ows:Identifier", newXMLTextNode('allow-cached-response'), parent = wps_in)
            wps_data <- newXMLNode("wps:Data",parent=wps_in)
            newXMLNode("wps:LiteralData", newXMLTextNode('false'), parent = wps_data)
            
            rf <- newXMLNode("wps:ResponseForm", parent = root)
            rd <- newXMLNode("wps:RawDataOutput", parent = rf)
            newXMLNode("ows:Identifier", newXMLTextNode('result_as_xml'), parent = rd)
            response <- genericExecute(knife@UTILITY_URL,toString.XMLNode(root))
            values <- tryCatch({
              nodes <- getNodeSet(gcontent(response),'//gdp:availabletimes/gdp:time')
              as.POSIXct(sapply(nodes,xmlValue), tz = 'UTC')
            }, error = function(err) {
              return(as.POSIXct(c(NA,NA)))
            })
            
            return(values)
          })