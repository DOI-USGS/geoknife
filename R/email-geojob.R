#'@rdname email-method
#'@aliases email,geojob-method
#'@export
setGeneric(name="email",def=function(geojob, knife){standardGeneric("email")})

#' email user when processing job is complete
#' 
#' configure job to send an email when complete (success or failure)
#'
#'@param email a character for email address to use
#'
#'@rdname email-method
#'@aliases email,geojob-method
#'@keywords internal
#'@importFrom XML newXMLNode addChildren toString.XMLNode xmlChildren<- xmlValue<- xmlParseString removeNodes
#'@export
setMethod(f = "email",signature = c("geojob",'webprocess'), 
          definition = function(geojob, knife){
            
            doc <- xmlTreeParse(xml(geojob), useInternalNodes = TRUE)
            root <- xmlRoot(doc)
            removeNodes(root[names(root) == "DataInputs"])
            removeNodes(root[names(root) == "ResponseForm"])
            removeNodes(root[names(root) == "Identifier"])
            
            newXMLNode("ows:Identifier", newXMLTextNode(knife@emailK), parent=root)
            di <- newXMLNode("wps:DataInputs",parent=root)
            wps_in <- newXMLNode("wps:Input",parent=di)
            newXMLNode("ows:Identifier", newXMLTextNode('wps-checkpoint'), parent = wps_in)
            wps_data <- newXMLNode("wps:Data",parent=wps_in)
            newXMLNode("wps:LiteralData", newXMLTextNode(id(geojob)), parent = wps_data)
            
            wps_in <- newXMLNode("wps:Input",parent=di)
            newXMLNode("ows:Identifier", newXMLTextNode('email'), parent = wps_in)
            wps_data <- newXMLNode("wps:Data",parent=wps_in)
            newXMLNode("wps:LiteralData", newXMLTextNode(knife@email), parent = wps_data)
            
            wps_in <- newXMLNode("wps:Input",parent=di)
            newXMLNode("ows:Identifier", newXMLTextNode('filename'), parent = wps_in)
            wps_data <- newXMLNode("wps:Data",parent=wps_in)
            newXMLNode("wps:LiteralData", newXMLTextNode('geoknife_output'), parent = wps_data)
            
            rf <- newXMLNode("wps:ResponseForm", parent = root)
            rd <- newXMLNode("wps:ResponseDocument", parent = rf)
            out <- newXMLNode("wps:Output", parent = rd)
            newXMLNode("ows:Identifier", newXMLTextNode('result'), parent = out)
            response <- genericExecute(knife@UTILITY_URL,toString.XMLNode(root))
            #return boolean?
          })