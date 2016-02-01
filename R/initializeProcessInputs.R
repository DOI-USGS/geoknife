#'@importFrom XML htmlParse getNodeSet xmlValue
defaultProcessInputs <- function(algorithm, wps_url, wps_version){
  getCaps <- gGET(wps_url, query = list(
    'service' = 'WPS', 'version' = wps_version,'request' = 'DescribeProcess', 'identifier'=algorithm))

  doc	<- gcontent(getCaps)
  
  if(length(getNodeSet(doc,'//Exception/ExceptionText'))>0){
    stop(xmlValue(getNodeSet(doc,'//Exception/ExceptionText')[[1]]))
  }
  
  optionNd	<-	getNodeSet(doc,'//DataInputs/Input[@minOccurs=0]/following-sibling::node()[1]')
  optionLs	<-	vector("list",length(optionNd))
  optionLs[]	<-	NA # "NA" is the equivalent of "optional" as an input
  names(optionLs)	<-	sapply(optionNd,xmlValue)
  
  requirLs	<-	vector("list",length(requirNd))	# max > 0 is required
  requirNd	<-	getNodeSet(doc,'//DataInputs/Input[@minOccurs>0]/following-sibling::node()[1]')
  names(requirLs)	<-	sapply(requirNd,xmlValue)
  
  # now find any defaults and set those fields to those default values
  defaultNd	<-	getNodeSet(doc,'//DataInputs/Input/LiteralData/DefaultValue/parent::node()[1]/DefaultValue')
  defaultLs	<-	vector("list",length(sapply(defaultNd,xmlValue)))
  defaultLs[]	<-	sapply(defaultNd,xmlValue)
  names(defaultLs)	<-	sapply(getNodeSet(doc,'//DataInputs/Input/LiteralData/DefaultValue/
			parent::node()[1]/preceding-sibling::node()[3]'),xmlValue)	
  
  
  # now set any accepted values
  allowNd    <-	getNodeSet(doc,'//DataInputs/Input/LiteralData//parent::node()/ows:AllowedValues/ows:Value[1]')
  allowLs	<-	vector("list",length(sapply(allowNd,xmlValue)))
  allowLs[]	<-	sapply(allowNd,xmlValue)
  names(allowLs)	<-	sapply(getNodeSet(doc,'//DataInputs/Input/LiteralData/ows:AllowedValues/
			parent::node()[1]/preceding-sibling::node()[3]'),xmlValue)
  
  # add fields for optionLs and requirLs
  processInputs	<-	append(optionLs,requirLs)
  processInputs[names(defaultLs)] <- defaultLs
  
  # remove Feature, as it is handled elsewhere
  processInputs$FEATURE_COLLECTION	<-	NULL 
  return(processInputs)
}
