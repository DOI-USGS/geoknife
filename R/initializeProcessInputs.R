#'@importFrom XML htmlParse getNodeSet xmlValue
defaultProcessInputs <- function(algorithm, wps_url, wps_version){
  getCaps <- gGET(wps_url, query = list(
    'service' = 'WPS', 'version' = wps_version,'request' = 'DescribeProcess', 'identifier'=algorithm))

  
  doc = tryCatch({
    gcontent(getCaps)
  }, error = function(e) {
    stop(wps_url, ' does not seem to be a valid Web Processing Service url.', call. = FALSE)
  })

  checkException(doc)
  
  # -- get optional arguments --
  optionNd	<-	getNodeSet(doc,'//DataInputs/Input[@minOccurs=0]/ows:Identifier')
  optionLs	<-	vector("list",length(optionNd))
  optionLs[]	<-	NA # "NA" is the equivalent of "optional" as an input
  names(optionLs)	<-	sapply(optionNd,xmlValue)
  
  # -- get required arguments -- (minOccurs > 0 means required)
  requirNd	<-	getNodeSet(doc,'//DataInputs/Input[@minOccurs>0]/ows:Identifier')
  requirLs	<-	vector("list",length(requirNd))	
  names(requirLs)	<-	sapply(requirNd,xmlValue)
  
  # -- get and set default values --
  defaultTag <- '//DataInputs/Input/LiteralData/DefaultValue'
  defaultNd	<-	getNodeSet(doc,paste0(defaultTag, '/parent::node()[1]/DefaultValue'))
  defaultLs	<-	vector("list",length(sapply(defaultNd,xmlValue)))
  defaultLs[]	<-	sapply(defaultNd,xmlValue)
  names(defaultLs)	<-	sapply(getNodeSet(doc,paste0(defaultTag, '/parent::node()[1]/preceding-sibling::node()[3]')),xmlValue)	
  
  
  # -- get and set allowed values -- ?? why isn't this set up like defaults??
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

checkException <- function(doc){
  exceptionTag <- '//ows:Exception/ows:ExceptionText'
  if(length(getNodeSet(doc, exceptionTag))>0){
    stop(xmlValue(getNodeSet(doc, exceptionTag)[[1]]))
  }
}
