#'@importFrom XML htmlParse getNodeSet xmlValue
defaultProcessInputs <- function(algorithm, wps_url, wps_version){
  processURL  <-	paste0(wps_url,'?service=WPS&version=',
                        wps_version,'&request=DescribeProcess',
                         '&identifier=',algorithm)
  # test connection with processURL here, provide error if necessary
  doc	<-	htmlParse(processURL,isURL=TRUE, useInternalNodes = TRUE)
  
  if(length(getNodeSet(doc,'//exception/exceptiontext'))>0){
    stop(xmlValue(getNodeSet(doc,'//exception/exceptiontext')[[1]]))
  }
  
  optionNd	<-	getNodeSet(doc,'//datainputs/input[@minoccurs=0]/following-sibling::node()[1]')
  optionLs	<-	vector("list",length(optionNd))
  optionLs[]	<-	NA # "NA" is the equivalent of "optional" as an input
  names(optionLs)	<-	sapply(optionNd,xmlValue)
  
  requirNd	<-	getNodeSet(doc,'//datainputs/input[@minoccurs>0]/following-sibling::node()[1]')
  requirLs	<-	vector("list",length(requirNd))	# max > 0 is required
  names(requirLs)	<-	sapply(requirNd,xmlValue)
  
  # now find any defaults and set those fields to those default values
  defaultNd	<-	getNodeSet(doc,'//datainputs/literaldata/defaultvalue/parent::node()[1]/defaultvalue')
  defaultLs	<-	vector("list",length(sapply(defaultNd,xmlValue)))
  defaultLs[]	<-	sapply(defaultNd,xmlValue)
  names(defaultLs)	<-	sapply(getNodeSet(doc,'//datainputs/literaldata/defaultvalue/
			parent::node()[1]/preceding-sibling::node()[3]'),xmlValue)	
  
  
  # now set any accepted values
  allowNd    <-	getNodeSet(doc,'//datainputs/literaldata//parent::node()/allowedvalues/value[1]')
  allowLs	<-	vector("list",length(sapply(allowNd,xmlValue)))
  allowLs[]	<-	sapply(allowNd,xmlValue)
  names(allowLs)	<-	sapply(getNodeSet(doc,'//datainputs/literaldata/allowedvalues/
			parent::node()[1]/preceding-sibling::node()[3]'),xmlValue)
  
  # add fields for optionLs and requirLs
  processInputs	<-	append(optionLs,requirLs)
  processInputs[names(defaultLs)] <- defaultLs
  
  # remove Feature, as it is handled elsewhere
  processInputs$FEATURE_COLLECTION	<-	NULL 
  return(processInputs)
}
