setGeneric(name="initializePostInputs",def=function(.Object){
	standardGeneric("initializePostInputs")
	}
)



setMethod(f = "initializePostInputs",signature="rGDP",
	definition =	function(.Object){
		algorithm	<-	.Object@algorithm
		processURL	<-	paste(c(.Object@WPS_URL,'?service=WPS&version=',
			.Object@WPS_DEFAULT_VERSION,'&request=DescribeProcess',
			'&identifier=',algorithm),collapse="")
			
		# test connection with processURL here, provide error if necessary
		doc	<-	htmlParse(processURL,isURL=TRUE, useInternalNodes = TRUE)
		optionNd	<-	getNodeSet(doc,'//datainputs/input[@minoccurs=0]/following-sibling::node()[1]')
		optionLs	<-	vector("list",length(optionNd))
		optionLs[]	<-	NA
		names(optionLs)	<-	sapply(optionNd,xmlValue)
		
		requirNd	<-	getNodeSet(doc,'//datainputs/input[@minoccurs>0]/following-sibling::node()[1]')
		requirLs	<-	vector("list",length(requirNd))
		names(requirLs)	<-	sapply(requirNd,xmlValue)
		
		.Object@postInputs	<-	append(optionLs,requirLs)
		.Object	<-	setPostInputs(.Object,requirLs)
		
		# now set any defaults to their default values
		defaultNd	<-	getNodeSet(doc,'//datainputs/literaldata/defaultvalue/parent::node()[1]/defaultvalue')
		defaultLs	<-	vector("list",length(sapply(defaultNd,xmlValue)))
		defaultLs[]	<-	sapply(defaultNd,xmlValue)
		names(defaultLs)	<-	sapply(getNodeSet(doc,'//datainputs/literaldata/defaultvalue/
			parent::node()[1]/preceding-sibling::node()[3]'),xmlValue)
			
		if (length(defaultLs) > 0){
			.Object@postInputs	<-	setList(.Object@postInputs,defaultLs)
		}
		
		# now set any accepted values
		allowNd    <-	getNodeSet(doc,'//datainputs/literaldata//parent::node()/allowedvalues/value[1]')
		allowLs	<-	vector("list",length(sapply(allowNd,xmlValue)))
		allowLs[]	<-	sapply(allowNd,xmlValue)
		names(allowLs)	<-	sapply(getNodeSet(doc,'//datainputs/literaldata/allowedvalues/
			parent::node()[1]/preceding-sibling::node()[3]'),xmlValue)
		if (length(allowLs)>0){.Object@postInputs	<-	setList(.Object@postInputs,allowLs)}
		
		.Object@postInputs$FEATURE_COLLECTION	<-	NULL # handled elsewhere
		return(.Object)
		
	}
)