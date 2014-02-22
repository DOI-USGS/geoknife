setGeneric(name="initializePostInputs",def=function(.Object){
	standardGeneric("initializePostInputs")
	}
)



setMethod(f = "initializePostInputs",signature="rGDP",
	definition =	function(.Object){
		# this method is private to rGDP, and CREATES and SETS fields in the PostInputs
		
		algorithm	<-	.Object@algorithm
		processURL	<-	paste(c(.Object@WPS_URL,'?service=WPS&version=',
			.Object@WPS_DEFAULT_VERSION,'&request=DescribeProcess',
			'&identifier=',algorithm),collapse="")	
			
		# test connection with processURL here, provide error if necessary
		doc	<-	htmlParse(processURL,isURL=TRUE, useInternalNodes = TRUE)
		optionNd	<-	getNodeSet(doc,'//datainputs/input[@minoccurs=0]/following-sibling::node()[1]')
		optionLs	<-	vector("list",length(optionNd))
		optionLs[]	<-	NA # "NA" is the equivalent of "optional" as an input
		names(optionLs)	<-	sapply(optionNd,xmlValue)
		
		requirNd	<-	getNodeSet(doc,'//datainputs/input[@minoccurs>0]/following-sibling::node()[1]')
		requirLs	<-	vector("list",length(requirNd))	# max > 0 is required
		names(requirLs)	<-	sapply(requirNd,xmlValue)
		
		# add fields for optionLs and requirLs
		.Object@postInputs	<-	append(optionLs,requirLs)
		.Object	<-	setPostInputs(.Object,requirLs)
		.Object	<-	setPostInputs(.Object,optionLs)

		# now find any defaults and set those fields to those default values
		defaultNd	<-	getNodeSet(doc,'//datainputs/literaldata/defaultvalue/parent::node()[1]/defaultvalue')
		defaultLs	<-	vector("list",length(sapply(defaultNd,xmlValue)))
		defaultLs[]	<-	sapply(defaultNd,xmlValue)
		names(defaultLs)	<-	sapply(getNodeSet(doc,'//datainputs/literaldata/defaultvalue/
			parent::node()[1]/preceding-sibling::node()[3]'),xmlValue)	
		if (length(defaultLs) > 0){
			.Object	<-	setPostInputs(.Object,defaultLs)
		}
		
		# now set any accepted values
		allowNd    <-	getNodeSet(doc,'//datainputs/literaldata//parent::node()/allowedvalues/value[1]')
		allowLs	<-	vector("list",length(sapply(allowNd,xmlValue)))
		allowLs[]	<-	sapply(allowNd,xmlValue)
		names(allowLs)	<-	sapply(getNodeSet(doc,'//datainputs/literaldata/allowedvalues/
			parent::node()[1]/preceding-sibling::node()[3]'),xmlValue)

		if (length(allowLs)>0){
			.Object	<-	setPostInputs(.Object,defaultLs)
		}
		
		.Object@postInputs$FEATURE_COLLECTION	<-	NULL # handled elsewhere
		return(.Object)
		
	}
)