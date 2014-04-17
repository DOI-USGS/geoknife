setGeneric(name="initializeProcessInputs",def=function(.Object){
	standardGeneric("initializeProcessInputs")
	}
)

setMethod(f = "initializeProcessInputs",signature="geoknife",
	definition =	function(.Object){
		# this method is private to geoknife, and CREATES and SETS fields in the ProcessInputs
		
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
		.Object@processInputs	<-	append(optionLs,requirLs)
		
    # if required elements are NULL, they are set to text --required--
		#requirLs[]  <-	sapply(requirLs,function(x) {if (is.null(x)){'--required--'}})
    
		setProcessInputs(.Object)	<-	requirLs
		if("FEATURE_ATTRIBUTE_NAME" %in% names(requirLs)){
		  .Object@processInputs$FEATURE_ATTRIBUTE_NAME  <-	'the_geom'
		}
    setProcessInputs(.Object)	<-	optionLs
		# set defaults and first of the allowed values
		setProcessInputs(.Object)	<-	defaultLs
		setProcessInputs(.Object)	<-	allowLs
		
		# remove Feature, as it is handled elsewhere
		.Object@processInputs$FEATURE_COLLECTION	<-	NULL 

		return(.Object)
		
	}
)