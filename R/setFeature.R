#'@details setFeature is a method for setting or modifying the feature elements
#' of for \code{geoknife} object. 
#'
#'@param An \code{geoknife} object.
#'@param a list containing a valid feature collection, or a list of a subset of a valid feature collection.
#'@return An \code{geoknife} object.
#'
#'@docType methods
#'@keywords setFeature
#'@description Set feature for \code{geoknife}
#'@title Set feature geometry for processing
#'@seealso \code{setWFS}
#'@export
#'@examples gk <- geoknife() # create geoknife object
#'linearRing = bufferPoint(c(-111.48,36.95))
#'setFeature(gk) <- list(LinearRing=linearRing)
#'
#'gk2 <- geoknife() # create geoknife object
#'feature_collection  <-  'sb:WBIC_190900'
#'attribute  <-  'WBDY_WBIC'
#'setFeature(gk2) <- list('FEATURE_COLLECTION'=feature_collection,'ATTRIBUTE'=attribute)
setGeneric(name="setFeature<-",def=function(.Object,value){standardGeneric("setFeature<-")})

# '@rdname setFeature-methods
# '@aliases setFeature,geoknife-method	
                   
setReplaceMethod(f = "setFeature",signature = "geoknife",
	definition = function(.Object,value){
		
		if ("LinearRing" %in% names(value)){
			# if we are setting the LinearRing, all other feature elements should be wiped
			if (length(names(value)) > 1){
				stop('Cannot set LinearRing and WFS components for single feature')
			} else {
				sw.idx	<-	names(.Object@feature)!='LinearRing'
				hid.feature	<-	.Object@feature[sw.idx]
				hid.feature[]	<-	'hidden'
				# set all other elements to 'hidden'

				.Object@feature	<-	setList(.Object@feature,hid.feature)
				.Object@feature	<-	setList(.Object@feature,value)

			}
		} else {
			hid.feature	<-	list(LinearRing='hidden')
			.Object@feature	<-	setList(.Object@feature,value)
			.Object@feature	<-	setList(.Object@feature,hid.feature)
			if ("FEATURE_ATTRIBUTE_NAME" %in% names(.Object@processInputs)){
			setProcessInputs(.Object)	<-	list("FEATURE_ATTRIBUTE_NAME"=.Object@feature$ATTRIBUTE)}
		}
	
		
		return(.Object)
	})