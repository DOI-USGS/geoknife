#'@details bufferPoint is a function that takes a longitude and latitude pair and creates
#' a buffered ring (i.e., a feature with non-zero area) centered.
#'
#'@param A vector of longitude and latitude values
#'@return A linear ring (with closure) as a numeric vector 
#'
#'@keywords bufferPoint
#'@description Buffer ring creation from point
#'@title Create linear ring from point
#'@seealso \code{setFeature}
#'@examples linearRing = bufferPoint(c(-111.48, 36.95))
#'@export
bufferPoint	<-	function(point){
	offset	<-	0.0001
	
	if (length(point) %% 2 != 0){
		stop('point must be latitude and longitude pairs')
	}
	
	tp.lat	<-	point[2]+offset
	md.lat	<-	point[2]
	bt.lat	<-	point[2]-offset
	rt.lon	<-	point[1]+offset
	md.lon	<-	point[1]
	lf.lon	<-	point[1]-offset
	LinearRing	<-	c(lf.lon,md.lat, md.lon,tp.lat, rt.lon,md.lat, md.lon,bt.lat, lf.lon,md.lat)
	return(LinearRing)
	
}