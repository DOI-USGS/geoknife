#'@rdname XML-method
#'@aliases XML,webgeom-method
#'@export
setGeneric(name="XML",def=function(stencil, fabric, knife){standardGeneric("XML")})

#' XML from set of objects
#' 
#' Extract important parts of stencil, fabric, and knife into POST XML
#' 
#'@param stencil a \code{\link{webdata}} OR \code{\link{simplegeom}} object
#'@param fabric a \code{\link{webdata}} object
#'@param knife a \code{\link{webprocess}} object
#'@return XML as ?string?
#'
#'@examples
#'wd <- webdata('prism',times = as.POSIXct(c('2001-01-01','2002-02-05')))
#'wg <- webgeom('state::Wisconsin')
#'\dontrun{
#'XML(wg, wd, webprocess())
#'sg <- simplegeom(c(-89,45))
#'XML(sg, wd, webprocess())
#'}
#'@rdname XML-method
#'@export
setMethod(f = "XML",signature = c("ANY","webdata","webprocess"), 
          definition = function(stencil, fabric, knife){
            #stencil can be webgeom OR simplegeom 
            
  knife <- .setProcessInputs(webprocess = knife, 
                             stencil = stencil, 
                             fabric = fabric)
  
  whisker_list <- get_wps_execute_attributes(knife)
  
  whisker_list["identifier"] <- knife@algorithm[[1]]
  
  input_list <- list() # unnamed list for {{#inputs}}
  
  for (i in 1:length(knife@processInputs)){
    input_identifier	<-	names(knife@processInputs[i])
    input_literal_data <- knife@processInputs[input_identifier]
    
    if (!is.na(input_literal_data)){
      
      data_element_list <- list() # unnamed list for {{#data_elements}}
      
      for (j in 1:length(unlist(input_literal_data))){
        input_literal_data_element <- 
          unlist(knife@processInputs[input_identifier])[[j]]
        
        if (is.null(input_literal_data_element)) {
          stop(input_identifier, ' cannot be NULL. it is required') }
        
        input_list <- c(input_list,
                        list(list(input_identifier = input_identifier,
                                  input_literal_data_element = input_literal_data_element)))
      }
    }
    
  }
  
  whisker_list["inputs"] <- list(input_list)
  
  whisker_list["feature_collection"] <- "FEATURE_COLLECTION"

  whisker_list <- c(whisker_list, suppressWarnings(addGeom(stencil)))
  
  whisker_list <- c(whisker_list, addResponse(knife))
  
  return(whisker::whisker.render(readLines(system.file(
    "templates/execute_template.xml", package = "geoknife")), whisker_list))
  
})

addResponse <- function(.Object){
  
  response_list <- list()
  
  # Store the response document on the server to download later?
  response_list["storeExecuteResponse"] <- "true"
  
  # Return a status document or just return the result?
  response_list["status"] <- "true"

  # include the response as a url reference or in line?
  response_list["asReference"] <- "true"
  
  #if text/tab-separated-values" or output_type
  if (!is.null(.Object@processInputs$DELIMITER) && 
      .Object@processInputs$DELIMITER=="TAB"){
    response_list["output_mimetype"] <- ' mimeType="text/tab-separated-values"'
  } else if (!is.null(.Object@processInputs$OUTPUT_TYPE) && 
             .Object@processInputs$OUTPUT_TYPE=="geotiff") {
    response_list["output_mimetype"] <- ' mimeType="application/zip"'
  }

  response_list["output_identifier"] <- "OUTPUT"
  
  return(response_list)
}

setGeneric(name="addGeom",def=function(stencil, xmlNodes){standardGeneric("addGeom")})

setMethod(f = "addGeom",signature = c("webgeom","ANY"), 
          definition = function(stencil){
            
  geom_list <- list()
  
  # This is the WFS service base URL
  geom_list["wps_reference_href"]  <- url(stencil)
  
  # These could / should be hard coded in the template.
  geom_list["wfs_namespace"] <- stencil@WFS_NAMESPACE
  geom_list["gml_namespace"] <- stencil@GML_NAMESPACE
  geom_list["wfs_version"] <- version(stencil)
  geom_list["wfs_service"] <- "WFS"
  
  # This is the format required by the GDP.
  geom_list["wfs_outputformat"] <- "text/xml; subtype=gml/3.1.1"

  # This is the WFS layer/typename
  geom_list["wfs_typename"] <- as.character(stencil@geom)
  
  # This is what the WFS layer calls its geometry property
  geom_list["wfs_geom_property"] <- "the_geom" # this is only valid for geoserver
  
  # This is the attribute property that the GDP will use to label output
  geom_list["wfs_attribute_property"] <- stencil@attribute
  
  if(!is.na(stencil@values[1])) {
    properties <- list()
    for (val in values(stencil)){
      properties <- c(properties,
                      list(list(match_case = "true",
                                property_name = stencil@attribute,
                                property_literal = val)))
    }
    
    geom_list["filter_properties"] <- list(list(properties = properties))
  }
  
  return(list(wfs_reference = list(geom_list)))
})

setMethod(f = "addGeom",signature = c("ANY","ANY"),
          definition = function(stencil, xmlNodes){
            stencil <- simplegeom(stencil)
            return(addGeom(stencil, xmlNodes))
})

#'@importFrom sp coordinates
setMethod(f = "addGeom",signature = c("simplegeom","ANY"),
          definition = function(stencil, xmlNodes){
  
  simplegeom_list <- list()
  
  simplegeom_list["simplegeo_mimetype"] <- "text/xml" # "encoding"="UTF-8"?
  simplegeom_list["simplegeo_schema"] <- 
    "http://schemas.opengis.net/gml/3.1.1/base/feature.xsd"
  simplegeom_list["gml_namespace"] <- "http://www.opengis.net/gml"
  simplegeom_list["draw_namespace"] <- stencil@DRAW_NAMESPACE
  simplegeom_list["draw_schema_location"] <- paste(c(stencil@DRAW_NAMESPACE,
                                                     stencil@DRAW_SCHEMA),
                                                   collapse=' ')
  
  # loop this section for multiple polygons:
  
  geom <- stencil@sp
  poly_list <- list()
  for (j in seq_along(geom)){
    
    geom_list <- list(poly_id = sprintf("poly.%s",j))
    geom_list["geom_property"] <- "the_geom"
    
    ringCoords <- geom@polygons[[j]]@Polygons[[1]]@coords
    
    ring = vector(length = 2*nrow(ringCoords))
    ring[seq(from = 1,by = 2,length.out = nrow(ringCoords))] <- ringCoords[,2]
    ring[seq(from = 2,by = 2,length.out = nrow(ringCoords))] <- ringCoords[,1]
    ring.val	<-	paste(ring,collapse = ' ')
    drawID <- geom@polygons[[j]]@ID

    geom_list["filter_id"] <- .FEATURE_ATTRIBUTE_NAME(stencil)
    geom_list["ID_property"] <- drawID
    geom_list["srsDimension"] <- "2"
    geom_list["srsName"] <- "urn:x-ogc:def:crs:EPSG:4326" # FROM CRS in the future!!!
    
    geom_list["poslist"] <- ring.val
    
    poly_list <- c(poly_list,
                   list(geom_list))
  }
  simplegeom_list["polys"] <- list(poly_list)

  return(list(simple_geometry = list(simplegeom_list)))
  
})