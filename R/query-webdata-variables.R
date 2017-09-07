#'@rdname variables_query-method
#'@aliases variables_query,webdata-method
#'@export
setGeneric(name="variables_query",def=function(fabric, knife){standardGeneric("variables_query")})

#' variables query
#' 
#' find possible variables in a webdata dataset
#' 
#' 
#'@param variables
#'
#'@rdname variables_query-method
#'@aliases variables_query,webdata-method
#'@keywords internal
#'@export
setMethod(f = "variables_query",signature = c("webdata","missing"), 
          definition = function(fabric, knife){
            knife <- webprocess()
            variables_query(fabric, knife)
          })

#'@rdname variables_query-method
#'@aliases variables_query,webdata-method
#'@keywords internal
#'@export
setMethod(f = "variables_query",signature = c("webdata","webprocess"), 
          definition = function(fabric, knife){
            if (is.na(url(fabric))){
              stop('url cannot be NA for fabric argument when querying for available variables', call. = FALSE)
            } 

            response <- genericExecute(knife@UTILITY_URL,
                                       make_listopendapgrids_execute_xml(fabric, knife))

            # will error if none found
            values <- xml2::xml_text(
              xml2::xml_find_all(gcontent(response),'//gdp:shortname'),)
            return(values)
          })

make_listopendapgrids_execute_xml <- function(fabric, knife) {
  getgtr_list <- get_wps_execute_attributes(knife)
  getgtr_list["identifier"] <- fabric@dataList
  input_list <- list()
  input_list <- list(list(input_identifier = "catalog-url",
                          input_literal_data_element = url(fabric)),
                     list(input_identifier = "allow-cached-response",
                          input_literal_data_element = "false"))
  getgtr_list <- c(getgtr_list, inputs = list(input_list))
  getgtr_list["rawoutput"] <- list(list(result_name = "result_as_xml"))
  return(whisker::whisker.render(readLines(system.file(
    "templates/utility_execute_template.xml", package = "geoknife")), 
    getgtr_list))
}
