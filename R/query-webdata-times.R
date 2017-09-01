#'@rdname times_query-method
#'@aliases times_query,webdata-method
#'@export
setGeneric(name="times_query",def=function(fabric, knife){standardGeneric("times_query")})

#' times query
#' 
#' query a webdata object for the time range
#' 
#' @rdname times_query-method
#' @aliases times_query,webdata-method
#' @keywords internal
#' @export
setMethod(f = "times_query",signature = c("webdata","missing"), 
          definition = function(fabric, knife){
            knife <- webprocess()
            times_query(fabric, knife)
          })

#'@rdname times_query-method
#'@aliases times_query,webdata-method
#'@keywords internal
#'@export
setMethod(f = "times_query",signature = c("webdata","webprocess"), 
          definition = function(fabric, knife){
            
            if (any(is.na(variables(fabric)))){
              stop('variables cannot be NA for fabric argument when querying for available time range', call. = FALSE)
            } 
            if (length(variables(fabric)) > 1) {
              warning('variables is > 1, using ', variables(fabric)[1], 
                      ' only when querying for available time range', call. = FALSE)    
            }
            
            response <- genericExecute(knife@UTILITY_URL,
                                       make_getgridtimerange_execute_xml(fabric, knife))
            values <- tryCatch({
              nodes <- xml2::xml_find_all(gcontent(response),'//gdp:availabletimes/gdp:time')
              as.POSIXct(sapply(nodes,xml2::xml_text), tz = 'UTC')
            }, error = function(err) {
              return(as.POSIXct(c(NA,NA)))
            })
            
            return(values)
          })

make_getgridtimerange_execute_xml <- function(fabric, knife) {
  getgtr_list <- get_wps_execute_attributes(knife)
  getgtr_list["identifier"] <- fabric@timeList
  input_list <- list()
  input_list <- list(list(input_identifier = "catalog-url",
                          input_literal_data_element = url(fabric)),
                     list(input_identifier = "grid",
                          input_literal_data_element = variables(fabric)[1]),
                     list(input_identifier = "allow-cached-response",
                          input_literal_data_element = "false"))
  getgtr_list <- c(getgtr_list, inputs = list(input_list))
  getgtr_list["rawoutput"] <- list(list(result_name = "result_as_xml"))
  return(whisker::whisker.render(readLines(system.file(
    "templates/utility_execute_template.xml", package = "geoknife")), 
    getgtr_list))
}
