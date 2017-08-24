#'@rdname email-method
#'@aliases email,geojob-method
#'@export
setGeneric(name="email",def=function(geojob, knife){standardGeneric("email")})

#' email user when processing job is complete
#' 
#' configure job to send an email when complete (success or failure)
#'
#'@param email a character for email address to use
#'
#'@rdname email-method
#'@aliases email,geojob-method
#'@keywords internal
#'@export
setMethod(f = "email",signature = c("geojob",'webprocess'), 
          definition = function(geojob, knife){

            response <- genericExecute(knife@UTILITY_URL,
                                       make_email_execute_xml(geojob, knife))
            #return boolean?
          })

make_email_execute_xml <- function(geojob, knife) {
  email_list <- get_wps_execute_attributes(knife)
  email_list["identifier"] <- knife@emailK
  input_list <- list()
  input_list <- list(list(input_identifier = "wps-checkpoint",
                          input_literal_data_element = id(geojob)),
                     list(input_identifier = "email",
                          input_literal_data_element = knife@email),
                     list(input_identifier = "filename",
                          input_literal_data_element = "geoknife_output"))
  email_list <- c(email_list, inputs = list(input_list))
  email_list["result_name"] <- "result"
  return(whisker::whisker.render(readLines(system.file(
    "templates/utility_execute_template.xml", package = "geoknife")), 
                                 email_list))
}