
#'@title parse timeseries file into R environment
#'
#'@details a function for loading data into R from a file (or URL) from a completed processing request
#'
#'@param file a \code{\link{geoknife}} timeseries processing result file location
#'(See \code{\link{checkProcess}}).
#'@param delim the file delimiter
#'@return list of timeseries values.
#'@keywords methods
#'@author Luke A. Winslow, Jordan S. Read
#'@export
#'@examples
#'local_file <- system.file('extdata','tsv_linear_ring.tsv', package = 'geoknife')
#'output <- parseTimeseries(local_file, delim = '\t')
#'@importFrom lubridate parse_date_time2
parseTimeseries <- function(file, delim){
  
  ## open file
  fid = file(file, open='r')
  # drop the first line, just variable name 
  firstLine <- readLines(fid, 1)
  varName = substr(x = firstLine, start = 3, stop = nchar(firstLine))
  
  ids = strsplit(readLines(fid, 1), delim, fixed=T)[[1]]
  
  #read the full data table
  
  data= read.table(fid, sep=delim, header=TRUE, as.is=TRUE)
  tryCatch({
    statName = strsplit(unique(names(data[-1])),'.', fixed = T)[[1]][1]
  }, error = function(e) {close(fid);stop('delimiter parse fail. ',e)})
  close(fid)
  
  #parse the date into POSIXct format
  data$TIMESTEP = parse_date_time2(data$TIMESTEP, 'YmdHMS')
  
  ids[1] = 'DateTime'
  if (ncol(data) != length(ids)) warning('col names were likely truncated due to conflict with delimiter. try TAB next time')
  names(data) = ids[1:ncol(data)] # delim can be in colnames...no fix for this.
  output <- list(data)
  names(output) <- paste0(varName,'_',statName)
  return(output)
}

