
#'@title parse timeseries file into R environment
#'
#'@details a function for loading data into R from a file (or URL) from a completed processing request
#'
#'@param file a \linkS4class{geojob} timeseries processing result file location
#'(See \code{\link{check}}).
#'@param delim the file delimiter
#'@param keep.units boolean for including a units column in returned data.frame
#'@return a data.frame of timeseries values.
#'@keywords methods
#'@author Luke A. Winslow, Jordan S. Read
#'@export
#'@examples
#'local_file <- system.file('extdata','tsv_linear_ring.tsv', package = 'geoknife')
#'output <- parseTimeseries(local_file, delim = '\t')
#'@importFrom lubridate parse_date_time2
parseTimeseries <- function(file, delim, keep.units = FALSE){
  
  config = parseConfig(file, delim)
  
  dataOut <- data.frame(
    row.names = c('TIMESTEP',config[['features']], 'variable', 'statistic'))
  
  for (blk in 1:length(config[['vars']])){
    blockData <- read.table(file, sep = delim, 
                  header = TRUE, 
                  nrows = config[['nrows']][blk], 
                  skip = config[['skip']][blk], 
                  as.is=TRUE, check.names = FALSE)
    #parse the date into POSIXct format
    blockData$TIMESTEP = parse_date_time2(blockData$TIMESTEP, 'YmdHMS')
    statNames <- unique(names(blockData)[-1])
    for (st in 1:length(statNames)){
      # select data columns based on stat code
      statI <- which(names(blockData) == statNames[st])
      statData <- blockData[, c(1,statI)]
      
      tryCatch({
        names(statData)[-1] <- config[['features']]
      }, warning = function(w) {
        stop('Delimiter parse fail.')
      }, error = function(e) {
        stop('Delimiter parse fail.')
        }
      )
      
      #remove units from stats names
      statSplit = strsplit(x = statNames[st], split = '[()]')[[1]]
      cleanStat <- statSplit[1]
      units <- statSplit[2]
      
      statData = cbind(statData, data.frame(
        'variable' = rep(as.character(config[['vars']][blk]), length.out = nrow(statData)),
        'statistic' = rep(cleanStat, length.out = nrow(statData)))
      )
      if (keep.units){
        statData = cbind(statData, data.frame('units'=rep(units, length.out = nrow(statData))))
      }
      dataOut <- rbind(dataOut, statData)
    }
    
  }

  names(dataOut)[1] = 'DateTime'
  return(dataOut)
}

parseConfig = function(file, delim){
  featureLine = 2
  skipHead = 1
  varMarker = '# '
  c <- file(file,"r") 
  fileLines <- readLines(c)
  close(c)
  nRead <- length(fileLines)
  blockStart <- grep(varMarker, fileLines)
  skips = blockStart+skipHead
  blockEnd = c(blockStart[-1] - 1, nRead)
  nrows = blockEnd - skips - 1
  features = unique(strsplit(fileLines[featureLine], split = delim)[[1]][-1])
  vars = sub(varMarker,"",fileLines[blockStart])
  config = list(vars = vars, features = features, skip = skips, nrows = nrows)
  return(config)
}
parseChunk = function(lines, delim, use_cols){
  # won't know var name, will return d.f
  
  df <- data.frame()
  return(df)
}