
#' parse timeseries file into R environment
#'
#' a function for loading data into R from a file (or URL) from a completed processing request
#'
#' @param file a \linkS4class{geojob} timeseries processing result file location
#' (See \code{\link{download}}).
#' @param delim the file delimiter
#' @param with.units boolean for including a units column in returned data.frame (default = \code{FALSE})
#' @return a data.frame of timeseries values.
#' @keywords methods
#' @author Luke A. Winslow, Jordan S. Read
#' @export
#' @seealso \code{\link{check}}, \code{\link{download}}, \code{\link{parseCategorical}}
#' @importFrom utils read.table
#' @examples
#' local_file <- system.file('extdata','tsv_linear_ring.tsv', package = 'geoknife')
#' output <- parseTimeseries(local_file, delim = '\t')
parseTimeseries <- function(file, delim, with.units = FALSE){
  
  config = parseTimeseriesConfig(file, delim)
  
  if(!is.na(config[['features']][1]) && config[['features']][1]=="") {
    config[['features']]<-config[['features']][-1]
  }
  
  dataOut <- data.frame(
    row.names = c('TIMESTEP',config[['features']], 'variable', 'statistic'))
  
  for (blk in 1:length(config[['vars']])){
    blockData <- read.table(file, sep = delim, 
                  header = TRUE, 
                  nrows = config[['nrows']][blk], 
                  skip = config[['skip']][blk], 
                  as.is=TRUE, check.names = FALSE, stringsAsFactors = FALSE)
    #parse the date into POSIXct format
    blockData$TIMESTEP = as.POSIXct(blockData$TIMESTEP, "%Y-%m-%dT%H:%M:%S", tz="UTC")
    if(any(grepl("threshold",names(blockData)))){
      replace.name <- names(blockData)[grepl("threshold",names(blockData))]
      startCol<-3
    } else if (any(grepl("time",names(blockData)))){
      replace.name <- names(blockData)[grepl("time",names(blockData))]
      startCol<-3
    } else {
      startCol<-2
    }
    statNames <- unique(names(blockData)[startCol:ncol(blockData)])
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
        'statistic' = rep(cleanStat, length.out = nrow(statData)), stringsAsFactors = FALSE)
      )
      if (startCol==3) {
      	names(blockData)[2] <- replace.name
        statData=cbind(statData,blockData[2])
      }
      if (with.units){
        statData = cbind(statData, data.frame('units'=rep(units, length.out = nrow(statData)), stringsAsFactors = FALSE))
      }
      tryCatch({
        dataOut <- rbind(dataOut, statData)
      }, warning = function(w) {
        stop(paste('Variable',as.character(config[['vars']][blk]),'had a problem being added to the data frame.'))
      }, error = function(e) {
        stop(paste('Variable',as.character(config[['vars']][blk]),'had a problem being added to the data frame.'))
        }
      )
    }
  }

  names(dataOut)[1] = 'DateTime'

  return(dataOut)
}

parseTimeseriesConfig = function(file, delim){
    featureLine = 2 # Line containing unique IDs of features (stencil) that were processed
    skipHead = 1 # Number of lines to skip past the variable marker header?
    varMarker = '# ' # Symbol that denotes a variable identifier and a new block of output.
    c <- file(file,"r") 
    fileLines <- readLines(c) # This concerns me!!!
    close(c)
    nRead <- length(fileLines)
    blockStart <- grep(varMarker, fileLines) # Lines containing variable IDs.
    skips = blockStart+skipHead 
    blockEnd = c(blockStart[-1] - 1, nRead) # End of blocks
    nrows = blockEnd - skips - 1 # Number of ros per block.
    features = unique(strsplit(fileLines[featureLine], split = delim)[[1]][-1]) # Parsing out feature identifiers
    vars = sub(varMarker,"",fileLines[blockStart]) # Getting the variable names from the block starts.
    config = list(vars = vars, features = features, skip = skips, nrows = nrows) # Return all the good stuff!
  return(config)
}
parseChunk = function(lines, delim, use_cols){
  # won't know var name, will return d.f
  
  df <- data.frame()
  return(df)
}