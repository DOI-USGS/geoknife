
#' parse categorical coverage file into R environment
#'
#' a function for loading data into R from a file (or URL) from a completed processing request
#'
#' @param file a \linkS4class{geojob} categorical processing result file location
#' (See \code{\link{download}}).
#' @param delim the file delimiter
#' @return a data.frame of categorical fraction (and/or count) values.
#' @keywords methods
#' @export
#' @importFrom utils read.table
#' @seealso \code{\link{check}}, \code{\link{download}}, \code{\link{parseTimeseries}}
#' @examples
#' local.file <- system.file('extdata','csv_categorical_multifeature.csv', package = 'geoknife')
#' output <- parseCategorical(local.file, delim = ',')
parseCategorical <- function(file, delim){

  config = parseCategoricalConfig(file, delim)
  
  if(!is.na(config[['features']][1]) && config[['features']][1]=="") {
    config[['features']]<-config[['features']][-1]
  }
  
  dataOut <- data.frame(c())
  
  for (blk in 1:length(config[['vars']])){
    blockData <- read.table(file, sep = delim, 
                            header = TRUE, 
                            nrows = config[['nrows']][blk], 
                            skip = config[['skip']][blk], 
                            as.is=TRUE, check.names = FALSE, stringsAsFactors = FALSE)
    startCol<-2
    catNames <- unique(names(blockData)[startCol:ncol(blockData)])
    for (cat in seq_along(catNames)){
      # select data columns based on stat code
      cat.i <- which(names(blockData) == catNames[cat])
      cat.data <- matrix(blockData[, cat.i], nrow=1)
      cat.name <- catNames[cat]
      
      cat.data = data.frame(cat.name, cat.data, as.character(config[['vars']][blk]), stringsAsFactors = FALSE)
      names(cat.data) <- c('category', blockData[, 1], 'variable')

      tryCatch({
        dataOut <- rbind(dataOut, cat.data)
      }, warning = function(w) {
        stop(paste('Variable',as.character(config[['vars']][blk]),'had a problem being added to the data frame.'))
      }, error = function(e) {
        stop(paste('Variable',as.character(config[['vars']][blk]),'had a problem being added to the data frame.'))
      }
      )
    }
  }
  return(dataOut)
  
}

parseCategoricalConfig = function(file, delim){
  skipHead = 1 # Number of lines to skip past the variable marker header?
  vals.start = 3 #lines after blockStart when the data begin
  varMarker = '# ' # Symbol that denotes a variable identifier and a new block of output.
  c <- file(file,"r") 
  fileLines <- readLines(c) # This concerns me!!!
  close(c)
  nRead <- length(fileLines)
  blockStart <- grep(varMarker, fileLines) # Lines containing variable IDs.
  skips = blockStart+skipHead 
  blockEnd = c(blockStart[-1] - 1, nRead) # End of blocks
  nrows = blockEnd - skips - 1 # Number of ros per block.
  data.lines <- unlist(lapply(seq_along(blockStart), function(x) (blockStart[x]+vals.start):blockEnd[x]))
  features = unique(unlist(lapply(fileLines[data.lines], function(x) strsplit(x, split = delim)[[1]][1]))) # Parsing out feature identifiers
  vars = sub(varMarker,"",fileLines[blockStart]) # Getting the variable names from the block starts.
  config = list(vars = vars, features = features, skip = skips, nrows = nrows) # Return all the good stuff!
  return(config)
}