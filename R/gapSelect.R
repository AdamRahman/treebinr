#' Select the Bin to be Split
#' 
#' \code{gapSelect} uses the measures computed using \code{gapMeasure} to determine which of 
#' the bins should be chosen for splitting.
#' 
#' @param bins A list containing bin objects
#' @param inputs A list containing additional input parameters required by user supplied functions
#' 
#' @return 
#' \item{Index}{The index number of the chosen bin to split}
#' 
#' @export
gapSelect <- function(bins, inputs){
  
  gapMax <- sapply(bins, findMax)
  
  #Find the bin with the largest of all gaps (in the event of a tie, take the first bin)
  binIndex <- which.max(gapMax)
  
  return(binIndex)
}

#Find the maximum gap in each bin
findMax <- function(bin){
  max <- max(bin@measure)
  return(max)
}
