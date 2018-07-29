#' Trim the Binning Tree to a Given Number of Bins
#' 
#' Given a treebinr object, \code{trimTo} trims the tree to the desired size by removing the bins with the lowest measure value. 
#' 
#' @param num The number of points desired in the trimmed configuration
#' @param treebinr_obj An object of class treebinr
#' @param getMeasure A user supplied function to compute the measure to be compared between bins. By default, returns whatever value is in the measure slot of the bin object.
#' @param binMeasure A user supplied function to compute the measure associated with each bin
#' @param splitBin A user supplied function for splitting a bin
#' @param makePoint A user supplied function to turn the contents of a bin into a single point
#' @param inputs A list containing additional input parameters required by user supplied functions
#' 
#' @return The return value is an object of class treebinr, which contains the following 
#' \item{points}{A matrix containing the reduced point configuration}
#' \item{counts}{A vector containing the number of points in each bin}
#' \item{bins}{A list containing bin objects, which detail the contents of each bin}
#' \item{tree}{An undirected graph object for the binning tree}
#' 
#' @export
treeTrim <- function(num, 
                   treebinr_obj,
                   getMeasure = function(bin, inputs){bin@measure},
                   binMeasure,
                   makePoint,
                   updateBin,
                   inputs = NULL){
  
  points <- treebinr_obj@points
  counts <- treebinr_obj@counts
  bins <- treebinr_obj@bins
  tree <- treebinr_obj@tree
  
  #Collect the measures from each bin
  binVals <- sapply(bins, getMeasure, inputs=inputs)
  
  #Find the measure of binvals that will bin to desired number of bin
  numNA <- sum(is.na(binVals))
  adjustedNum <- num - numNA
  adjustedBinNum <- (length(bins) - numNA)
  
  getQuantile <- quantile(binVals, adjustedNum/adjustedBinNum, na.rm=TRUE)
  
  out <- trim(getQuantile, treebinr_obj, getMeasure, binMeasure, makePoint, updateBin, inputs)
  
  return(out)
}