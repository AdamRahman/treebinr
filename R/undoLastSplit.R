#' Prune the Last Split Performed by treebin
#' 
#' Given a treebinr object, \code{lastSplit} prunes the last bin created (during the binning process or by the user).
#' 
#' @param treebinr_obj An object of class treebinr
#' @param selectBin A user supplied function for choosing between bins to be split
#' @param splitBin A user supplied function for splitting a bin
#' @param binMeasure A user supplied function to compute the measure associated with each bin
#' @param inputs A list containing additional input parameters required by user supplied functions
#' 
#' @return The return value is an object of class treebinr, which contains the following 
#' \item{points}{A matrix containing the reduced point configuration}
#' \item{counts}{A vector containing the number of points in each bin}
#' \item{bins}{A list containing bin objects, which detail the contents of each bin}
#' \item{tree}{An undirected graph object for the binning tree}
#' 
#' @export
undoLastSplit <- function(treebinr_obj, binMeasure, makePoint, updateBin, inputs){
  
  if(class(treebinr_obj) != "treebinr"){
    stop("Input is not a valid treebinr object")
  }
  
  if(!is.null(inputs) && !is.list(inputs)){
    stop("inputs must be a list object or be null")
  }
  
  bins <- treebinr_obj@bins
  numBins <- length(bins)
  
  #The last split will always correspond to the final bin in the bin list
  out <- treePrune(bins[[numBins]], treebinr_obj, binMeasure, makePoint, updateBin, inputs)
  
  return(out)
}