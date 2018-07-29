#' Perform one Additional Iteration of treebin
#' 
#' Given a treebinr object, \code{nextSplit} performs one additional iteration of treebin based on user supplied input function
#' 
#' @param treebinr_obj An object of class treebinr
#' @param selectBin A user supplied function for choosing between bins to be split
#' @param splitBin A user supplied function for splitting a bin
#' @param binMeasure A user supplied function to compute the measure associated with each bin
#' @param makePoint  A user supplied function to turn the contents of a bin into a single point
#' @param inputs A list containing additional input parameters required by user supplied functions
#' 
#' @return The return value is an object of class treebinr, which contains the following 
#' \item{points}{A matrix containing the reduced point configuration}
#' \item{counts}{A vector containing the number of points in each bin}
#' \item{bins}{A list containing bin objects, which detail the contents of each bin}
#' \item{tree}{An undirected graph object for the binning tree}
#' 
#' @examples 
#' 
#' @export 
doNextSplit <- function(treebinr_obj, selectBin, splitBin, binMeasure, makePoint, inputs){
  
  if(class(treebinr_obj) != "treebinr"){
    stop("Input is not a valid treebinr object")
  }
  
  if(!is.null(inputs) && !is.list(inputs)){
    stop("inputs must be a list object or be null")
  }
  
  counts <- treebinr_obj@counts
  bins <- treebinr_obj@bins
  tree <- treebinr_obj@tree
  
  #Choose the bin with the optimal measure
  binIndex <- selectBin(bins, inputs)
  chosenBin <- bins[[binIndex]]
  
  #Feed it to split
  out <- treeSplit(chosenBin, treebinr_obj, binMeasure, splitBin, makePoint, inputs)
  
  return(out)
  
}