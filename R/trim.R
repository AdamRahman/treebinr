#' Trim the Binning Tree
#' 
#' Given a treebinr object, \code{trim} removes all leaves with measure values less than one specified by the user
#' 
#' @param val The largest value of measure to be removed
#' @param treebinr_obj An object of class treebinr
#' @param getMeasure A user supplied function to compute the measure to be compared between bins. By default, returns whatever value is in the measure slot of the bin object.
#' @param binMeasure A user supplied function to compute the measure associated with each bin
#' @param makePoint A user supplied function to turn the contents of a bin into a single point
#' @param updateBin A user supplied function combine several bins into a single bin 
#' @param inputs A list containing additional input parameters required by user supplied functions
#' 
#' @return The return value is an object of class treebinr, which contains the following 
#' \item{points}{A matrix containing the reduced point configuration}
#' \item{counts}{A vector containing the number of points in each bin}
#' \item{bins}{A list containing bin objects, which detail the contents of each bin}
#' \item{tree}{An undirected graph object for the binning tree}
#' 
#' @examples 
#' #Bin a Normal Configuration to 500 Points
#' set.seed(1337)
#' X <- matrix(rnorm(2000),ncol=2)
#'
#' numbins <- 500
#' nCols <- ncol(X)
#' binMeasure <- gapMeasure
#' selectBin <- gapSelect
#' splitBin <- gapSplit
#' boundaryTest <- gapBoundaryTest
#' makePoint <- gapPoints
#'
#' inputs <- list(dimRange = sapply(1:nCols, FUN = function(j) {range(X[,j])}), binsperdim = 10)
#' binInfo <- list(binRange = sapply(1:nCols, FUN = function(j) {range(X[,j])}))
#'
#' treebin_obj <- treebin(X, numbins, binMeasure, boundaryTest, selectBin, splitBin, makePoint, binInfo, inputs)
#' 
#' #Trim all values of measure less than 0.01
#' val <- .01
#' updateBin <- gapUpdate
#' getMeasure <- function(bin, inputs){
#'   max <- max(bin@measure)
#'   return(max)
#' }
#'
#' out <- trim(val, treebin_obj, getMeasure, binMeasure, makePoint, updateBin, inputs)
#' 
#' @export
trim <- function(val,
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
  
  #Find all the bins with measure values below val
  binVals <- sapply(bins, getMeasure, inputs=inputs)
  which.bins <- which(binVals <= val)
  out <- treebinr_obj
  
  while(length(which.bins) > 0){
    
    #Prune the first bin in the index list
    out <- treePrune(bins[[which.bins[1]]], out, binMeasure, makePoint, updateBin, inputs)
    
    #Update
    points <- out@points
    counts <- out@counts
    bins <- out@bins
    tree <- out@tree
    
    #Compute new values and update list
    binVals <- sapply(bins, getMeasure, inputs=inputs)
    which.bins <- which(binVals <= val)
  }
  
  return(out)
  
}