#' Split A Node
#' 
#' Given a bin, \code{split} splits it into two or more bins according to functions provided by the user
#' 
#' @param bin An object of class bin (contained in treebinr_obj) to be split
#' @param treebinr_obj An object of class treebinr
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
treeSplit <- function(bin, treebinr_obj, binMeasure, splitBin, makePoint, inputs){
  
  points <- treebinr_obj@points
  counts <- treebinr_obj@counts
  bins <- treebinr_obj@bins
  tree <- treebinr_obj@tree
  
  isNode <- function(bin, node){
    out <- (bin@index == node)
    return(out)
  }
  
  #Find the bin in the bin set
  binIndex <- which(sapply(bins, isNode, node=bin@index))
  
  #Update
  binToSplit <- bins[[binIndex]]
  bins[[binIndex]] <- NULL
  
  counts <- counts[-binIndex]
  points <- points[-binIndex,]
  
  counter <- bins[[length(bins)]]@index
  
  #split bin
  newBins <- splitBin(binToSplit, binMeasure, inputs)
  
  #Update Tree Size
  numNewBins <- length(newBins)
  
  tree <- rbind(tree, matrix(0, numNewBins, ncol(tree)))
  tree <- cbind(tree, matrix(0, nrow(tree), numNewBins))
  
  index <- cbind(rep(binToSplit@index, numNewBins), seq(counter + 1, counter + numNewBins))
  
  tree[index] <- 1
  
  for(i in 1:length(newBins)){
    newBins[[i]]@index <- counter + 1
    counter <- counter + 1
  }
  
  newPoints <- t(sapply(newBins, makePoint, inputs))
  newCounts <- sapply(newBins, getBinCount)
  
  #Update treebinr object
  points <- rbind(points, newPoints)
  counts <- c(counts, newCounts)
  bins <- unlist(list(bins, newBins), recursive=FALSE)
  
  out <- treebinr(points = points, counts = counts, bins = bins, tree = tree)
  return(out)
}