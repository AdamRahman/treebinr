#' Create a Bin from a Number of Pruned Bins
#' 
#' Given a number of bins that we would like to prune, combine them into a single bin and update all relevant bin information
#' 
#' @param bins A list of bin objects to be pruned
#' @param binMeasure A user supplied function to compute the measure associated with each bin
#' @param inputs A list containing additional input parameters required by user supplied functions 
#'
#' @return An updated bin object containing the contents of the pruned bins
#'
#' @export
gapUpdate <- function(bins, binMeasure, inputs){
  
  #Using pruned nodes, update information
  newContents <- c()
  newLowerBounds <- c()
  newUpperBounds <- c()
  
  for(i in 1:length(bins)){
    newContents <- rbind(newContents, bins[[i]]@contents)
    newLowerBounds <- rbind(newLowerBounds, bins[[i]]@info$binRange[1,])
    newUpperBounds <- rbind(newUpperBounds, bins[[i]]@info$binRange[2,])
  }
  
  newBoundary <- rbind(apply(newLowerBounds, 2, min), apply(newUpperBounds, 2, max))
  
  newBin <- bin(boundary = bins[[1]]@boundary, 
                measure=NULL, 
                contents = newContents, 
                index = inputs$node, 
                info=list(binRange = newBoundary))
  
  newBin@measure <- binMeasure(newBin, inputs)
  
  return(newBin)
}