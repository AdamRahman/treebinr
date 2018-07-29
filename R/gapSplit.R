#' Split the Chosen Bin
#' 
#' \code{gapSplit} splits the bin chosen by \code{gapSelect} by placing a boundary at the midpoint of the two points 
#' with the largest gap measure computed by \code{gapMeasure}
#' 
#' @param bin An object of class bin
#' @param binMeasure A user supplied function to compute the measure associated with each bin
#' @param inputs A list containing additional input parameters required by user supplied functions
#' 
#' @return 
#' \item{newBins}{A list containing two bin objects to be added to the existing list of bins}
#' 
#' @export
gapSplit <- function(bin, binMeasure, inputs){
  
  #Extract the bin information
  currentMeasure <- bin@measure
  currentBin <- bin@contents
  currentBoundary <- bin@info$binRange

  #Find the maximum measure in each dimension, and take the overall maximum as the dimension to be split
  whichAxis <- which.max(apply(currentMeasure,2,max))
  
  #sort the bin along the chosen axis
  sortedBin <- currentBin[order(currentBin[,whichAxis]),,drop=FALSE]
  
  #Split the sorted bin at the maximum gap
  gapIndex <- which.max(currentMeasure[,whichAxis])
  
  leftContents <- sortedBin[seq(1, gapIndex, 1),,drop=FALSE]
  rightContents <- sortedBin[seq(gapIndex + 1, nrow(currentBin), 1),,drop=FALSE]
  
  #Define the new bin boundaries
  newBoundary <- (max(leftContents[,whichAxis]) + min(rightContents[,whichAxis]))/2   #Split at the midpoint of the maximum gap
  
  leftBoundary <- currentBoundary
  leftBoundary[2,whichAxis] <- newBoundary #The boundary of the left leaf is the same as the parent, except a new maximum along the split dimension
  
  rightBoundary <- currentBoundary
  rightBoundary[1,whichAxis] <- newBoundary  #The boundary of the right leaf is the same as the parent, except a new minimum along the split dimension
  
  #Put together the new bins in a binfo object
  leftBin <- bin(boundary = bin@boundary, contents = leftContents, measure = NULL, info = list(binRange = leftBoundary))
  rightBin <- bin(boundary = bin@boundary, contents = rightContents, measure = NULL, info = list(binRange = rightBoundary))
  
  #Compute the new bin measures
  leftBin@measure <- binMeasure(leftBin, inputs)
  rightBin@measure <- binMeasure(rightBin, inputs)
  
  #Return the new bins in a list
  return(list(leftBin, rightBin))
  
}