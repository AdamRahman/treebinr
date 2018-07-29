#' Split the Chosen Bin
#' 
#' \code{ewSplit} splits the bin chosen by \code{ewSelect} by placing a boundary at the midpoint of the two points 
#' with the largest gap measure computed by \code{ewMeasure}
#' 
#' @param bin An object of class bin
#' @param binMeasure A user supplied function to compute the measure associated with each bin
#' @param inputs A list containing additional input parameters required by user supplied functions
#' 
#' @return 
#' \item{newBins}{A list containing two bin objects to be added to the existing list of bins}
#' 
#' @export
ewSplit <- function(bin, binMeasure, inputs){
  
  #Extract the bin information
  currentMeasure <- bin@measure
  currentBin <- bin@contents
  currentBoundary <- bin@info$binRange
  
  #Find the maximum measure
  whichAxis <- which.max(currentMeasure)
  
  #sort the bin along the chosen axis
  sortedBin <- currentBin[order(currentBin[,whichAxis]),]
  
  #Split bin
  splits <- seq(inputs$oRange[1,whichAxis], inputs$oRange[2,whichAxis], length.out = inputs$binsperdim+1)
  
  #make new bins
  newBins <- list()
  
  for(i in 2:(inputs$binsperdim+1)){
    
    #Define new boundary
    newBoundary <- currentBoundary
    newBoundary[1,whichAxis] <- splits[i-1]
    newBoundary[2,whichAxis] <- splits[i]
    
    #Find the points below the split
    index <- which(currentBin[,whichAxis] <= splits[i])
    
    if(length(index) > 0){
      newBin <- bin(boundary = bin@boundary, contents = currentBin[index,,drop=FALSE], info=list(binRange = newBoundary))
      newBin@measure <- ewMeasure(newBin, inputs)
      currentBin <- currentBin[-index,,drop=FALSE]    #Remove the points from the list
    }else{
      newBin <- bin(boundary = bin@boundary, contents = matrix(,0,ncol(currentBin)), info=list(binRange = newBoundary))
      newBin@measure <- ewMeasure(newBin, inputs)
    }
    
    newBins <- unlist(list(newBins, newBin), recursive=FALSE)
  }
  
  return(newBins)
  
}