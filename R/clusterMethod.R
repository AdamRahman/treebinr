#' Boundary Test for the GapBin Method
#' 
#' Tests whether a given point is contained in the bin boundary produced by the GapBin method
#' 
#' @param point A point to be tested to see if it's inside the provided bin object
#' @param bin A bin object
#' @param inputs A list containing additional input parameters required by user supplied functions
#' 
#' @return Returns a value of TRUE if the point belongs in the bin, FALSE otherwise
#' 
#' @export
clusterBoundaryTest <- function(point, bin, inputs){
  
  point <- matrix(point,nrow=1)
  boundary <- bin@info$binRange
  indicator <- c()
  
  for(i in 1:ncol(point)){
    indicator <- c(indicator, (point[i] < boundary[2,i] & point[i] > boundary[1,i]))
  }
  
  if(all(indicator)){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

#' Compute the Gap Measure of Rahman & Oldford
#' 
#' \code{gapMeasure} computes the Gap measure of Rahman & Oldford (2018) as a measure of dissimilarity within a bin
#' 
#' @param bin An object of class bin
#' @param inputs A list containing additional input parameters required by user supplied functions
#' 
#' @return 
#' \item{measure}{Returns the measure to be associated with the supplied bin object}
#' 
#' @references Radchenko et. al. Convex Clustering via L1 fusion penalization. JRSS-B (2017).
#'  
#' @export
clusterMeasure <- function(bin, inputs){
  #Find the cluster that results in the biggest difference in cluster mean
  
  if(nrow(bin@contents) == 1){
    measure <- 0 #If there is only one point in the bin, there can only be one cluster
    return(measure)
  }
  
  getBiggestClusterDiff <- function(bin){
    bin <- sort(bin)
    n <- length(bin)
    
    C1 <- bin[1]
    C2 <- bin[2:n]
    
    meanC1 <- mean(C1)
    meanC2 <- mean(C2)
    
    maxDiff <- meanC2 - meanC1
    
    for(i in 2:(n-1)){
      C1 <- bin[1:i]
      C2 <- bin[(i+1):n]
      
      meanC1 <- mean(C1)
      meanC2 <- mean(C2)
      
      maxDiff <- c(maxDiff, (meanC2-meanC1))
    }
    
    return(maxDiff)
  }
  
  maxDiff <- apply(bin@contents,2,getBiggestClusterDiff) 
  
  return(maxDiff)
}

#' Convert Bins into a Reduced Point Configuration
#' 
#' Given a bin object, reduce to a single point to make up the final point configuration
#' 
#' @param bin A bin object
#' @param inputs A list containing additional input parameters required by user supplied functions
#' 
#' @return A single point to be included in the final configuration
#' 
#' @export
clusterPoints <- function(bin, inputs){
  point <- apply(bin@contents, 2, mean)
  return(point)
}

#' Select the Bin to be Split
#' 
#' \code{clusterSelect} uses the measures computed using \code{clusterMeasure} to determine which of 
#' the bins should be chosen for splitting.
#' 
#' @param bins A list containing bin objects
#' @param inputs A list containing additional input parameters required by user supplied functions
#' 
#' @return 
#' \item{Index}{The index number of the chosen bin to split}
#' 
#' @export
clusterSelect <- function(bins, inputs){
  #Find the maximum gap in each bin
  findMax <- function(bin){
    max <- max(bin@measure)
    return(max)
  }
  
  gapMax <- sapply(bins, findMax)
  
  #Find the bin with the largest of all gaps (in the event of a tie, take the first bin)
  binIndex <- which.max(gapMax)
  
  return(binIndex)
}

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
clusterSplit <- function(bin, binMeasure, inputs){
  
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
clusterUpdate <- function(bins, binMeasure, inputs){
  
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

#' Stopping Criteria for the Clustered Binning Algorithm
#' 
#' \code{clusterStop} Computes the stopping critera for the Clustered Binning Algorithm.
#' 
#' @param bins A list of objects of class bin
#' @param inputs A list containing additional input parameters required by user supplied functions
#' 
#' @return 
#' Returns TRUE if the algorithm should be stopped, FALSE otherwise
#' 
#' @export
clusterStop <- function(bins, inputs){
  n <- length(bins)
  return((n >= inputs$numbins))
}