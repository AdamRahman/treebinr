#' Tree-based Binning
#' 
#' \code{treebin} bins the provided data using the tree-based binning method, as described in Rahman & Oldford (2018).
#' 
#' @param x The point configuration to be binned
#' @param stopCriteria A user supplied function to compute the stopping criteria of the function
#' @param binMeasure A user supplied function to compute the measure associated with each bin
#' @param boundaryTest A user supplied function to test if a given point is contained in a given bin
#' @param selectBin A user supplied function for choosing between bins to be split
#' @param splitBin A user supplied function for splitting a bin
#' @param makePoint A user supplied function to turn the contents of a bin into a single point
#' @param binInfo Additional information to be supplied to the first bin. Default is NULL.
#' @param inputs A list containing additional input parameters required by user supplied functions. Default is NULL.
#' 
#' @return The return value is an object of class treebinr, which contains the following 
#' \item{points}{A matrix containing the reduced point configuration}
#' \item{counts}{A vector containing the number of points in each bin}
#' \item{bins}{A list containing bin objects, which detail the contents of each bin}
#' \item{tree}{An undirected graph object for the binning tree}
#' 
#' @references
#' 
#' @examples
#' set.seed(1324567)
#' X <- data.frame(x = rnorm(2000), y = rnorm(1000))
#' out <- treebin(X, inputs = list(tau = 1, numbins = 500))
#' Xreduced <- as.data.frame(out@points, col.names = c("x", "y"))
#' Xsampled <- X[sample(1:nrow(X), 500, replace = FALSE),]
#' 
#' savePar <- par(mfrow = c(1,3))
#' xlim <- extendrange(X$x); ylim <- extendrange(X$y)
#' plot(X, 
#'      main = paste0("original data (", nrow(X)," points)"),
#'      xlim = xlim, ylim = ylim)
#' plot(Xreduced, 
#'      main = paste0("reduced data (", nrow(Xreduced)," points)"),
#'      xlim = xlim, ylim = ylim)
#' plot(Xsampled, 
#'      main = paste0("sampled data (", nrow(Xsampled)," points)"),
#'      xlim = xlim, ylim = ylim)
#' par(savePar)
#' 
#' @export
treebin <- function(x, 
                    stopCriteria = gapStop, 
                    binMeasure = gapMeasure,
                    boundaryTest = gapBoundaryTest,
                    selectBin = gapSelect,
                    splitBin = gapSplit,
                    makePoint = gapPoints,
                    binInfo = list(binRange = matrix(rep(c(-Inf,Inf), ncol(x)),2,ncol(x))),
                    inputs = list(tau = 1, numbins = floor(nrow(x)/2))){
  
  #Convert the point configuration from whatever it is, to a matrix
  x <- as.matrix(x)
  
  #Preliminary Definitions and Error Checking
  n <- nrow(x)  #number of points in original configuration
  p <- ncol(x)  #numer of dimensions in original configuration
  
  #Add the range of each dimension to the inputs list
  dimRange = sapply(1:ncol(x), FUN = function(j){diff(range(x[,j]))})
  inputs$dimRange <- dimRange
  
  #Check that the matrix is numeric
  if(!is.numeric(x)){
    stop("X must be a numeric matrix.")
  }
  
  #Check that binfo and info are either null or a list object
  if(!is.null(binInfo) && !is.list(binInfo)){
    stop("binInfo must be a list object or be null")
  }
  
  if(!is.null(inputs) && !is.list(inputs)){
    stop("inputs must be a list object or be null")
  }
  
  #Create initial bin
  initialBin <- bin(boundary = boundaryTest,
                    contents = x,
                    measure = NULL,
                    index = 1,
                    info = binInfo)
  
  initialBin@measure <- binMeasure(initialBin, inputs)
  
  #Initialize bin list and relevant counts
  bins <- list(initialBin)
  nbins <- 1
  binCounts <- n
  
  #Tree Matrix
  treeSize <- 1 + 2*(n - 1)   #Worst case scenario is to have to do numbin-1 splits (i.e. each bin is split in 2), which would give 2*number of splits new nodes
  treeIndex <- matrix(0, treeSize, 2)
  indexCount <- 1
  
  #Main while loop  that executes splitting and updating until desired number of bins is reached
  while(!stopCriteria(bins,inputs)){
    
    #Choose the bin with the optimal measure
    binIndex <- selectBin(bins, inputs)

    chosenBin <- bins[[binIndex]]
    bins[[binIndex]] <- NULL   #Remove the chosen bin from the current set of bins
    binCounts <- binCounts[-binIndex]
    
    #Split the chosen bin
    newBins <- splitBin(chosenBin, binMeasure, inputs)     #out will be a list containing at least two binfo objects
   
    numNewBins <- length(newBins)
    
    #Update Tree
    treeIndex[seq(indexCount, indexCount + numNewBins - 1),] <- cbind(rep(chosenBin@index, numNewBins), seq(indexCount+1, indexCount + numNewBins))
    
    #For each new bin, index it
    for(i in 1:numNewBins){
      newBins[[i]]@index <- indexCount + 1
      indexCount <- indexCount + 1
    }
    
    #update lists and counts
    bins <- unlist(list(bins, newBins), recursive=FALSE)
    binCounts <- c(binCounts, sapply(newBins, getBinCount))
    nbins <- nbins + length(newBins) - 1       #updated number of bins equal to old number of bins minus 1 (the one split) plus the number of new bins
  }

  #Trim the tree
  treeIndex <- treeIndex[1:(indexCount-1),]
  
  #Calculate the representative point of each bin
  points <- t(sapply(bins, makePoint, inputs))
 
  #Create the tree graph
  #tree <- treeGraph(index=treeIndex, size=indexCount-1)
  
  tree <- matrix(0, indexCount, indexCount)
  tree[treeIndex] <- 1
  
  #Create treebinr object, and return
  out <- treebinr(points = points, counts = binCounts, bins = bins, tree = tree)
  return(out)
}