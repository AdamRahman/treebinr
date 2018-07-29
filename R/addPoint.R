#' Add a Point to an Existing Configuration
#' 
#' \code{addPoint} adds a point to an existing binned configuration by first finding the bin to which it belongs, and updating the target bin
#' 
#' @param point A vector containing the point to be added to the binned configuration 
#' @param treebinr_obj An object of class treebinr
#' @param binMeasure A user supplied function to compute the measure associated with each bin
#' @param makePoint A user supplied function to turn the contents of a bin into a single point
#' @param inputs A list containing additional input parameters required by user supplied functions. Default is NULL.
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
#' inputs <- list(dimRange = sapply(1:nCols, FUN = function(j) {range(X[,j])}), tau=1)
#' binInfo <- list(binRange = sapply(1:nCols, FUN = function(j) {range(X[,j])}))
#'
#' treebinr_obj <- treebin(X, numbins, binMeasure, boundaryTest, selectBin, splitBin, makePoint, binInfo, inputs)
#' 
#' #Add a Point at (-3,3)
#' point <- c(-3,3)
#' out2 <- addPoint(point, treebinr_obj, binMeasure, makePoint, inputs)
#' 
#' @export
addPoint <- function(point, treebinr_obj, binMeasure, makePoint, inputs){
  
  #Check
  if(!is.numeric(point)){
    stop("point must be a numeric input")
  }
  
  if(length(point) > ncol(treebinr_obj@points)){
    stop("The point to be added must have the same dimension as the binned configuration")
  }
  
  if(class(treebinr_obj) != "treebinr"){
    stop("treebinr_obj is not a valid treebinr object")
  }
  
  if(!is.null(inputs) && !is.list(inputs)){
    stop("inputs must be a list object or be null")
  }
  
  points <- treebinr_obj@points
  counts <- treebinr_obj@counts
  bins <- treebinr_obj@bins
  tree <- treebinr_obj@tree
  
  #Find the bin in which the point belongs
  
  checkBoundary <- function(bin, point, inputs){
    boundaryTest <- bin@boundary
    out <- boundaryTest(point, bin, inputs)
    return(out)
  }
  
  whichBin <- which(sapply(bins, checkBoundary, point=point, inputs=inputs))
  
  if(length(whichBin) == 0){
    stop("Point to be added falls outside the original range of the data. Please adjust the test of boundary.")
  }else{
    chosenBin <- bins[[whichBin]]
  
    #Update Bin
    chosenBin@contents <- rbind(chosenBin@contents, as.numeric(point))
    chosenBin@measure <- binMeasure(chosenBin, inputs)
  
    #Update treebinr_obj
    counts[whichBin] <- counts[whichBin] + 1
    points[whichBin,] <- makePoint(chosenBin, inputs)
    bins[[whichBin]] <- chosenBin
  
    #Remake treebinr_obj
    out <- treebinr(points=points, counts=counts, bins=bins, tree=tree)
  }
  
  return(out)
}