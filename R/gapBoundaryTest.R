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
gapBoundaryTest <- function(point, bin, inputs){
  
  point <- matrix(point,nrow=1)
  boundary <- bin@info$binRange
  indicator <- sapply(1:ncol(point), testBounds, point=point, lowerBound = boundary[1,], upperBound = boundary[2,])

  if(all(indicator)){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

testBounds <- function(i,point, lowerBound, upperBound){
  out <- (point[i] <= upperBound[i] & point[i] >= lowerBound[i])
  return(out)
}