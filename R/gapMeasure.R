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
#' @references Adam Rahman & Wayne Oldford (2018). Tree-based Binning.
#' 
#' @export
gapMeasure <- function(bin, inputs){
  
  if(nrow(bin@contents) == 1){
    return(matrix(NA, 1, ncol(bin@contents)))
  }
  
  gaps <- apply(bin@contents, 2, getGap)
  
  #Scale the gaps by the number of points in the bin and by tau
  n <- nrow(bin@contents)
  tau <- inputs$tau
  
  gaps <- gaps * n^tau
  
  #Scale the gaps by the dimension range
  dimRange <- matrix(inputs$dimRange, nrow=(n-1), ncol=ncol(bin@contents), byrow=TRUE)
  gaps <- gaps/dimRange
  
  gaps[is.nan(gaps)] <- -Inf    #Any dimension that has no variability should not be split.
  return(gaps)
}

#For Each Dimension, compute the gap statistic
getGap <- function(binDim){
  
  sortedBin <- sort(binDim)
  axis <- sortedBin[-length(sortedBin)]
  axis2 <- sortedBin[-1]
  
  gaps <- abs(axis - axis2)
  
  return(gaps)
}
