#' Compute the Equal Width Measure
#' 
#' \code{ewMeasure} computes the Equal Width measure as a measure of dissimilarity within a bin
#' 
#' @param bin An object of class bin
#' @param inputs A list containing additional input parameters required by user supplied functions
#' 
#' @return 
#' \item{measure}{Returns the measure to be associated with the supplied bin object}
#' 
#' @export
ewMeasure <- function(bin, inputs){
  
  #Our measure is the bin with largest range (as a % of original range)

  unscaledMeasure <- bin@info$binRange[2,] - bin@info$binRange[1,]
  
  #Scale by original range
  oRange <- inputs$oRange[2,] - inputs$oRange[1,]
  scaledMeasure <- unscaledMeasure/oRange
  
  return(scaledMeasure)
  
}