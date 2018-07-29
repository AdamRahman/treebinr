#' Stopping Criteria for the Equal Width Binning Algorithm
#' 
#' \code{ewStop} Computes the stopping critera for the Equal Width Binning Algorithm.
#' 
#' @param bins A list of objects of class bin
#' @param inputs A list containing additional input parameters required by user supplied functions
#' 
#' @return 
#' Returns TRUE if the algorithm should be stopped, FALSE otherwise
#' 
#' @export
ewStop <- function(bins, inputs){
  n <- length(bins)
  return((n >= inputs$numbins))
}