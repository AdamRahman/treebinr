#' Stopping Criteria for the GapBin Algorithm
#' 
#' \code{gapStop} Computes the stopping critera for the GapBin algorithm.
#' 
#' @param bins A list of objects of class bin
#' @param inputs A list containing additional input parameters required by user supplied functions
#' 
#' @return 
#' Returns TRUE if the algorithm should be stopped, FALSE otherwise
#' 
#' @references Adam Rahman & Wayne Oldford (2018). Tree-based Binning.
#' 
#' @export
gapStop <- function(bins, inputs){
  n <- length(bins)
  return((n >= inputs$numbins))
}