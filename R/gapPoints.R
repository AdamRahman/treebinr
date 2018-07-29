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
gapPoints <- function(bin, inputs){
  point <- apply(bin@contents, 2, mean)
  return(point)
}