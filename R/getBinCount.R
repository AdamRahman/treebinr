#' @export
getBinCount <- function(bin){
  count <- nrow(bin@contents)
  return(count)
}