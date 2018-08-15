#' The S4 class treebinr to represent the data reduction output from tree binning.
#' 
#' @slot points A matrix whose rows represent the points after data reduction.  These
#'       are the locations of the bins in the original space.
#' @slot counts These are the counts in the bin, or the number of original values that
#'       each point now represents
#' @slot bins A vector of bins, one for each of the reduced points.  Each bin itself
#'       an instance of the treebinr class `bin` and so contains much more information
#' @slot tree A large (n-1) by (n-1) matrix containing information necessary to reconstruct
#'       the tree
#' 
#' @export treebinr
treebinr <- setClass("treebinr", slots = c("points", "counts", "bins", "tree"))

#' @export bin
bin <- setClass("bin", slots = c("boundary", "contents", "measure", "index", "info"))
