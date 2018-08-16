#' The S4 class treebinr to represent the data reduction output from tree binning.
#' 
#' @slot points An m by d matrix whose rows represent the points after data reduction.
#' These are the representative locations for each bin.
#' @slot counts A vector of length m containing the number of points from 
#' the original conguration in each bin.
#' @slot bins Information about each bin in the conguration, returned as an m-element list 
#' containing objects of class bin.
#' @slot tree A directed graph adjacency matrix representing the 
#' binning tree underlying the algorithm.
#' 
#' @export treebinr
treebinr <- setClass("treebinr", slots = c("points", "counts", "bins", "tree"))

#' The S4 class bin to represent bin produced by tree binning.
#' 
#' @slot boundary The defined boundary of the bin. The only requirement here is that this 
#' slot is compatible with the user defined function to test the inclusion of out-of-sample points.
#' @slot contents A matrix whose rows are the coordinates of the original data points in this bin
#' @slot measure The bin score associated with this bin
#' @slot index An internal tracker indicating where the bin is located in the binning tree.
#' @slot info Any additional information that should be associated with the bin.
#' 
#' @export bin
bin <- setClass("bin", slots = c("boundary", "contents", "measure", "index", "info"))
