% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trim.R
\name{trim}
\alias{trim}
\title{Trim the Binning Tree}
\usage{
trim(val, treebinr_obj, getMeasure = function(bin, inputs) {     bin@measure
  }, binMeasure, makePoint, updateBin, inputs = NULL)
}
\arguments{
\item{val}{The largest value of measure to be removed}

\item{treebinr_obj}{An object of class treebinr}

\item{getMeasure}{A user supplied function to compute the measure to be compared between bins. By default, returns whatever value is in the measure slot of the bin object.}

\item{binMeasure}{A user supplied function to compute the measure associated with each bin}

\item{makePoint}{A user supplied function to turn the contents of a bin into a single point}

\item{updateBin}{A user supplied function combine several bins into a single bin}

\item{inputs}{A list containing additional input parameters required by user supplied functions}
}
\value{
The return value is an object of class treebinr, which contains the following 
\item{points}{A matrix containing the reduced point configuration}
\item{counts}{A vector containing the number of points in each bin}
\item{bins}{A list containing bin objects, which detail the contents of each bin}
\item{tree}{An undirected graph object for the binning tree}
}
\description{
Given a treebinr object, \code{trim} removes all leaves with measure values less than one specified by the user
}
\examples{
#Bin a Normal Configuration to 500 Points
set.seed(1337)
X <- matrix(rnorm(2000),ncol=2)

numbins <- 500
nCols <- ncol(X)
binMeasure <- gapMeasure
selectBin <- gapSelect
splitBin <- gapSplit
boundaryTest <- gapBoundaryTest
makePoint <- gapPoints

inputs <- list(dimRange = sapply(1:nCols, FUN = function(j) {range(X[,j])}), binsperdim = 10)
binInfo <- list(binRange = sapply(1:nCols, FUN = function(j) {range(X[,j])}))

treebin_obj <- treebin(X, numbins, binMeasure, boundaryTest, selectBin, splitBin, makePoint, binInfo, inputs)

#Trim all values of measure less than 0.01
val <- .01
updateBin <- gapUpdate
getMeasure <- function(bin, inputs){
  max <- max(bin@measure)
  return(max)
}

out <- trim(val, treebin_obj, getMeasure, binMeasure, makePoint, updateBin, inputs)

}
