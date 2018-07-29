#' Prune a Bin
#' 
#' Given a bin, \code{prune} combines all of the bins sister bins into a single node.
#' 
#' @param bin bin An object of class bin (contained in treebinr_obj) to be pruned
#' @param treebinr_obj An object of class treebinr
#' @param binMeasure A user supplied function to compute the measure associated with each bin
#' @param makePoint A user supplied function to turn the contents of a bin into a single point
#' @param updateBin A user supplied function to produce a single bin given the pruned bins
#' @param inputs A list containing additional input parameters required by user supplied functions
#' 
#' @return Returns an updated treebinr object containing the newly pruned bins
#' \item{points}{A matrix containing the reduced point configuration}
#' \item{counts}{A vector containing the number of points in each bin}
#' \item{bins}{A list containing bin objects, which detail the contents of each bin}
#' \item{tree}{An undirected graph object for the binning tree}
#' 
#' @export
treePrune <- function(bin, treebinr_obj, binMeasure, makePoint, updateBin, inputs){
  
  #Check
  if(class(treebinr_obj) != "treebinr"){
    stop("Input is not a valid treebinr object")
  }
  if(class(bin) != "bin"){
    stop("Input is not a valid bin object")
  }
  
  if(!is.null(inputs) && !is.list(inputs)){
    stop("inputs must be a list object or be null")
  }
  
  points <- treebinr_obj@points
  counts <- treebinr_obj@counts
  bins <- treebinr_obj@bins
  tree <- treebinr_obj@tree
  
  #Find the parent of the bin to be pruned
  node <- which(tree[,bin@index] == 1)   #Find the parent node
  
  #Find all nodes to which the pruned node is a parent/grandparent/etc
  isParent <- which(tree[node,] == 1)
  matriarch <- isParent

  getParents <- function(kid, tree){
    return(which(tree[kid,] == 1))
  }
  
  while(any(rowSums(tree[isParent,]) > 0)){
    #Which nodes have their own child nodes
    notTerminal <- which(rowSums(tree[isParent,]) > 0)
    hasKids <- isParent[notTerminal]
    
    #update the parent set
    isParent <- isParent[-notTerminal]
    newParents <- c(sapply(hasKids,getParents,tree=tree))
    
    isParent <- c(isParent, newParents)
    matriarch <- c(matriarch, newParents)
  }
  
  findBins <- function(bin, index){
    out <- bin@index == index
    return(out)
  }
  
  isParentBins <- c()
  for(i in 1:length(isParent)){
    isParentBins <- c(isParentBins, which(sapply(bins, findBins, index = isParent[i])))
  }
  
  #Extract Information on nodes to be pruned
  prunedCounts <- counts[isParentBins]
  prunedPoints <- points[isParentBins,]
  prunedBins <- bins[isParentBins]
  
  numPruned <- length(isParentBins)
  
  inputs$node <- node
  newBin <- updateBin(prunedBins, binMeasure, inputs)

  newCount <- sum(prunedCounts)
  newPoint <- makePoint(newBin, inputs)
  
  #update treebinr object & return
  bins <- bins[-isParentBins]
  bins <- unlist(list(bins, newBin), recursive=FALSE)
  
  counts <- counts[-isParentBins]
  counts <- c(counts, newCount)
  
  points <- points[-isParentBins,,drop=FALSE]
  points <- rbind(points, newPoint)
  
  tree[node,] <- 0    #The newly formed bin is a terminal node
  tree <- tree[-matriarch,,drop=FALSE]
  tree <- tree[,-matriarch,drop=FALSE]
  
  #With rows removed, need to clean up the indexing in the bins
  cleanIndex <- function(bin, removedIndices){
    bin@index <- bin@index - length(which(removedIndices < bin@index))
    return(bin)
  }
  
  bins <- sapply(bins, cleanIndex, removedIndices = matriarch)

  out <- treebinr(points = points, counts = counts, bins = bins, tree = tree)
  return(out)
}