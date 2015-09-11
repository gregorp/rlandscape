#' Recursively reassigns ID numers after merging.
#'
#' After merging, this assigns the lowest ID number among a set of merged polygons
#' to the entire set.
#'
#' @param toMerge a matrix of adjacent polygons to be merged
#' @param mergeList a list
#'
#' @return List of new identities
#' @export
mergeListMaker <- function(toMerge, mergeList = list()) {        
    lowest <- min(toMerge)
    
    ind1Merge <- toMerge[toMerge[, 2] == lowest, 1]
    ind2Merge <- toMerge[toMerge[, 1] == lowest, 2]
    
    mergeWith <- unique(c(ind1Merge, ind2Merge))
    mergeList[[as.character(lowest)]] <- c(mergeList[[as.character(lowest)]], mergeWith)
    
    toMerge[toMerge %in% mergeWith] <- lowest
    toMerge <- toMerge[-which(toMerge[, 1] == toMerge[, 2]), ]
    
    if (!is.matrix(toMerge)) {
        toMerge <- matrix(toMerge, ncol = 2)
    }
    
    
    if (nrow(toMerge) == 0) {
        return(mergeList)
    }
    
    return(mergeListMaker(toMerge, mergeList))    
}
