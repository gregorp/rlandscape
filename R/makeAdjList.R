#' Creates an adjacency list
#'
#' @param edges a data.frame with edge data
#' @param n the number of kept polygons
#' @param merges =merges that occurred
#'
#' @return A list of adjacencies
makeAdjList <- function(edges, n, merges = NULL) {
    merges$realIndex <- with(merges, ifelse(mergedWith == 0, index, mergedWith))
    indices <- unique(merges$realIndex[merges$holeThese == 0])
    adj <- list()
    length(adj) <- length(indices)
    edges$realInd1 <- merges$realIndex[edges$ind1]
    edges$realInd2 <- merges$realIndex[edges$ind2]
    edges <- edges[edges$delete == 0 & edges$holeBorder == 0 & edges$tooShort == 0, ]
    for(i in indices) {
        this.adj <- unique(with(edges, c(realInd2[realInd1 == i], realInd1[realInd2 == i])))
        adj[[i]] <- sort(this.adj)
    }
    return(adj)
}
