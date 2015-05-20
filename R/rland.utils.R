# rland.utils.R
# Utilities for rlandscape

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


#' Deletes extraneous edges
#'
#' This is run after merging. It recursively checks the landscape
#' for extra edges and deletes them
#'
#' @param edges a data.frame with edge data (from class landscape)
#'
#' @return A data.frame with extraneous edges marked for deletion.
edgeClean <- function(edges) {
    # Gather the vertices
    vert <- edges[edges$delete != 1 & !edges$bp1, c("x1", "y1")]
    names(vert) <- c("x2", "y2")
    vert <- rbind(vert, edges[edges$delete != 1 & !edges$bp2, c("x2", "y2")])
    names(vert) <- c("x", "y")
    
    # Identify the vertices of degree 1
    vert <- aggregate(vert$x, by = vert, length)
    names(vert)[3] <- "degree"
    degOne <- which(vert$degree == 1)
    
    # if none, return as is
    if (length(degOne) == 0) {
        return(edges)
    } else {
    # else, delete vertices of degree 1
        for (one in degOne) {
            delX <- vert$x[one]
            delY <- vert$y[one]
            delEdge <- which(edges$delete == 0 &
                ( (edges$x1 == delX & edges$y1 == delY) | (edges$x2 == delX & edges$y2 == delY) ) )
            edges$delete[delEdge] <- 1
        }
    }
    # And recurse
    return(edgeClean(edges))
}


#' Recursively reassigns ID numers after merging.
#'
#' After merging, this assigns the lowest ID number among a set of merged polygons
#' to the entire set.
#'
#' @param toMerge a matrix of adjacent polygons to be merged
#' @param mergeList a list
#'
#' @return List of new identities
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
    

#' Turns an adjacency list into a vector of degrees.
#'
#' @param adj list of adjacencies.
#'
#' @return Vector of degrees.
makeDegVec <- function(adj) {
    return(vapply(adj, function(h) ifelse(is.null(h), NA, length(h)), 0))
}


#' Takes a summary matrix (from class landscape), a subset with info on the "good" polygons.
#' Calculates areas.
#'
#' @param summ a summary matrix (from class landscape)
#'
#' @return A matrix with columns index, area, x, and y, one row per "good" polygon
makeAreas <- function(summ) {
    keepers <- which(summ[, 10] == 0 & summ[, 11] == 0)
    keepers <- cbind(keepers, summ[keepers, c(8, 1, 2)])
    dimnames(keepers)[[2]][1:2] <- c("index", "area")
    return(keepers)
}


#' Calculates the coefficient of variation
#'
#' Calculates the coefficient of variation (as a percent)
#'
#' @param x a vector
#'
#' @return Coefficient of variation, 100 * SD / mean.
coeffVar <- function(x) {
    return(100 * sd(x) / mean(x))
}


#' Saves an adjacency list
#'
#' @param adj list of adjacencies
#' @param filename character, filename (and path) to save, will have "_adj.txt" appended.
#'
#' @return NULL
saveAdj <- function(adj, filename) {
    adjTable <- matrix(data = c(rep(1:length(adj), times = unlist(lapply(adj, length))),
                                unlist(adj)), ncol = 2)
    write.table(adjTable, file = paste(filename, "_adj.txt", sep = ""), sep = "\t",
                row.names = FALSE, col.names = FALSE)
}


#' Saves an area list
#'
#' @param keepers data.frame with 4 columns, index, area, x and y.
#' @param filename character, filename (and path) to save to.
#'
#' @return Coefficient of variation, 100 * SD / mean.
saveAreas <- function(keepers, filename) {
    ## takes a summary dataframe
    #  keepers must have 4 columns that are "index", "area", "x", and "y"
    write.csv(keepers, file = filename,
              row.names = FALSE,
              col.names = c("index", "area", "x", "y"))
}