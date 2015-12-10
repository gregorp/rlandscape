#' Deletes extraneous edges
#'
#' This is run after merging. It recursively checks the landscape
#' for extra edges and deletes them
#'
#' @param edges a data.frame with edge data (from class landscape)
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


edgeClean2 = function(edges) {
    # let's get the edges that are still here
    indices = c(edges[edges$delete == 0, "ind1"], 
                       edges[edges$delete == 0, "ind2"])
    tab = table(indices)
    ind_1 = names(tab)[tab == 1]
    
}

## edges ind1 and ind2 are the VORONOI points the edge forms the boundary between.
## so, one way to clean is to "correct" those edges after deletion
## if an edge between 8 and 3 is deleted, replace all ind = 8 with 3
## To avoid the need of many multipile updates, we'll start with the small
## numbers and go up. For example, if edges between 8 and 3 and between 11 and 8
## are deleted, we'll find the smallest number (3), see that it is merged
## with 8, then change all 8s to 3s. While we have deleted edges still
## 'between' two different indices, we'll then continue to clean.
## Once all indices have been updated, we can delete any edges remaining
## between identical indices.
edgeClean3 = function(edges) {
    
    edges_mat = as.matrix(edges[, -(1:4)])
    
#     minInd = min(edges$ind2)
#     maxInd = max(edges$ind1)
#     mergeKey = cbind(minInd:maxInd, minInd:maxInd)
     
    problemRows = find_problem_edges(edges_mat)
    while(any(problemRows)) {
        
        # ind2 is always < ind1 to start, but this might change
        # after replacements are made
        minIndex = min(c(edges[unequalRows, "ind2"], edges[unequalRows, "ind1"]))
        toReplace = c(edges[unequalRows & edges$ind1 == minIndex, "ind2"],
                      edges[unequalRows & edges$ind2 == minIndex, "ind1"])
        edges[edges$ind1 %in% toReplace, "ind1"] = minIndex
        edges[edges$ind2 %in% toReplace, "ind2"] = minIndex
        
        mergeKey[mergeKey[, 2] %in% toReplace, 2] = minIndex
        
        # update unequal row listing
        problemRows = find_problem_edges(edges_mat)
    }
    edges[edges$ind1 == edges$ind2, "delete"] = 1
    return(list(edges = edges, mergeKey = mergeKey))
}


find_problem_edges = function(edges_mat) {
    edges_mat[, "delete"] == 1 & (edges_mat[, "ind1"] != edges_mat[, "ind2"])
}

edgeWideToLong = function(edges) {
    long = as.data.frame(matrix(
        data = c(c(edges$x1, edges$x2),
          c(edges$y1, edges$y2),
        c(edges$ind1, edges$ind2),
        c(edges$bp1, edges$bp2),
        rep(edges$delete, 2)),
        nrow = NROW(edges) * 2,
        dimnames = list(1:(NROW(edges) * 2), c("x", "y", "ind", "bp", "delete")))
    )
    dplyr::group_by(long, ind) %>%
        summarize(x = first(x), y = first(y),
                  bp = first)
    
}