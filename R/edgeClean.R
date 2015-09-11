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
