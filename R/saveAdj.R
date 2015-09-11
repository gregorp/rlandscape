#' Saves an adjacency list
#'
#' @param adj list of adjacencies
#' @param filename character, filename (and path) to save, will have "_adj.txt" appended.
#'
#' @return No return.
#' @export
saveAdj <- function(adj, filename) {
    adjTable <- matrix(data = c(rep(1:length(adj), times = unlist(lapply(adj, length))),
                                unlist(adj)), ncol = 2)
    write.table(adjTable, file = paste(filename, "_adj.txt", sep = ""), sep = "\t",
                row.names = FALSE, col.names = FALSE)
    invisible()
}
