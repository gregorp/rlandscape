#' Turns an adjacency list into a vector of degrees.
#'
#' @param adj list of adjacencies.
#'
#' @return Vector of degrees.
#' @export
makeDegVec <- function(adj) {
    return(vapply(adj, function(h) ifelse(is.null(h), NA, length(h)), 0))
}
