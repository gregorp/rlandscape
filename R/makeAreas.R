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
