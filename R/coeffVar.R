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
