#' Saves an area list
#'
#' @param keepers data.frame with 4 columns, index, area, x and y.
#' @param filename character, filename (and path) to save to.
#'
#' @return Coefficient of variation, 100 * SD / mean.
#' @export
saveAreas <- function(keepers, filename) {
    ## takes a summary dataframe
    #  keepers must have 4 columns that are "index", "area", "x", and "y"
    write.csv(keepers, file = filename,
              row.names = FALSE,
              col.names = c("index", "area", "x", "y"))
    invisible()
}
