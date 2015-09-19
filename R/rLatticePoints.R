#' lattice point process
#'
#' Chooses n random points from a lattice
#'
#' @param n number of points to choose
#' @param seg vector of 2 integers, the number of horizontal and vertical grid lines, respectively.
#'
#' @return List of n x and y coordinates.
rLatticePoints <- function(n, seg = rep(ceiling(n[2]^.5 * 2), 2)) {
    # Randomly generates lattice points
    #
    # n:    number of points to generate
    # seg:  vector (2), number of horizontal and vertical lines
    
    # Enumerate potential points
    m <- seg[1] * seg[2]
    if(n > m) stop("More points than lattice points")
    M <- matrix(0, m, 2)
    row <- 1
    for (i in 1:seg[1]) {
        for (j in 1:seg[2]) {
            M[row, ] <- c(i, j)
            row <- row + 1
        }
    }
    
    coords <- sample(m, n, replace = FALSE)
    x <- M[coords, 2] 
    y <- M[coords, 1] 
    
    # Scale to fit inside window
    x <- (x - 0.5) / (seg[2])
    y <- (y - 0.5) / (seg[1])
    
    return(list(x = x, y = y))
}
