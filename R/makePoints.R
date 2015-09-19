#' Creates a set of random points
#'
#' This is called by rlandscape to generate points. Uses any combination of four methods
#' \itemize{
#'    \item uniform - the x and y coordinates are evenly distributed across the landscape
#'    \item lattice - the points are chosen from the lattice points of a grid overlaid on
#'          the landscape
#'    \item cluster - cluster locations are chosen by the uniform method, then points
#'          are added to each cluster following a bivariate Gaussian distribution
#'    \item SSI - points are chosen by the uniform method with an imposed inhibition
#'          distance so that new points cannot be placed close to existing points
#' }
#'
#' Additional control parameters can be passed to the lattice, cluster and SSI point
#' processes via the \code{control} parameter.
#'
#' @param n a vector of length up to 4 specifying how many points to create by each method:
#' uniform, lattice, cluster, and inhibition (SSI), respectively.
#' @param hAsp a single positive numeric, the horizontal to vertical aspect ratio of the
#' window to create the points in.
#' @param control a list of parameters. The elements include any or all of the following:
#' \itemize{
#'    \item \code{lattice}, a vector of length 2 containing the number of horizontal and
#'          vertical grid lines that lattice points are to be chosen from.
#'    \item \code{cluster}, a vector of length 2 specifying the number of points per cluster
#'          and the cluster spread (standard deviation of a bivariate Gaussian)
#'    \item \code{ssi}, a single numeric, the inhibition distance for the SSI method.
#' }
#' @return Returns coordinates of the points produced in a list of two vectors,
#' \code{x} and \code{y}.
#' @export
makePoints <- function(n = c(100, 0, 0, 0), hAsp = 1, control = list()) {
    ## Check inputs and set defaults
    if (hAsp <= 0) stop("Aspect ratio must be positive")
    
    if (length(n) < 4) n <- c(n, rep(0, 4 - length(n)))
    
    if (is.null(control$lattice) & n[2] > 0) {
        hseg <- ceiling(sqrt(n[2] / hAsp))
        vseg <- ceiling(hseg * hAsp)
        control$lattice <- c(hseg, vseg)
    }
    if (is.null(control$cluster) & n[3] > 0) {
        control$cluster <- c(10, 0.1)
    }
    if (is.null(control$ssi) & n[4] > 0) {
        control$ssi = .639 * sqrt(hAsp / n[4])
    }
    ## Initializing
    x <- vector()
    y <- vector()
    
    ## Method 1: uniform
    if (n[1] > 0) {
        x <- c(x, hAsp * runif(n[1]))
        y <- c(y, runif(n[1]))
    }
    
    ## Method 2: lattice
    if (n[2] > 0) {
        hseg <- control$lattice[1]
        vseg <- control$lattice[2]
        
        newPoints <- rLatticePoints(n[2], seg = c(hseg, vseg))
        x <- c(x, hAsp * newPoints$x)
        y <- c(y, newPoints$y)
    }
    
    # Method 3: cluster
    if (n[3] > 0) {
        n.children <- control$cluster[1]
        spread <- control$cluster[2]
        newPoints <- rCluster(n[3], n.children, spread, hAsp = hAsp)
        x <- c(x, newPoints$x)
        y <- c(y, newPoints$y)
    }
    
    # Method 4: SSI
    # the giveup parameter scales up with number of points
    if (n[4] > 0) {
        newPoints <- rSSI(r = control$ssi, n = n[4],
                          win = owin(c(0, hAsp), c(0, 1)),
                          giveup = 50 * n[4])
        x <- c(x, newPoints$x)
        y <- c(y, newPoints$y)
        # Second chance in case not enough points are placed
        # Though will not inhibit placement near previous points
        if (length(newPoints$x) < n[4]) {
            newPoints <- rSSI(r = control$ssi, n = n[4] - length(newPoints$x),
                              win = owin(c(0, hAsp), c(0, 1)),
                              giveup = 50 * n[4])
            x <- c(x, newPoints$x)
            y <- c(y, newPoints$y)
        }
    }
    
    return(list(x = x, y = y))
}
