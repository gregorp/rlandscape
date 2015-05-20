# rland.points.R
# Planar Point Processes

#' Creates a set of random points using up to 4 methods.
#'
#' This is called by rlandscape to generate points.
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
#' @return Returns coordinates of th points produced in a list of two vectors, \code{x} and \code{y}.
makePoints <- function(n = c(100, 0, 0, 0), hAsp = 1, control = list()) {
    # Returns a list of points in the unit square selected random
    # using a variety of methods
    #
    # Arguments:
    # n = c(100, 0, 0, 0)
    #   the number of points to choose using each method
    #   uniform, lattice, rCluster, rSSI, respectively
    #
    # hAsp = 1
    #   the maximum x value (y is always 1)
    # control
    #   control parameters for each of the methods
    #   defaults set within functio
    #   lattice = c(hseg, vseg)
    #       defaults to appropriately scale hSeg given hAsp
    #   cluster = c(number of points per cluster, cluster spread)
    #       will produce as many clusters as necessary, deafault is
    #       10 points per cluster
    #   ssi = inhibition distance
    #       default should be small enough to place all points
    #
    # Value:
    # A list of vectors of x and y coordinates list(x = .., y = ..)
    
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


#' Generates random points by a clustering process
#'
#' Used by makePoints, similar to a Thomas clustering method but with less randomness.
#' Chooses an appropriate number of parent locations randomly
#' Assigns children in terms of their deviations from parent coordinates
#' Assigns each child to a parent as uniform multinomial distribution
#' Culls children placed outside of bounds
#'
#' @param pointsToPlace the number of points to be returned
#' @param n.children number of points per cluster
#' @param spread standard deviation of distance from parents to children
#' @param hAsp horizontal to vertical aspect ratio of window.
#' @return list of x and y coordinates
rCluster <- function(pointsToPlace, n.children, spread = 0.05, hAsp = 1) {
    pointsPlaced <- 0
    n.parents <- round(pointsToPlace / n.children)
    tryCounter <- 0
    
    while(pointsPlaced < pointsToPlace) {
        if (tryCounter > 0.2 * pointsToPlace) stop("Spread value for clustering is too high.")
        parents <- data.frame(x = hAsp * runif(n.parents), y = runif(n.parents))
        childGuesses <- ifelse(tryCounter > 2, 100 * pointsToPlace, 2 * pointsToPlace)
        
        children <- data.frame(parent = ceiling(n.parents * runif(childGuesses)),
                               x.dev = rnorm(childGuesses, sd = spread),
                               y.dev = rnorm(childGuesses, sd = spread),
                               x = NA, y = NA)
        ## calculate children coordinates
        for (par in 1:n.parents) {
            children[children$parent == par, ]$x <-
                children[children$parent == par, ]$x.dev + parents[par, ]$x
            children[children$parent == par, ]$y <-
                children[children$parent == par, ]$y.dev + parents[par, ]$y
        }
        ## cull children outside of window
        children$cull <- with(children,
            ifelse(x < 0 | x > hAsp | y < 0 | y > 1 | is.na(x) | is.na(y), 1, 0))
        children <- children[children$cull == 0, ]
        pointsPlaced <- length(children$x)
        tryCounter <- tryCounter + 1
    }
    
    return(list(x = children$x[1:pointsToPlace], y = children$y[1:pointsToPlace]))
}
