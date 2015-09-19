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
