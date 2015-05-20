\name{rCluster}
\alias{rCluster}
\title{Generates random points by a clustering process}
\usage{
  rCluster(pointsToPlace, n.children, spread = 0.05,
    hAsp = 1)
}
\arguments{
  \item{pointsToPlace}{the number of points to be returned}

  \item{n.children}{number of points per cluster}

  \item{spread}{standard deviation of distance from parents
  to children}

  \item{hAsp}{horizontal to vertical aspect ratio of
  window.}
}
\value{
  list of x and y coordinates
}
\description{
  Used by makePoints, similar to a Thomas clustering method
  but with less randomness. Chooses an appropriate number
  of parent locations randomly Assigns children in terms of
  their deviations from parent coordinates Assigns each
  child to a parent as uniform multinomial distribution
  Culls children placed outside of bounds
}

