\name{rLatticePoints}
\alias{rLatticePoints}
\title{lattice point process}
\usage{
  rLatticePoints(n, seg = rep(ceiling(n[2]^0.5 * 2), 2))
}
\arguments{
  \item{n}{number of points to choose}

  \item{seg}{vector of 2 integers, the number of horizontal
  and vertical grid lines, respectively.}
}
\value{
  List of n x and y coordinates.
}
\description{
  Chooses n random points from a lattice
}

