\name{makePoints}
\alias{makePoints}
\title{Creates a set of random points using up to 4 methods.}
\usage{
  makePoints(n = c(100, 0, 0, 0), hAsp = 1,
    control = list())
}
\arguments{
  \item{n}{a vector of length up to 4 specifying how many
  points to create by each method: uniform, lattice,
  cluster, and inhibition (SSI), respectively.}

  \item{hAsp}{a single positive numeric, the horizontal to
  vertical aspect ratio of the window to create the points
  in.}

  \item{control}{a list of parameters. The elements include
  any or all of the following: \itemize{ \item
  \code{lattice}, a vector of length 2 containing the
  number of horizontal and vertical grid lines that lattice
  points are to be chosen from.  \item \code{cluster}, a
  vector of length 2 specifying the number of points per
  cluster and the cluster spread (standard deviation of a
  bivariate Gaussian) \item \code{ssi}, a single numeric,
  the inhibition distance for the SSI method. }}
}
\value{
  Returns coordinates of th points produced in a list of
  two vectors, \code{x} and \code{y}.
}
\description{
  This is called by rlandscape to generate points.
}

