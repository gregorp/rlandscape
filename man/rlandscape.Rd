\name{rlandscape}
\alias{rlandscape}
\title{Creates a landscape object based on direct control parameters.}
\usage{
  rlandscape(n = c(100, 0, 0, 0), hAsp = 1, pMerge = 0.1,
    pHole = 0.1, plot = FALSE, control = list(),
    filename = NULL, ...)
}
\arguments{
  \item{n}{a vector of the numbers of points to pick with
  each method: uniform, lattice, cluster, and SSI,
  respectively. If the vector is of length less than 4 it
  will be filled out with zeroes.}

  \item{hAsp}{numeric, the horizontal to vertical aspect
  ratio of the landscape.}

  \item{pMerge}{numeric between 0 and 1, the proportion of
  tiles in the landscape that have been merged with another
  tile. In practice, pMerge should be less than 0.5 or so.
  See 'Details'.}

  \item{pHole}{numeric between 0 and 1, the proportion of
  tiles in the final landscape that are deleted to become
  holes. In practie, pHole should be less than 0.5 or so.
  See 'Details'.}

  \item{plot}{boolean to indicate whether or not a plot is
  automatically produced. If you save the object returned,
  you can plot it later.}

  \item{control}{a list of control parameters passed to the
  \code{\link{makePoints}} function.}

  \item{filename}{a character string giving the prefix file
  name to be attached to saved output. Can include a file
  path. If \code{NULL} (the default) nothing will be
  saved.}

  \item{...}{Additional arguments will be passed to the
  plot call if appropriate.}
}
\value{
  A \code{landscape} object.
}
\description{
  This function takes parameters such as the number of
  tiles, the proportion of final tiles that are holes and
  merged, control parameters for choosing random points. It
  calls random point processes to create a point patter,
  generates a Voronoi diagram based on the results,
  modifies the diagram as necessary and outputs a landscape
  object. \code{pHole} and \code{pMerge} can be set to any
  value in [0, 1), but larger values in this range will
  increase the runtime and are not recommended in general.
  The number of points picked is scaled up based on
  \code{pHole} and \code{pMerge} values so that the number
  of tiles in the landscape is very close to (usually equal
  to) \code{sum(n)}. The \code{control} list can include
  any or all of the following elements: \itemize{ \item
  \code{lattice}, a vector of length 2 containing the
  number of horizontal and vertical grid lines that lattice
  points are to be chosen from.  \item \code{cluster}, a
  vector of length 2 specifying the number of points per
  cluster and the cluster spread (standard deviation of a
  bivariate Gaussian) \item \code{ssi}, a single numeric,
  the inhibition distance for the SSI method. } The
  \code{makePoints} function has defaults for any control
  parameters not specified.
}
\examples{
myLand <- rlandscape()
myLand$stats
\dontrun{
plot(myLand)
myComplicatedLand <- rlandscape(n = c(20, 80, 40, 10), hAsp = 2, pMerge = 0.3,
    plot = TRUE, control = list(lattice = c(15, 15), ssi = 0.05))
}
}
\seealso{
  \code{\link{rland}}
}

