\name{rland}
\alias{rland}
\title{Produces landscapes with descriptive statistics within input ranges.}
\usage{
  rland(targets = list(), bounds = list(), reps = 10,
    effort = 0.95, filename = "landscape", saveAdj = TRUE,
    saveAreas = TRUE, saveSummary = TRUE, savePlot = FALSE,
    saveLand = FALSE, setSeed = NULL,
    method = c("linear", "random"), ...)
}
\arguments{
  \item{targets}{a list with min and max targets for any or
  all of the landscape characteristics. Targetable
  characteristics are n, degMean, degSD, areaCV and hAsp.
  Each item should be of the form \code{n = c(nMin, nMax)}.
  It's okay if \code{min == max}.}

  \item{bounds}{a list with min and max allowable values
  for any or all of the landscape characteristics. (Input
  format as \code{targets}. hAsp is ignored as it has no
  associated randomness). Unspecified characteristics will
  be unrestricted. Works quickly if the min and max bounds
  cover the min and max targets with some wiggle room.}

  \item{reps}{the number of landscapes with these
  characteristics to create.}

  \item{effort}{the maximum failure rate to not
  self-terminate. Evaluated every 500 tries, default 0.95.}

  \item{filename}{character string prefix for all saved
  files created. Can include a filepath.}

  \item{saveAdj}{boolean, whether or not to save adjacency
  tables. Default \code{TRUE}}

  \item{saveAreas}{boolean, whether or not to save area
  tables. Default \code{TRUE}}

  \item{saveSummary}{boolean, whether or not to save
  summary statistics. Default \code{TRUE}}

  \item{savePlot}{boolean, whether or not to save plots.
  Default \code{FALSE}}

  \item{saveLand}{boolean, whether or not to save
  landscapes as R objects. Default \code{FALSE}}

  \item{setSeed}{single integer or NULL (default). If a
  value, will set the random seed to that value before
  simulations so that behavior can be reproduced.}

  \item{method}{Charachter, the name of the function that
  takes targets as arguments and retuns parameters for
  \code{rlandscape}. \code{"linear"} and \code{"random"}
  work, other methods may be added in the future. are
  built-in. Defaults to the linear method.}

  \item{...}{Additional parameters passed to \code{plot} if
  applicable.}
}
\value{
  Returns the number of successes and failures. All other
  results are written to file.
}
\description{
  Uses the results from a series of regressions to find
  appropriate control parameters to call \code{rlandscape}
  and produce output landscapes with input characteristics.
  Useful for generating series of landscapes. Any
  descriptive statistics not specified will be allowed to
  vary freely. If not using a complete filepath set your
  working directly appropriately! You may need to increase
  effort (and wait a long time!) if the constraints are
  very narrow. About 0.5, 0.25, and 25 are the minimum
  ranges I'd recommend for degMean, degSD and areaCV,
  respectively. As for the methods, "linear" uses the
  linear models as described in the paper (in review). The
  "random" option picks completely random parameters
  ignoring the targets. It opens the possibility of more
  pathological landscapes. Generalized Additive Models
  might be available soon as a seperate download.
  Currently, the models I have for that are too big to put
  on CRAN.
}
\details{
  See the examples at the bottom! The defaults for targets
  are \code{ list(n = c(30, 120), areaCV = c(40, 70),
  degMean = c(4.5, 5.5), degSD = c(1, 2), hAsp = c(1, 3)) }
  Any of the targets that are not set explicitly will
  revert to these defaults. Note that \code{degMean} must
  be between 2 and 6, \code{degSD} should be positive and
  less than 3, and \code{areaCV} needs to be positive,
  below 40 is small and above 80 is big, though it is
  unbounded above. \code{hAsp} is the horizontal to
  vertical aspect ratio, so a value of 2 is equivalent to a
  90-degree rotation of a value of 1/2.

  Bounds are specified in the same way as targets.
  \code{hAsp} is not eligible for bounds because the
  targets are always hit exactly.

  Use the \code{method = "random"} to ignore the targets
  and bounds and just pick control parameters randomly.
}
\examples{
\dontrun{
rland(reps = 2, setSeed = 47)
rland(reps = 5, method = "random", saveLand = TRUE, savePlot = TRUE)
rland(targets = list(n = c(100, 100), areaCV = c(40, 60)), reps = 5, savePlot = TRUE)
rland(targets = list(n = c(30, 80), degMean = c(5, 5.5), areaCV = c(30, 80), degSD = c(1, 2)),
    bounds = list(n = c(30, 80), degMean = c(4.9, 5.6), areaCV = c(25, 85), degSD = c(0.9, 2.1)))
}
}
\seealso{
  \code{\link{rlandscape}}
}

