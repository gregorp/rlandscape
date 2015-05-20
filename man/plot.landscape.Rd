\name{plot.landscape}
\alias{plot.landscape}
\title{Plotting routine for landscape objects}
\usage{
  \method{plot}{landscape} (x, y, add = FALSE,
    number = FALSE, points = FALSE, cex = 1, nex = 1,
    pch = 1, lty = 1, lineCol = "black",
    pointCol = "gray60", holeCol = "gray20",
    numCol = "gray40", holeNumCol = "gray80", ylim = NULL,
    xlim = NULL, axes = FALSE, main = NULL, tex = 1,
    mar = rep(2, 4), ...)
}
\arguments{
  \item{x}{The landscape to plot. Required.}

  \item{y}{Ignored. Included for compatibility with plot.}

  \item{add}{Boolean, if true the landscape will be added
  to the current plot. Default \code{FALSE}.}

  \item{number}{Boolean, whether or not to number the
  tiles. Default \code{FALSE}.}

  \item{points}{Boolean, whether or not to draw the random
  points that built the tiles. Default \code{FALSE}.}

  \item{cex}{cex for points (if drawn).}

  \item{nex}{cex for numbers (if drawn).}

  \item{pch}{pch for points (if drawn).}

  \item{lty}{lty for borders.}

  \item{lineCol}{Color used for borders, default "black".}

  \item{pointCol}{Color used for points, default "gray60".}

  \item{holeCol}{Fill for deleted tiles (holes). Default
  "gray20".}

  \item{numCol}{Color for numbers, default "gray40".}

  \item{holeNumCol}{Color for numbers of holes, default
  "gray80".}

  \item{ylim}{y limits of plot.}

  \item{xlim}{x limits of plot.}

  \item{axes}{Boolean, whether or not to draw axes. Can be
  useful especially if \code{hAspect} is not 1.}

  \item{main}{Character string to title the plot.}

  \item{tex}{cex for \code{main} argument.}

  \item{mar}{vector of 4 values, margins for the plot.}

  \item{...}{additional parameters passed to
  \code{segments} if for drawing the boundaries.}
}
\value{
  \code{NULL}
}
\description{
  A flexible plotting call for landscpe objects. The only
  required argument is the landscape object. Modified from
  \code{plot.deldir} from the \code{deldir} package.
}
\seealso{
  \link{par}
}

