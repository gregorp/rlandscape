\name{checkLand}
\alias{checkLand}
\title{Checks if stats are within bounds}
\usage{
  checkLand(stats, bounds)
}
\arguments{
  \item{stats}{a named vector (or list or DF) of landscape
  statistics}

  \item{bounds}{a list of 2-elemnt vectors indicating upper
  and lower bounds}
}
\value{
  boolean indicating whether all stats in in bounds or not.
}
\description{
  Bounds taken from environment it is called from.
}

