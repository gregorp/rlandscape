\name{getParamsLm}
\alias{getParamsLm}
\title{Calculates rlandscape parameters using the original lm approach}
\usage{
  getParamsLm(targetMat)
}
\arguments{
  \item{targetMat}{matrix of targets.}
}
\value{
  matrix of parameters for \code{rlandscape}. Column order
  is pHole, pMerge, pLat, pClust, pSSI, children, spread
}
\description{
  targetMat should have column order "n", "degMean",
  "degSD", "areaCV", "hAsp" as in the output of
  \code{makeTargetMat} function.
}

