\name{mergeListMaker}
\alias{mergeListMaker}
\title{Recursively reassigns ID numers after merging.}
\usage{
  mergeListMaker(toMerge, mergeList = list())
}
\arguments{
  \item{toMerge}{a matrix of adjacent polygons to be
  merged}

  \item{mergeList}{a list}
}
\value{
  List of new identities
}
\description{
  After merging, this assigns the lowest ID number among a
  set of merged polygons to the entire set.
}

