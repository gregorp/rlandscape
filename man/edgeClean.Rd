\name{edgeClean}
\alias{edgeClean}
\title{Deletes extraneous edges}
\usage{
  edgeClean(edges)
}
\arguments{
  \item{edges}{a data.frame with edge data (from class
  landscape)}
}
\value{
  A data.frame with extraneous edges marked for deletion.
}
\description{
  This is run after merging. It recursively checks the
  landscape for extra edges and deletes them
}

