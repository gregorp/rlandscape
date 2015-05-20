\name{rland.gui.wrapper}
\alias{rland.gui.wrapper}
\title{A wrapper for Rland with arguments parseable by the GUI-maker}
\usage{
  rland.gui.wrapper(n.target.min = 30, n.target.max = 200,
    n.bounds.min = 0, n.bounds.max = 1000,
    degMean.target.min = 4, degMean.target.max = 5,
    degMean.bounds.min = 0, degMean.bounds.max = 6,
    degSD.target.min = 1, degSD.target.max = 2,
    degSD.bounds.min = 0, degSD.bounds.max = 4,
    areaCV.target.min = 40, areaCV.target.max = 70,
    areaCV.bounds.min = 10, areaCV.bounds.max = 200,
    hAsp.min = 1, hAsp.max = 4,
    Save_adjacencies_areas_and_summary = TRUE,
    save_R_objects = FALSE, savePlot = FALSE,
    filename = "landscape", number_of_landscapes = 10)
}
\arguments{
  \item{n.target.min}{= 30,}

  \item{n.target.max}{= 200,}

  \item{n.bounds.min}{= 0}

  \item{n.bounds.max}{= 1000,}

  \item{degMean.target.min}{= 4}

  \item{degMean.target.max}{= 5,}

  \item{degMean.bounds.min}{= 0,}

  \item{degMean.bounds.max}{= 6,}

  \item{degSD.target.min}{= 1,}

  \item{degSD.target.max}{= 2,}

  \item{degSD.bounds.min}{= 0,}

  \item{degSD.bounds.max}{= 4,}

  \item{areaCV.target.min}{= 40,}

  \item{areaCV.target.max}{= 70,}

  \item{areaCV.bounds.min}{= 10,}

  \item{areaCV.bounds.max}{= 200,}

  \item{hAsp.min}{= 1,}

  \item{hAsp.max}{= 4,}

  \item{Save_adjacencies_areas_and_summary}{= TRUE,}

  \item{save_R_objects}{= FALSE,}

  \item{savePlot}{= FALSE,}

  \item{filename}{= "landscape",}

  \item{number_of_landscapes}{= 10}
}
\value{
  Number of successes and tries.
}
\description{
  A wrapper for Rland with arguments parseable by the
  GUI-maker
}

