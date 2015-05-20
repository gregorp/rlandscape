#' rlandscape: Random Landscape Generation for Harvest Scheduling Models
#' 
#' With the \code{rlandscape} function you can generate individual landscapes with 
#' explicit control over the control parameters (e.g. hole proportion, merge proportion,
#' number of points placed with each method) used to create the landscape.
#' The \code{rland} function is designed for batch productions where the desired
#' characteristics of the resulting landscapes (e.g. degree mean, area coefficient
#' of variation) are specified. Rland uses previously run regressions to determine 
#' suitable control parameters. Rland can also be used through a graphical user interface (GUI).
#' This can be started by entering \code{rland.gui()} on the R console.
#' code{\link{rlandscape}}, \code{\link{rland}}, \code{\link{plot.landscape}}
#' 
#' @references Gregor Passolt, Miranda J. Fix, and Sandor F. Toth. A Voronoi
#' Tesselation-based Approach to Generate Hypothetical Forest Landscapes.
#' \emph{Canadian Journal of Forest Research}, 2012.
#' \url{http://www.nrcresearchpress.com/doi/abs/10.1139/cjfr-2012-0265}
#'  (2012pks
#' @docType package
#' @aliases rlandscape-package
#' @import deldir
#' @suggests gWidgets
#' @suggests gWidgetsRGtk2
#' @importFrom spatstat rSSI
#' @name rlandscape-package
NULL


# rlandscape: an R package for generating hypothetical forest landscapes
# Copyright (C) 2012 Gregor Passolt
# 
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.


## rlandscape 1.0
## Gregor Passolt
#  Quantitative Ecology and Resource Management (QERM)
#  University of Washington
#
#  under supervision of Prof. Sandor F. Toth
#  2011-2012
#
#################################################
#  Based on a project
#    by Mandi Fix and Gregor Passolt
#    Quantitative Ecology and Resource Management
#    University of Washington
#
#    fix@uw.edu, gregorp@uw.edu
#
#    June 2010
#
#    Functions that enable the creation of forests with randomized 
#    stands and holes using a Voronoi tesselation approach with
#    several options for generating seed points.
#
##################################################
#  Modifications made
#  # Jan 2010
#    Cleaned code, standardized names
#    increased readability
#
#    Added "lattice" method of point selection
#
#  # Summer 2011
#    Implemented edge deletion, revised data structures (elimiated Spatstat's ppp class)
#    Eliminated dependency on package "tripack"
#    Enabled combinations of point patterns. Removed "grid" point pattern (lattice is better)
#    Started work on GUI--incomplete
#
#  # Fall 2011
#    Ran simulations, cleaned handling of control parameters, fixed aspect ratio problems
#    Re-created hole creation to get more predictable number of tiles out
#    Re-wrote clustering point process to reduce variability
#    New plotting routine (based off of the one from package deldir)
#    
#  # Winter 2012
#    fixed bugs in plotting with different aspect ratios, enable adjustable margins
#    Wrote wrapper to hit targets (rland())
#    Analyzed target-hitting ability
#    made GUI for rland
#    documented in roxygen style
#
#  # Summer 2012
#    added GAM fitting routines
#    turned into a package


#' Creates a landscape object based on direct control parameters.
#'
#' This function takes parameters such as the number of tiles, the proportion of
#' final tiles that are holes and merged, control parameters for choosing random
#' points. It calls random point processes to create a point patter,
#' generates a Voronoi diagram based on the results,
#' modifies the diagram as necessary and outputs a landscape object.
#' \code{pHole} and \code{pMerge} can be set to any value in [0, 1), but larger values
#' in this range will increase the runtime and are not recommended in general. The number
#' of points picked is scaled up based on \code{pHole} and \code{pMerge} values so that the
#' number of tiles in the landscape is very close to (usually equal to) \code{sum(n)}.
#' The \code{control} list can include any or all of the following elements:
#' \itemize{
#'    \item \code{lattice}, a vector of length 2 containing the number of horizontal and
#'          vertical grid lines that lattice points are to be chosen from.
#'    \item \code{cluster}, a vector of length 2 specifying the number of points per cluster
#'          and the cluster spread (standard deviation of a bivariate Gaussian)
#'    \item \code{ssi}, a single numeric, the inhibition distance for the SSI method.
#' }
#' The \code{makePoints} function has defaults for any control parameters not specified.
#'
#' @param n a vector of the numbers of points to pick with each method: uniform, lattice,
#' cluster, and SSI, respectively. If the vector is of length less than 4 it will be filled
#' out with zeroes.
#' @param hAsp numeric, the horizontal to vertical aspect ratio of the landscape.
#' @param pMerge numeric between 0 and 1, the proportion of tiles in the landscape that have
#' been merged with another tile. In practice, pMerge should be less than 0.5 or so. See 'Details'.
#' @param pHole numeric between 0 and 1, the proportion of tiles in the final landscape that are
#' deleted to become holes. In practie, pHole should be less than 0.5 or so. See 'Details'.
#' @param plot boolean to indicate whether or not a plot is automatically produced. If you
#' save the object returned, you can plot it later.
#' @param control a list of control parameters passed to the \code{\link{makePoints}} function.
#' @param filename a character string giving the prefix file name to be attached to saved output.
#' Can include a file path. If \code{NULL} (the default) nothing will be saved.
#' @param ... Additional arguments will be passed to the plot call if appropriate. 
#' @return A \code{landscape} object.
#' @export
#' @seealso \code{\link{rland}}
#' @examples
#' myLand <- rlandscape()
#' myLand$stats
#' \dontrun{
#' plot(myLand)
#' myComplicatedLand <- rlandscape(n = c(20, 80, 40, 10), hAsp = 2, pMerge = 0.3,
#'     plot = TRUE, control = list(lattice = c(15, 15), ssi = 0.05))
#' }
rlandscape <- function(n = c(100, 0, 0, 0),
                       hAsp = 1,
                       pMerge = 0.1,
                       pHole = 0.1,
                       plot = FALSE,
                       control = list(),
                       filename = NULL,
                       ... ) {
    ## additional parameter passed to plot.landscape, etc
    ## n = c(n uniform, n lattice, n cluster, n SSI)
    
    ## Input checking
    if (pHole < 0 | pHole >= 1 | pMerge < 0 | pMerge >= 1) stop("pHole and pMerge must be between 0 and 1")
    if (!is.list(control)) stop("Control parameters must be in a list.")
    
    ## How many points to make
    if (length(n) < 4) n <- c(n, rep(0, 4 - length(n)))
    nInput <- n
    ## Correcting for holes and merges
    nHoles <- round(sum(n) * pHole)
    nMerges <- round(sum(n) * pMerge)
    n <- n + round(n / sum(n) * (nHoles + nMerges)) ## holes and merges are evenly distributed
    
    ## Make the points ------------------------------------------
    points <- makePoints(n, hAsp = hAsp, control = control)
    n <- length(points$x)
    
    ## Construct the Voronoi Diagram ----------------------------
    voro <- deldir(points, rw = c(0, hAsp, 0, 1))
    class(voro) <- c("landscape", "deldir")
    edges <- voro$dirsgs
    summ <- as.matrix(voro$summary)

    holeThese <- rep(0, n)
    mergedWith <- rep(0, n)
    summ <- cbind(summ, holeThese, mergedWith)
    
    edges$delete <- 0
    edges$holeBorder <- 0
    ## flag edges of zero length
    edges$tooShort <- with(edges, ifelse(x1 == x2 & y1 == y2, 1, 0))
    voro$holes <- 0
    voro$holeIndex <- 0
    mergesExist <- FALSE
    holesExist <- FALSE
    
    # summ is a matrix with cols
    #  1,  2, ...,     6,       7,                  8,    9,  10,     11
    #  x,  y, ..., *degree*, # boundary intsctns, *area*, -, hole, mergedWith
    
    # edges is a matrix with cols
    #  1,   2,   3,   4,      5,        6,         7,            8,        9
    # vx1, vy1, vx2, vy2, p.index1, p.index2, v1 boundary, v2 boundary, delete
    
    # where hole and delete are binary to indicate whether that tile (edge) is
    # turned into a hole (deleted), and mergedWith gives the index of the tile
    # that this tile is merged with if it's edges are deleted.
    
    ## Make holes -------------------------------------
    if (nHoles > 0) {
        voro$holeIndex <- sample(n, nHoles)
        holeThese[voro$holeIndex] <- 1
        holesExist <- TRUE
        ## indicate holes
        summ[, 10] <- holeThese
        voro$holes <- tile.list(voro)[-which(holeThese == 0)]
        edges$holeBorder[which(edges$ind1 %in% voro$holeIndex)] <- 1
        edges$holeBorder[which(edges$ind2 %in% voro$holeIndex)] <- 1
    }
    
    ## Delete edges --------------------------------------
    if (nMerges > 0) {
        n.edges <- dim(edges)[1]
        delThese <- sample(which(edges$holeBorder == 0 & edges$tooShort == 0), nMerges)
        mergesExist <- TRUE
        edges$delete[delThese] <- 1
        
        ## Get rid of extraneous edges (if more than 2 cells are merged together)
        edges <- edgeClean(edges)
        
        ## Merge cells
        mergeList <- mergeListMaker(as.matrix(edges[delThese, c("ind1", "ind2")]))
        for (i in 1:length(mergeList)) {
            mergedWith[mergeList[[i]]] <- as.numeric(names(mergeList)[i])
        }
        
        summ[, 11] <- mergedWith
        ## Area: for each remaining merged with tile ()
        
        areas <- tapply(summ[mergedWith != 0, "dir.area"], mergedWith[mergedWith != 0], sum)
              for (i in length(areas)) {
            summ[as.numeric(names(areas)[i]), 8] <- areas[i]
        }
    }
    mergeSummary <- as.data.frame(summ[, c("x", "y", "holeThese", "mergedWith")])
    mergeSummary$index <- 1:dim(mergeSummary)[1]
 
    
    voro$dirsgs <- edges
    voro$summary <- summ
    ## for area calculations...
    keepers <- makeAreas(summ)
    
    ## Plot
    if (plot) plot(voro, ...)
    
    ## Prepare output
    summ.df <- as.data.frame(summ)
    nFinal <- length(summ.df[summ.df$holeThese == 0 & summ.df$mergedWith == 0, ]$x)
    
    adj <- makeAdjList(voro$dirsgs[voro$dirsgs$delete == 0 &
                                   voro$dirsgs$holeBorder == 0 &
                                   voro$dirsgs$tooShort == 0, ],
                       nFinal, merges = mergeSummary)
    
    degVec <- makeDegVec(adj)
    stats <- c(nFinal, mean(degVec, na.rm = TRUE), sd(degVec, na.rm = TRUE),
               coeffVar(keepers[, 2]))
    names(stats) <- c("n output", "degree mean", "degree sd",
                      "area CV")
    statsDF <- data.frame("nOut" = stats[1],
                          "degMean" = stats[2],
                          "degSD" = stats[3],
                          "areaCV" = stats[4],
                          "hAsp" = hAsp,
                          "n1" = nInput[1],
                          "n2" = nInput[2],
                          "n3" = nInput[3],
                          "n4" = nInput[4],
                          "pMerge" = pMerge,
                          "pHole" = pHole)
    if (length(control) >  0) statsDF <- cbind(statsDF, data.frame(t(unlist(control))))
    row.names(statsDF) <- NULL
    
    voro$nInput <- nInput
    voro$stats <- statsDF
    voro$adjacencies <- adj
    ## Output
    if (!is.null(filename)) {
        statsDF$file <- filename
        save(voro, file = paste(filename, "Rdata", sep = "."))
        
        write.csv(statsDF, file = paste(filename, "summary.csv", sep = "_"),
                  row.names = FALSE)
        
        ## Write the adjacency table
        saveAdj(adj = adj, filename = filename)
        
        ## Write the area table
        saveAreas(keepers = keepers, filename = paste(filename, "_area.txt", sep = ""))
        
    } ## End output
    
    return(voro)
}

                     