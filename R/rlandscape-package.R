#' rlandscape: Random Landscape Generation for Harvest Scheduling Models
#' 
#' With the \code{\link{rlandscape}} function you can generate individual landscapes with 
#' explicit control over the control parameters (e.g. hole proportion, merge proportion,
#' number of points placed with each method) used to create the landscape.
#' The \code{link{rland}} function is designed for batch productions where the desired
#' characteristics of the resulting landscapes (e.g. degree mean, area coefficient
#' of variation) are specified. Rland uses previously run regressions to determine 
#' suitable control parameters. Rland can also be used through a graphical user interface (GUI).
#' This can be started by entering \code{rlandGui()} on the R console.
#' 
#' @seealso \code{\link{rlandscape}}, \code{\link{rland}}, \code{\link{plot.landscape}}
#' 
#' @references Gregor Passolt, Miranda J. Fix, and Sandor F. Toth. A Voronoi
#' Tesselation-based Approach to Generate Hypothetical Forest Landscapes.
#' \emph{Canadian Journal of Forest Research}, 2012.
#' \url{http://www.nrcresearchpress.com/doi/abs/10.1139/cjfr-2012-0265}
#'  (2012pks
#' @docType package
#' @aliases rlandscape-package
#' @import deldir
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