#' A wrapper for Rland with arguments parseable by the GUI-maker
#' 
#' 
#' 
#' @param n.target.min = 30, 
#' @param n.target.max = 200, 
#' @param n.bounds.min = 0
#' @param n.bounds.max = 1000, 
#' @param degMean.target.min = 4
#' @param degMean.target.max = 5, 
#' @param degMean.bounds.min = 0,
#' @param degMean.bounds.max = 6, 
#' @param degSD.target.min = 1, 
#' @param degSD.target.max = 2, 
#' @param degSD.bounds.min = 0, 
#' @param degSD.bounds.max = 4, 
#' @param areaCV.target.min = 40, 
#' @param areaCV.target.max = 70, 
#' @param areaCV.bounds.min = 10, 
#' @param areaCV.bounds.max = 200, 
#' @param hAsp.min = 1, 
#' @param hAsp.max = 4, 
#' @param Save_adjacencies_areas_and_summary = TRUE, 
#' @param save_R_objects = FALSE, 
#' @param savePlot = FALSE, 
#' @param filename = "landscape", 
#' @param number_of_landscapes = 10
#' @return Number of successes and tries.
rland.gui.wrapper <- function(n.target.min = 30, n.target.max = 200, 
                             n.bounds.min = 0 , n.bounds.max = 1000, 
                             degMean.target.min = 4, degMean.target.max = 5, 
                             degMean.bounds.min = 0, degMean.bounds.max = 6, 
                             degSD.target.min = 1, degSD.target.max = 2, 
                             degSD.bounds.min = 0, degSD.bounds.max = 4, 
                             areaCV.target.min = 40, areaCV.target.max = 70, 
                             areaCV.bounds.min = 10, areaCV.bounds.max = 200, 
                             hAsp.min = 1, hAsp.max = 4, 
                             Save_adjacencies_areas_and_summary = TRUE, 
                             save_R_objects = FALSE, 
                             savePlot = FALSE, 
                             filename = "landscape", 
                             number_of_landscapes = 10) {
    return()
}
#     rland(targets = list(n = c(n.target.min, n.target.max), 
#                           degMean = c(degMean.target.min, degMean.target.max), 
#                           degSD = c(degSD.target.min, degSD.target.max), 
#                           areaCV = c(areaCV.target.min, areaCV.target.max), 
#                           hAsp = c(hAsp.min, hAsp.max)), 
#           bounds = list(n = c(n.bounds.min, n.bounds.max), 
#                         degMean = c(degMean.bounds.min, degMean.bounds.max), 
#                         degSD = c(degSD.bounds.min, degSD.bounds.max), 
#                         areaCV = c(areaCV.bounds.min, areaCV.bounds.max)), 
#            reps = number_of_landscapes, 
#            filename = filename, 
#            saveAdj = Save_adjacencies_areas_and_summary, 
#            saveAreas = Save_adjacencies_areas_and_summary, 
#            saveSummary = Save_adjacencies_areas_and_summary, 
#            savePlot = savePlot, 
#            saveLand = save_R_objects)
#}

#' Starts the GUI window for using rland.
#' 
#' The the help page for \code{rland} to understand the options.
#' @export
#' @return None.
rland.gui <- function() {
    return()
    }
#     if(!require(gWidgets, quietly = TRUE)) {
#         stop(paste0("To use the GUI the gWiddgets package must be installed.\n",
#                     "Try install.packages(\"gWidgets\")"))
#     }
#     if(!require(gWidgetsRGtk2, quietly = TRUE)) {
#         stop(paste0("To use the GUI the RGtk2 package must be installed.\n",
#              "Try install.packages(\"RGtk2\")"))
#     }
#     options("guiToolkit" = "RGtk2")
#     cat("Ignore the following message about variableType:\n    ")
#     rland.gui.list <- svalue(ggenericwidget("rland.gui.wrapper"))
#     rland.gui.list$variableType <- NULL
#     ggenericwidget(rland.gui.list, container = gwindow("Rland"))
# }
# 
# 
