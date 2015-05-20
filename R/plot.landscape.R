

#' Plotting routine for landscape objects
#' 
#' A flexible plotting call for landscpe objects. The only required argument is
#' the landscape object. Modified from \code{plot.deldir} from the \code{deldir} package.
#'
#' @param x The landscape to plot. Required.
#' @param y Ignored. Included for compatibility with plot.
#' @param add Boolean, if true the landscape will be added to the current plot.
#' Default \code{FALSE}.
#' @param number Boolean, whether or not to number the tiles.
#' Default \code{FALSE}.
#' @param points Boolean, whether or not to draw the random points that
#' built the tiles. Default \code{FALSE}.
#' @param cex cex for points (if drawn).
#' @param nex cex for numbers (if drawn).
#' @param pch pch for points (if drawn).
#' @param lty lty for borders.
#' @param lineCol Color used for borders, default "black".
#' @param pointCol Color used for points, default "gray60".
#' @param holeCol Fill for deleted tiles (holes). Default "gray20".
#' @param numCol Color for numbers, default "gray40".
#' @param holeNumCol Color for numbers of holes, default "gray80".
#' @param ylim y limits of plot.
#' @param xlim x limits of plot.
#' @param axes Boolean, whether or not to draw axes. Can be useful
#' especially if \code{hAspect} is not 1.
#' @param main Character string to title the plot.
#' @param tex cex for \code{main} argument.
#' @param mar vector of 4 values, margins for the plot.
#' @param ... additional parameters passed to \code{segments} if for drawing the boundaries.
#' @method plot landscape
#' @S3method plot landscape
#' @export
#' @return \code{NULL}
#' @seealso \link{par}
plot.landscape <- function (x,              # landscape to plot
                            y,              # ignored
                            add = FALSE,            # 
                            number = FALSE,         # number tiles?
                            points = FALSE,         # draw points?
                            cex = 1,                # cex for points (if drawn)
                            nex = 1,                # cex for numbers (if drawn)
                            pch = 1,                # pch for points
                            lty = 1,                # lty for borders
                            lineCol = "black",      # line color for borders
                            pointCol = "gray60",    # point color
                            holeCol = "gray20",     # color to shade in deleted tiles (holes)
                            numCol = "gray40",      # number color
                            holeNumCol = "gray80",  # hole number color
                            ylim = NULL,            # normal
                            xlim = NULL,            # normal
                            axes = FALSE,           # whether or not to draw axes
                            main = NULL,            # title
                            tex = 1,                # cex for main
                            mar = rep(2, 4),        # normal
                            ...) {                  # other arguments are passed to segments()
    ## Input Checking
    landscape <- x
    if (!is.null(landscape$landscape)) {
        landscape <- landscape$landscape  # useful for full rlandscape output
    }
    
    plot.rl <- points
    
    dirsgs <- landscape$dirsgs
    X <- landscape$summary[, 1]
    Y <- landscape$summary[, 2]
  
    points.df <- as.data.frame(landscape$summary[, c("x", "y", "holeThese", "mergedWith")])
    points.df$index = 1:dim(points.df)[1]
    
    if (!is.null(dirsgs$delete)) {
        dirsgs <- dirsgs[dirsgs$delete == 0, ]
        X <- X[which(landscape$summary[, 11] == 0)]
        Y <- Y[which(landscape$summary[, 11] == 0)]
    }
    
    rw <- landscape$rw
    u1 <- dirsgs[, 1]
    v1 <- dirsgs[, 2]
    u2 <- dirsgs[, 3]
    v2 <- dirsgs[, 4]
  
    if (!add) {
        pty.save <- par()$pty
        mar.save <- par()$mar
        on.exit(par(pty = pty.save, mar = mar.save))
        
        par(mar = mar)
        if (rw[2] != 1) {
            par(pty = "m")
        } else {
            par(pty = "s") ## forces square plot if square region
        }
        if (is.null(xlim)) 
            xlim <- rw[1:2]
        if (is.null(ylim)) 
            ylim <- rw[3:4]
        plot(0, 0, type = "n", xlim = xlim, ylim = ylim, 
            axes = FALSE, ann = FALSE, xaxs = "i", yaxs = "i")
        if (axes) {
            axis(side = 1)
            axis(side = 2)
        }
        if (!is.null(main)) mtext(main, side = 3, line = 0, cex = tex)
    }
    
    ## borders
    segments(u1, v1, u2, v2, col = lineCol, lty = lty, ...)
    ## Points
    if (plot.rl) {
        point.index <- which(points.df$holeThese == 0 & points.df$mergedWith == 0)
        points(points.df$x[point.index], points.df$y[point.index],
               pch = pch, col = pointCol, cex = cex)
    }
    
    ## numbers
    if (number) {
        keep.index <- points.df$holeThese == 0 & points.df$mergedWith == 0
        text(x = points.df$x[keep.index],
             y = points.df$y[keep.index],
             labels = points.df$index[keep.index],
             cex = nex, col = numCol)
    }
    
    holesExist <- ifelse(is.null(landscape$holeIndex),
                         FALSE, landscape$holeIndex[1] != 0)
    if (holesExist) {
        ## holes
        hole.index <- points.df$holeThese == 1
        for (i in 1:length(landscape$holeIndex)) {
            polygon(landscape$holes[[i]]$x, landscape$holes[[i]]$y, col = holeCol)
        }
        ## hole points
        if (plot.rl) {
            points(x = points.df$x[hole.index],
                   y = points.df$y[hole.index],
                   pch = pch, cex = cex, col = holeNumCol)
        }
        ## hole numbers
        if (number) {
            text(x = points.df$x[hole.index],
                 y = points.df$y[hole.index],
                 labels = points.df$index[hole.index],
                 cex = nex, col = holeNumCol)
        }
    }
    
    do.call(rect, as.list(landscape$rw)[c(1, 3, 2, 4)])
    invisible()
}
