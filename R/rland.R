#' Produces landscapes with descriptive statistics within input ranges.
#'
#' Uses the results from a series of regressions to find appropriate control
#' parameters to call \code{rlandscape} and produce output landscapes
#' with input characteristics. Useful for generating series of landscapes.
#' Any descriptive statistics not specified will be allowed to vary freely.
#' If not using a complete filepath set your working directly appropriately!
#' You may need to increase effort (and wait a long time!) if the constraints
#' are very narrow. About 0.5, 0.25, and 25 are the minimum ranges I'd recommend
#' for degMean, degSD and areaCV, respectively.
#' As for the methods, "linear" uses the linear models as described in the paper
#' (in review). The "random" option picks completely random
#' parameters ignoring the targets. It opens the possibility of more pathological landscapes.
#' Generalized Additive Models might be available soon as a seperate download. Currently,
#' the models I have for that are too big to put on CRAN.
#' 
#' See the examples at the bottom! The defaults for targets are
#' \code{
#' list(n = c(30, 120),
#'     areaCV = c(40, 70),
#'     degMean = c(4.5, 5.5),
#'     degSD = c(1, 2),
#'     hAsp = c(1, 3))
#' }
#' Any of the targets that are not set explicitly will revert to these defaults.
#' Note that \code{degMean} must be between 2 and 6, \code{degSD} should be positive and
#' less than 3, and \code{areaCV} needs to be positive, below 40 is small and above 80 is big,
#' though it is unbounded above. \code{hAsp} is the horizontal to vertical aspect ratio, so
#' a value of 2 is equivalent to a 90-degree rotation of a value of 1/2.
#' 
#' Bounds are specified in the same way as targets. \code{hAsp} is not eligible for bounds
#' because the targets are always hit exactly.
#' 
#' Use the \code{method = "random"} to ignore the targets and bounds and just pick
#' control parameters randomly.
#'
#' @param targets a list with min and max targets for any or all of the landscape characteristics.
#' Targetable characteristics are n, degMean, degSD, areaCV and hAsp.
#' Each item should be of the form \code{n = c(nMin, nMax)}. It's okay if \code{min == max}.
#' @param bounds a list with min and max allowable values for any or all of the landscape characteristics.
#' (Input format as \code{targets}. hAsp is ignored as it has no associated randomness).
#' Unspecified characteristics will be unrestricted.
#' Works quickly if the min and max bounds cover the min and max targets with some wiggle room.
#' @param reps the number of landscapes with these characteristics to create.
#' @param effort the maximum failure rate to not self-terminate. Evaluated every
#' 500 tries, default 0.95.
#' @param filename character string prefix for all saved files created.
#' Can include a filepath.
#' @param saveAdj boolean, whether or not to save adjacency tables.
#' Default \code{TRUE}
#' @param saveAreas boolean, whether or not to save area tables.
#' Default \code{TRUE}
#' @param saveSummary boolean, whether or not to save summary statistics.
#' Default \code{TRUE}
#' @param savePlot boolean, whether or not to save plots.
#' Default \code{FALSE}
#' @param saveLand boolean, whether or not to save landscapes as R objects.
#' Default \code{FALSE}
#' @param setSeed single integer or NULL (default). If a value, will set
#' the random seed to that value before simulations so that behavior can be reproduced.
#' @param method Charachter, the name of the function that takes targets as arguments
#' and retuns parameters for \code{rlandscape}. \code{"linear"}
#' and \code{"random"} work, other methods may be added in the future.
#' are built-in. Defaults to the linear method.
#' @param ... Additional parameters passed to \code{plot} if applicable.
#' @export
#' @return Returns the number of successes and failures. All other results
#' are written to file.
#' @seealso \code{\link{rlandscape}}
#' @examples
#' \dontrun{
#' rland(reps = 2, setSeed = 47)
#' rland(reps = 5, method = "random", saveLand = TRUE, savePlot = TRUE)
#' rland(targets = list(n = c(100, 100), areaCV = c(40, 60)), reps = 5, savePlot = TRUE)
#' rland(targets = list(n = c(30, 80), degMean = c(5, 5.5), areaCV = c(30, 80), degSD = c(1, 2)),
#'     bounds = list(n = c(30, 80), degMean = c(4.9, 5.6), areaCV = c(25, 85), degSD = c(0.9, 2.1)))
#' }
rland <- function(targets = list(),
                    bounds = list(),
                    reps = 10,              ## number of landscapes produced
                    effort = 0.95,          ## maximum allowable failure rate
                    filename = "landscape", ## can include a file path
                    saveAdj = TRUE,         ## save the adjacency table
                    saveAreas = TRUE,       ## save csv containing areas and positions of each tile
                    saveSummary = TRUE,     ## save a csv of summary statistics of each landscape
                    savePlot = FALSE,       ## save a plot of each landscape
                    saveLand = FALSE,       ## save the landscapes as R objects
                    setSeed = NULL,         ## seed the RNG for reproducibility
                    method = c("linear", "random"),
                    ...                     ## passed to plot if savePlot = T
                   ) {
    ## Input handling
    method <- match.arg(method) ## Match the method argument, then rename it to the function to use.
    if (method == "linear") method <- "getParamsLm"
    if (method == "random") method <- "getParamsRandom"
    
    if (!is.null(setSeed)) set.seed(setSeed)
    tryGuess <- 5 * reps
    
    boundNames <- c("n", "areaCV", "degMean", "degSD")
    targetDefaults <- list(n = c(30, 120),
                           areaCV = c(40, 70),
                           degMean = c(4.5, 5.5),
                           degSD = c(1, 2),
                           hAsp = c(1, 3))

    for (i in boundNames) {
        if (is.null(bounds[[i]])) bounds[[i]] <- c(0, Inf)
    }
    
    for (i in c(boundNames, "hAsp")) {
        if (is.null(targets[[i]])) targets[[i]] <- targetDefaults[[i]]
    }
    
    if (any(unlist(lapply(targets, function(x) length(x) != 2)))) {
        stop("Error: targets must include both a min and a max (they can be equal).")
    }
    
    if (any(targets$n[2] < targets$n[1],
            targets$degMean[2] < targets$degMean[1],
            targets$degSD[2] < targets$degSD[1],
            targets$areaCV[2] < targets$areaCV[1],
            targets$hAsp[2] < targets$hAsp[1])) {
        stop("Error: please input ranges in c(lower boud, upper bound) form.")
    }
    
    if (targets$degMean[2] > 6) print("Warning: Asymptotically, 6 is the maximum possible degree mean.")
    if (bounds$degMean[2] < 2.5) stop("Error: cannot reliably produce degree means < 2.5.")
    if (targets$degMean[1] > 5.6) print("Warning: it is difficult to prodce degree means close to 6.")
    if (bounds$degMean[1] > 5.9) stop("Error: minimum degree mean is too high.")
    if (targets$degMean[1] < 2.5) print("Warning: it is difficult to produce degree means < 2.5.")
    
    if (targets$degSD[2] > 3) print("Warning: it is difficult produce degree SD > 3.")
    if (bounds$degSD[2] < 1.5) stop("Error: cannot reliably produce degree SD < 1.5.")
    if (targets$degSD[1] < 1) print("Warning: it is difficult to produce degree SD < 1.")
    if (bounds$degSD[1] > 2.75) stop("Error: cannot reliably produce degree SD > 2.75.")
    
    if (bounds$areaCV[2] < 20) stop("Warning, it is difficult to produce area CV < 20.")
    if (targets$areaCV[1] < 20) print("Warning, it is difficult to produce area CV < 20.")
    if (targets$areaCV[1] > 200) print("Warning, it is difficult to produce area CV > 200.")
    
    if (bounds$n[2] < 2) stop("Error: must produce a landscape of more than one tile.")
    if (bounds$n[2] - bounds$n[1] < 1) {
        print("Warning: simulations will go much faster if a small tolerance in n is allowed.")
    }
    
    ##############
    ## Getting on with simulation
    ## Set up loop
    successes <- 0
    tries <- 0
    targetIndex <- 0
    
    ## generates file IDs with leading zeros as necessary
    suffix <- sprintf(paste0("%0", nchar(reps), "d"), 1:reps)
    fullName <- paste0(filename, suffix)
    
    summaryDF <- data.frame(file = fullName, ## Only successes will go in the summary
                            nOut = NA,
                            degMean = NA,
                            degSD = NA,
                            areaCV = NA,
                            hAsp = NA,
                            n1 = NA,
                            n2 = NA,
                            n3 = NA,
                            n4 = NA,
                            pMerge = NA,
                            pHole = NA)
    
    ## The loop for generating landscapes
    while (successes < reps) {
        if(tries %% tryGuess == 0) {
            targetMat <- makeTargetMat(targets = targets, tryGuess = tryGuess)
            controlMat <- do.call(method, args = list(targetMat = targetMat))
            ## thisControl is a matrix with columns for
            ##    n1, n2, n3, n4, pHole, pMerge, children, spread)
        }
        tries <- tries + 1
        targetIndex <- tries %% tryGuess
        if (targetIndex == 0) targetIndex <- tryGuess
        
        if (tries %% 500 == 0) {
            if ((successes / tries) < (1 - effort)) {
                print(paste0("After ", tries, " tries and only ", successes,
                            " successes, the algorithm terminated. Successes saved."))
                print("Increase the effort parameter or relax the bounds.")
                if (saveSummary & sum(!is.na(summaryDF$nOut)) > 0) {
                    summaryDF <- summaryDF[!is.na(summaryDF$nOut), ]
                    write.csv(summaryDF, file = paste0(filename, "_summary.csv"),
                              row.names = FALSE)
                }
                break
            }
            print(paste(successes, "successes in", tries, "tries so far...", sep = " "))
        }
        
        thisArg <- controlMat[targetIndex, ]
        thisArg <- list(n = thisArg[1:4],
                        pHole = thisArg[5],
                        pMerge = thisArg[6],
                        control = list(cluster = c(thisArg[7], thisArg[8])),
                        hAsp = targetMat[targetIndex, 5])
        thisLand <- tryCatch(do.call("rlandscape", thisArg),
                             error = function(e) list(stats = c(
                                 nOut = -1,
                                 degMean = -1,
                                 degSD = -1,
                                 areaCV = -1,
                                 hAsp = NA,
                                 n1 = NA,
                                 n2 = NA,
                                 n3 = NA,
                                 n4 = NA,
                                 pMerge = NA,
                                 pHole = NA)))
        thisStats <- thisLand$stats

        if (checkLand(stats = thisStats, bounds = bounds)) {

            successes <- successes + 1
            summaryDF[successes, 2:ncol(summaryDF)] <- thisStats[1:11]
            if (saveAdj) saveAdj(thisLand$adj, filename = fullName[successes])
            if (savePlot) {
                png(filename = paste0(fullName[successes], ".png"),
                    width = 640, height = round(640 / thisStats$hAsp))
                plot(thisLand, ...)
                dev.off()
            }
            if (saveAreas) {
                write.csv(makeAreas(thisLand$summary),
                          file = paste0(fullName[successes], "_areas.csv"),
                          row.names = FALSE)
            }
            if (saveLand) save(thisLand, file = paste0(fullName[successes], ".Rdata"))
    }
    }
    ## Save the summary
    if (saveSummary & sum(!is.na(summaryDF$nOut)) > 0) {
        write.csv(summaryDF, file = paste0(filename, "_summary.csv"),
                  row.names = FALSE)
    }
    
    return(c("successes" = successes, "tries" = tries))
}


#' Calculates rlandscape parameters using the original lm approach
#'
#' targetMat should have column order "n", "degMean", "degSD", "areaCV", "hAsp"
#' as in the output of \code{makeTargetMat} function.
#' 
#' @param targetMat matrix of targets.
#'
#' @return matrix of parameters for \code{rlandscape}. Column order is
#'         pHole, pMerge, pLat, pClust, pSSI, children, spread
getParamsLm <- function(targetMat) {
    ## Okay. This was written before I thought of a better way to do things.
    ## So the models are hardcoded.
    ##
    ## I'm sorry too.
    params <- matrix(NA, nrow = nrow(targetMat), ncol = 8)
    for (i in 1:nrow(targetMat)) {
        pHole <- 0
        pMerge <- 0
        pClust <- 0
        pLat <- 0
        pSSI <- 0
        spread <- 0.01
        children <- 10
        
        n <- targetMat[i, 1]
        dm <- targetMat[i, 2]
        sd <- targetMat[i, 3]
        cv <- targetMat[i, 4]
        hAsp <- targetMat[i, 5]        
        
        if (cv < 56.1) {
            ## Use SSI when high dm, Lat when low dm
            if (dm < 4.5) { 
                #  Begin lattice 
                # Must find pLat, pHole, and pMerge
                pLat <- (max(0,
                             0.0018916802 +
                                 0.0242357612 * cv +
                                 0.3448884426 * dm +
                                 0.7975754620 * sd +
                                 -0.1285168319 * hAsp +
                                 -0.0122143735 * cv * dm +
                                 -0.0272013806 * cv * sd +
                                 -0.2134670817 * dm * sd +
                                 0.0047607477 * cv * hAsp +
                                 0.0306755782 * dm * hAsp +
                                 0.0890567504 * sd * hAsp +
                                 0.0069329260 * cv * dm * sd +
                                 -0.0009202953 * cv * dm * hAsp +
                                 -0.0032959800 * cv * sd * hAsp +
                                 -0.0192665653 * dm * sd * hAsp +
                                 0.0006573028 * cv * dm * sd * hAsp)) ^ (1 / 2.6)
                
                pLatBox <- pLat ^ 2.6
                
                pHole <- (max(0,
                              -0.111726129 +
                                  0.020055917 * cv +
                                  0.102445411 * dm +
                                  1.240468628 * sd +
                                  0.971029828 * pLatBox +
                                  -0.005162486 * cv * dm +
                                  -0.014424910 * cv * sd +
                                  -0.262783129 * dm * sd +
                                  -0.009592290 * cv * pLatBox +
                                  -0.376047080 * dm * pLatBox +
                                  -0.522937725 * sd * pLatBox +
                                  0.003245974 * cv * dm * sd +
                                  0.004181883 * cv * dm * pLatBox +
                                  0.008188504 * cv * sd * pLatBox +
                                  0.148101201 * dm * sd * pLatBox +
                                  -0.002921096 * cv * dm * sd * pLatBox)) ^ (1 / 0.71)
                
                pHoleBox <- pHole ^ 0.71
                
                pMerge <- (max(0,
                               -0.851223375 +
                                   0.030557526 * cv +
                                   -0.154347713 * dm +
                                   0.605047039 * sd +
                                   -2.163712546 * pLatBox +
                                   -0.323784893 * pHoleBox +
                                   -0.001861974 * cv * dm +
                                   -0.009958089 * cv * sd +
                                   0.073295652 * dm * sd +
                                   -0.026261358 * cv * pLatBox +
                                   0.882713600 * dm * pLatBox +
                                   1.019102973  * sd * pLatBox +
                                   -0.006171021 * cv * pHoleBox +
                                   0.032948069 * dm * pHoleBox +
                                   0.292099442 * sd * pHoleBox +
                                   1.679892644 * pLatBox * pHoleBox +
                                   -0.003114309 * cv * dm * pLatBox +
                                   0.019323158 * cv * sd * pLatBox +
                                   -0.378709781 * dm * sd * pLatBox +
                                   -0.884864371 * sd * pLatBox * pHoleBox)) ^ (4 / 3)
                ## End Lattice
            } else {     
                ## Start SSI
                pSSI <- (max(0,
                             1.861864714 +
                                 -0.006812436 * dm +
                                 -0.029767572 * sd +
                                 -0.031320774 * cv)) ^ (1 / 2.5)
                
                pSSIbox <- pSSI ^ 2.5
                
                pHole <- (max(0, 
                              1.463375069 +
                                  -0.098484600 * pSSIbox +
                                  -0.150494846 * dm +
                                  0.342471110 * sd +
                                  -0.011659322 * cv +
                                  -0.018858640 * pSSIbox * dm +
                                  0.088031132 * pSSIbox * sd +
                                  -0.109025814 * dm * sd +
                                  -0.002353975 * pSSIbox * cv +
                                  0.004181020 * sd * cv)) ^ (1 / 0.7)
                
                pHoleBox <- pHole ^ 0.7
                
                pMerge <- (max(0,
                               -3.229551001 +
                                   0.858816217 * pSSIbox +
                                   0.401164457 * dm +
                                   1.141137939 * sd +
                                   0.048860426 * cv +
                                   0.926730792 * pHoleBox +
                                   -0.117577910 * pSSIbox * dm +
                                   -0.076030390 * pSSIbox * sd +
                                   -0.074082431 * dm * sd +
                                   -0.007196297 * dm * cv +
                                   -0.005547122 * sd * cv +
                                   -0.298113908 * pSSIbox * pHoleBox +
                                   -0.017367741 * cv * pHoleBox)) ^ (4 / 3)
                ## End SSI
            }
            ## End low CV
        } else {  ## Normal CV
            DMbox <- dm ^ (-1 / 3)
            SDbox <- sd ^ (-2 / 3)
            CVbox <- cv ^ (-1 / 10)
            ## pHole is based on degree mean lm dm^(-1/3) ~ pHole
            pHole <- (DMbox - 0.5665) / 0.1393
            
            ## pMerge based on sd lm SDbox ~ pHole * pMerge + DMbox
            pMerge <- (SDbox - .0296 + .1450 * pHole - 1.2657 * DMbox) / (-0.4269 + 0.3804 * pHole)
            
            ## We choose a pClust (correlated with CV) and then pick a spread term to match
            if (cv <= 125) {
                alpha <- 1
                beta <- 125 / cv
            } else {
                alpha <- cv / 125
                beta <- 1
            }
            
            pClust <- rbeta(1, shape1 = alpha, shape2 = beta) * .7 + .15 # between .15 and .85
            ## from lm cv^(-1/4) ~ pClust * spread + pHole + pMerge
            spread <- (CVbox - 5.973e-1 + 1.35e-2 * pHole - 6.892e-3 * pMerge - 1.166e-1 * DMbox -
                3.262e-2 * SDbox + 2.642e-5 * n + 1.349e-3 * hAsp + 8.956e-2 * pClust +
                2.985e-2 * SDbox * pClust) / (7.345e-2 + 2.863e-1 * pClust + 1.098e-1 * pMerge)
            
            ## children from lm children^(1/3) ~ pClust * spread * CV^(-1/4) + pHole + pMerge
            children <- round( (2.325e1 - 3.006e1 * CVbox - 1.240e2 * spread - 1.587e0 * pClust +
                7.503e-1 * pHole + 5.443e-1 * pMerge - 2.658e-2 * hAsp + 2.226e-3 * n +
                1.025e0 * SDbox - 4.551e0 * DMbox + 1.829e2 * spread * pClust +
                -6.071e-1 * pHole * pMerge)^3 )
        }
        
        ## Checking ranges of output
        if (pHole < 0) pHole <- 0
        if (pHole > 0.8) pHole <- 0.8
        if (pMerge < 0) pMerge <- 0
        if (pMerge > .7) pMerge <- 0.7
        if (pLat < 0) pLat <- 0
        if (pLat > 1) pLat <- 1
        if (pClust < 0) pLat <- 0
        if (pClust > 1) pLat <- 1
        if (pSSI < 0) pSSI <- 0
        if (pSSI > 1) pSSI <- 1
        if (children < 3) children <- 3
        if (spread < hAsp * .001 / n) spread <- hAsp * 0.001 / n
        if (spread > .2) spread <- 0.2
        
        nVec <- round(n * c(1 - (pLat + pClust + pSSI), pLat, pClust, pSSI))
        params[i, ] <- c(nVec, pHole, pMerge, children, spread)
    }
    
    return(params)
}


#' Gets random parameters without desired characteristics
#' 
#' Arbitrary bounds that should usually work
#' 
#' @param reps number of parameter sets to generate
#' @return matrix of control parameters with columns
#'         n1, n2, n3, n4, hAsp, pHM, nHoriz, nVert, children, spread, inh
getParamsRandom <- function(reps) {
    # n1, n2, n3, n4, pHole, pMerge, nHoriz, nVert, children, spread, inh
    pn <- matrix(runif(4 * reps), ncol = 4)
    pn <- pn / rowSums(pn)
    n <- round(pn * (sample(980, size = reps, replace = TRUE) + 20))
    
    hAsp <- runif(reps, min = 1, max = 5)
    pHM <- matrix(0.8 * runif(2 * reps), ncol = 2)
    nHoriz <- ceiling(runif(reps) * sqrt(n[, 2]))
    nVert <- ceiling(n[, 2] * (1 + rowSums(pHM)) / nHoriz * (1 + runif(reps)))
    
    children <- sample(3:12, size = reps, replace = TRUE)
    spread <- .639 * sqrt(hAsp / (n[, 3] * (1 + rowSums(pHM)))) * runif(reps)
    inh = .639 * sqrt(hAsp / (n[, 4]) * (1 + rowSums(pHM))) * runif(reps)
    
    return(cbind(n = n, hAsp = hAsp, pHM, nHoriz, nVert, children, spread, inh))
}

#' Generates specific targets from target ranges
#' 
#' None
#'
#' @param targets a named list of target ranges for each characteristic
#' @param tryGuess the number of targets to generate
#' @return A matrix of specific targets. Column order
#' "n", "degMean", "degSD", "areaCV", "hAsp"
makeTargetMat <- function(targets, tryGuess) {
    if (length(targets[["n"]]) == 1) {
        n <- rep(targets[["n"]], 2)
    } else n <- min(targets[["n"]]):max(targets[["n"]])
    if (length(targets[["hAsp"]]) == 1) {
        hAsp <- rep(targets[["hAsp"]], 2)
    } else hAsp <- min(targets[["hAsp"]]):max(targets[["hAsp"]])
    
    matrix(
        c(sample(n, size = tryGuess, replace = TRUE),
          runif(tryGuess, min = min(targets[["degMean"]]), max = max(targets[["degMean"]])),
          runif(tryGuess, min = min(targets[["degSD"]]), max = max(targets[["degSD"]])),
          runif(tryGuess, min = min(targets[["areaCV"]]), max = max(targets[["areaCV"]])),
          sample(hAsp, size = tryGuess, replace = TRUE)),
        ncol = 5,
        dimnames = list(1:tryGuess, c("n", "degMean", "degSD", "areaCV", "hAsp")))
}


#' Checks if stats are within bounds
#'
#' Bounds taken from environment it is called from.
#'
#' @param stats a named vector (or list or DF) of landscape statistics
#' @param bounds a list of 2-elemnt vectors indicating upper and lower bounds
#' @return boolean indicating whether all stats in in bounds or not.
checkLand <- function(stats, bounds) {
    if (stats[["nOut"]] < 1) return(FALSE)
    return(all(
        stats[["nOut"]] >= bounds$n[1],
        stats[["nOut"]] <= bounds$n[2],
        stats[["areaCV"]] >= bounds$areaCV[1],
        stats[["areaCV"]] <= bounds$areaCV[2],
        stats[["degMean"]] >= bounds$degMean[1],
        stats[["degMean"]] <= bounds$degMean[2],
        stats[["degSD"]] >= bounds$degSD[1],
        stats[["degSD"]] <= bounds$degSD[2]))
}

    