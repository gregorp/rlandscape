#' Shiny app for rland
#' 
#' @import shiny
rlandGui = function() {
    shinyApp(
        ui = navbarPage(
            title = "Rlandscape Simulator",
            tabPanel(title = "Single Run",
                     fluidPage(
                        sidebarLayout(
                            sidebarPanel = sidebarPanel(
                                # Arguments for rlandscape
                                sliderInput(inputId = "nUnif",
                                            label = "Uniform points",
                                            min = 0, max = 500, step = 10, value = 30),
                                sliderInput(inputId = "nLattice",
                                            label = "Lattice points",
                                            min = 0, max = 500, step = 10, value = 70),
                                sliderInput(inputId = "nClust",
                                            label = "Cluster points",
                                            min = 0, max = 500, step = 10, value = 0),
                                sliderInput(inputId = "nSsi",
                                            label = "Inhibition points",
                                            min = 0, max = 250, step = 10, value = 0),
                                sliderInput(inputId = "pHole",
                                            label = "Hole probability",
                                            min = 0, max = 0.8, step = 0.1, value = 0.2),
                                sliderInput(inputId = "pMerge",
                                            label = "Merge probability",
                                            min = 0, max = 0.8, step = 0.1, value = 0.2),
                                sliderInput(inputId = "hAsp",
                                            label = "Horizontal to vertical aspect ratio",
                                            min = 0.2, max = 1.5, step = 0.1, value = 1),
                                actionButton(inputId = "doPlot",
                                             label = "Resimulate")
                            ),
                            mainPanel = mainPanel(
                                h3("Landscape"),
                                plotOutput("landPlot")
                            ),
                            position = "left"
                        )    
                     )
            ),
            tabPanel(title = "Batch Mode",
                     fluidPage(
                         # targets column
                         column(width = 4,
                                h3("Target Ranges"),
                             sliderInput(inputId = "nTarget",
                                         label = "N (number of stands) target range:",
                                         min = 20, max = 1000, step = 10, value = c(30, 120)),
                             sliderInput(inputId = "degMeanTarget",
                                         label = "Degree mean target range:",
                                         min = 2, max = 6, step = 0.25, value = c(4.5, 5.5)),
                             sliderInput(inputId = "degSDTarget",
                                         label = "Degree standard deviation target range:",
                                         min = 1, max = 3.5, step = 0.1, value = c(1.2, 2)),
                             sliderInput(inputId = "areaCVTarget",
                                         label = "Area coefficient of variation target range:",
                                         min = 20, max = 300, step = 10, value = c(40, 70)),
                             sliderInput(inputId = "hAspTarget",
                                         label = "Aspect ratio target range:",
                                         min = 0.5, max = 10, step = 0.5, value = c(1, 3))
                         ),
                         # bounds column
                         column(width = 4,
                                h3("Bounds Ranges"),
                             sliderInput(inputId = "nBound",
                                         label = "N (number of stands) strict bounds:",
                                         min = 10, max = 1010, step = 10, value = c(20, 150)),
                             sliderInput(inputId = "degMeanBound",
                                         label = "Degree mean strict bounds:",
                                         min = 2, max = 6, step = 0.25, value = c(3.5, 6)),
                             sliderInput(inputId = "degSDBound",
                                         label = "Degree standard deviation strict bounds:",
                                         min = 0.5, max = 4, step = 0.1, value = c(1, 2.2)),
                             sliderInput(inputId = "areaCVBound",
                                         label = "Area coefficient of variation strict bounds:",
                                         min = 10, max = 350, step = 10, value = c(30, 100))
                         ),
                         # other column
                         column(width = 4,
                                h3("Other options"),
                             numericInput(inputId = "reps",
                                          label = "Number of landscapes:",
                                          value = 25, min = 1, max = 1e5),
                             radioButtons(inputId = "method",
                                          label = "Parameter prediction type:",
                                          choices = c("linear", "random"),
                                          selected = "linear"),
                             h5("Save options:"),
                             checkboxInput(inputId = "saveAdj",
                                           label = "Adjacencies",
                                           value = TRUE),
                             checkboxInput(inputId = "saveAreas",
                                           label = "Areas",
                                           value = TRUE),
                             checkboxInput(inputId = "saveSummary",
                                           label = "Summary",
                                           value = TRUE),
                             checkboxInput(inputId = "savePlot",
                                           label = "Plots",
                                           value = TRUE),
                             checkboxInput(inputId = "saveLand",
                                           label = "R Object",
                                           value = TRUE),
                             textInput(inputId = "savePath",
                                       label = "Path to save (including file name stem",
                                       value = paste(getwd(), "landscape", sep = "/")),
                             actionButton(inputId = "doSim",
                                          label = "Generate landscapes")
                         )
                     )
            )
        ),
        server = shinyServer(function(input, output) {
            # landscape for single mode
            output$landPlot = renderPlot({
                input$doPlot # for dependency of plot button
                this_land = rlandscape::rlandscape(
                    n = c(input$nUnif, input$nLattice, input$nClust, input$nSsi),
                    hAsp = input$hAsp,
                    pHole = input$pHole,
                    pMerge = input$pMerge
                )
                plot(this_land)
            })
            
            # build call for batch mode
            rlandCall = reactive({
                return(list(targets = list(n = input$nTarget,
                                    degMean = input$degMeanTarget,
                                    degSD = input$degSDTarget,
                                    areaCV = input$areaCVTarget,
                                    hAsp = input$hAspTarget),
                     bounds =  list(n = input$nBound,
                                    degMean = input$degMeanBound,
                                    degSD = input$degSDBound,
                                    areaCV = input$areaCVBound),
                     reps = input$reps,
                     method = input$method,
                     filename = input$savePath,
                     saveAdj = input$saveAdj,
                     savePlot = input$savePlot,
                     saveLand = input$saveLand,
                     saveAreas = input$saveAreas,
                     saveSummary = input$saveSummary))
            })
            
            # run batches on button press
            observeEvent(
                eventExpr = input$doSim,
                handlerExpr = {
                    #browser()
                    do.call(what = rlandscape::rland,
                            args = isolate(rlandCall()))
                }
            )
        })
    )
}
