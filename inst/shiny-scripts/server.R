# For Shiny app: server side
#
# Code adapted from examples in the Shiny tutorials:
# https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/.
#
# Citation:
# Chang W, Cheng J, Allaire J, Sievert C, Schloerke B, Xie Y, Allen J,
# McPherson J, Dipert A, Borges B (2025). _shiny: Web Application Framework
# for R_. doi:10.32614/CRAN.package.shiny
# <https://doi.org/10.32614/CRAN.package.shiny>, R package version 1.11.1,
# <https://CRAN.R-project.org/package=shiny>.

library(shiny)
library(bslib)
library(pheatmap)
library(clustsimlR)

server <- function(input, output) {

    data(dasatinib, package="clustsimlR")

    data_table <- reactive({
        if (is.null(input$data_file$datapath)) {
            data_table <- dasatinib
        } else if (grepl("\\.csv$", input$data_file$datapath)) {
            data_table <- read.csv(input$data_file$datapath)
        } else if (grepl("\\.rds$", input$data_file$datapath)) {
            data_table <- readRDS(input$data_file$datapath)
        }

        return(data_table)
    })

    # Run analyses only when button pressed
    pca <- eventReactive(input$go_button, {
        clustsimlR::PCA(data_table[ , -strsplit(input$cols_to_remove,
                                                       split = ",")],
                               pc_heatmap = FALSE, scree_plot = FALSE)
    })

    output$scree <- renderPlot({
        plot(pca())
    })

    output$pc_heatmap <- renderPlot({
        pheatmap::pheatmap(data$pca$loadings,
                           cluster_rows = FALSE,
                           cluster_cols = FALSE,
                           angle_col = 45)
    })


    output$table <- DT::renderDT({
        DT::datatable(data_table(),
                      options = list(pageLength = 10),
                      rownames = FALSE)
    })

}

# [END]
