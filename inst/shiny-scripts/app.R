# For Shiny app
# Code adapted from examples in the Shiny tutorials:
# https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/.
# Citation:
# Chang W, Cheng J, Allaire J, Sievert C, Schloerke B, Xie Y, Allen J,
# McPherson J, Dipert A, Borges B (2025). _shiny: Web Application Framework
# for R_. doi:10.32614/CRAN.package.shiny
# <https://doi.org/10.32614/CRAN.package.shiny>, R package version 1.11.1,
# <https://CRAN.R-project.org/package=shiny>.

library(shiny)
library(bslib)
library(clustsimlR)

ui <- page_sidebar(
    # App title ----
    title = "clustsimlR",

    # Sidebar panel for inputs ----
    sidebar = sidebar(
        # Input an excel file, with features as cols and cells as rows ----
        fileInput(
            inputId = "data_file",
            label = "Upload your data file here (.csv or .rds format):",
            accept = c(".csv", ".rds")
        ),

        # Input: Slider for the number of clusters ----
        numericInput(
            inputId = "clusters",
            label = "Number of clusters for GMM clustering:",
            min = 1,
            value = 10
        )
    ),

    # Output??
    card(
        DT::DTOutput(outputId = "table")
    )

)

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


    output$table <- DT::renderDT({
        DT::datatable(data_table(),
                      options = list(pageLength = 10),
                      rownames = FALSE)
    })

}

shinyApp(ui = ui, server = server)
