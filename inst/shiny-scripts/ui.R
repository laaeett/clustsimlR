# For Shiny app: UI side
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
library(clustsimlR)

ui <- page_sidebar(
    # App title
    title = "clustsimlR",

    # Sidebar panel for inputs
    sidebar = sidebar(
        # Input an excel file, with features as cols and cells as rows
        fileInput(
            inputId = "data_file",
            label = "Upload your data file here (.csv or .rds format):",
            accept = c(".csv", ".rds")
        ),

        # Input: Text for categorical and text columns to remove
        textInput(
            inputId = "cols_to_remove",
            label = paste0("Please input names of categorical",
                           " and text columns,",
                           " (comma-separated, no spaces between).",
                           " e.g. Cell no.,Conc"),
            value = "Cell no.,Conc"
        ),

        # Input: Slider for the number of clusters
        numericInput(
            inputId = "clusters",
            label = "Number of clusters for GMM clustering:",
            min = 2,
            value = 10
        ),

        # Input: Switch OFF to use original data for GMM instead of PCA scores
        input_switch(id = "use_pca",
                     label = paste0("Switch OFF to use original data",
                                    " for clustering instead of PC scores"),
                     value = TRUE),

        # add input for PCs to plot

        # Input: Action button to submit values
        actionButton(inputId = "go_button",
                     label = paste0("When ready, click this button ",
                                    "to run cluster distance analysis!"))
    ),

    # Tab panels for plots
    tabsetPanel(

        # Panel for PCA-related plots
        tabPanel("PCA",
                 div(
                     style = paste0("height:600px;",
                                    " overflow-y: scroll;",
                                    " padding-right:20px;"),
                     plotOutput("scree", height = "300px"),
                     plotOutput("pc_heatmap", height = "300px"),
                     plotOutput("scatter", height = "300px"),
                 )
        ),

        # Panel for TSNE plot of GMM clusters
        tabPanel("GMM",
                 plotOutput("tsne", height = "300px")
        ),

        # Panel for heatmap of cluster distances
        tabPanel("Cluster distances",
                 plotOutput(outputId = "clust_diff", height = "600px")),

        # Panel to show the data
        tabPanel("Data table",
                 DT::DTOutput(outputId = "table")),
        )
)

# [END]
