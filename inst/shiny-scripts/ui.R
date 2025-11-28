# For Shiny app: UI side
#
# Code adapted from examples in the Shiny tutorials:
# https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/.
#
# Citations for Shiny app-related packages:
#
# Attali, D., Edwards, T. (2024). _shinyalert: Easily Create Pretty Popup
#  Messages (Modals) in 'Shiny'_. doi:10.32614/CRAN.package.shinyalert
#  <https://doi.org/10.32614/CRAN.package.shinyalert>, R package version
#  3.1.0, <https://CRAN.R-project.org/package=shinyalert>.
#
# Attali, D., Sali, A. (2024). _shinycssloaders: Add Loading Animations to a
#   'shiny' Output While It's Recalculating_.
#   doi:10.32614/CRAN.package.shinycssloaders
#   <https://doi.org/10.32614/CRAN.package.shinycssloaders>,
#   R package version 1.1.0,
#   <https://CRAN.R-project.org/package=shinycssloaders>.
#
# Chang, W., Cheng, J., Allaire, J., Sievert, C., Schloerke, B., Xie, Y.,
#  Allen, J., McPherson, J., Dipert, A., Borges, B. (2025). _shiny: Web
#  Application Framework for R_. doi:10.32614/CRAN.package.shiny
#  <https://doi.org/10.32614/CRAN.package.shiny>, R package version 1.11.1,
#  <https://CRAN.R-project.org/package=shiny>.
#
# Sievert, C., Cheng, J., Aden-Buie, G. (2025). _bslib: Custom 'Bootstrap'
#   'Sass' Themes for 'shiny' and 'rmarkdown'_. doi:10.32614/CRAN.package.bslib
#   <https://doi.org/10.32614/CRAN.package.bslib>, R package version 0.9.0,
#   <https://CRAN.R-project.org/package=bslib>.
#

library(shiny)
library(shinycssloaders)
library(bslib)
library(clustsimlR)

ui <- page_sidebar(

    # App theme, purely for aesthetic reasons
    theme = bslib::bs_theme(
        version = 5,
        bootswatch = "minty"
    ),

    # To render math expressions
    withMathJax(),
    tags$script(HTML("
        MathJax.Hub.Config({
          tex2jax: {inlineMath: [['$$','$$']]}
        });")
    ),

    # App title
    title = "clustsimlR",

    # Sidebar panel for inputs
    sidebar = sidebar(
        width = 400,

        # To reduce empty space at the top of the sidebar
        div(style = "margin-top: -40px"),

        # Input a .csv or .rds file, with features as cols and cells as rows
        div(id = "file_input_div",
            fileInput(
                inputId = "data_file",
                label = paste0(
                    "Upload your data file here. Please ensure the ",
                    "file is a .csv or .rds file, where the first row are ",
                    "column headers, each following row is an individual ",
                    "cell, and each column represents a feature (e.g. ",
                    "gene, protein etc.) "),
                accept = c(".csv", ".rds")
            )
        ),

        # To bring the file upload and button closer together
        div(style = "margin-top: -50px"),

        # INput: button to reset file input
        tags$p("Click the button below to restore the default dataset. "),
        actionButton(inputId = "reset_file",
                     label = "Restore default"),

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


        # Input: toggle dark mode
        tags$p("Toggle dark mode. User's system settings will be used ",
               "by default."),
        input_dark_mode(),


        # Input: Action button to submit values
        actionButton(inputId = "go_button",
                     label = paste0("When ready, click this button ",
                                    "to run cluster distance analysis!"))
    ),

    # Tab panels for instructions and plots
    tabsetPanel(

        tabPanel("Welcome!",
         # Description of app
         br(),

         tags$h1("Welcome to the clustsimlR Shiny App!"),
         tags$p(paste0("clustsimlR is an R package that provides",
                       "functions for single-cell data clustering ",
                       "analysis, with the use of within-cluster ",
                       "covariances to characterise a cluster and to ",
                       "assess similarity or difference between clusters.")),
         br(),
         tags$p(paste0("This app is intended as a simple way to explore ",
                       "single-cell data using the clustsimlR package ",
                       "functions, for users who may not be familiar with ",
                       "R programming.")),
         tags$p(paste0("However, please note that to customise your analyses ",
                       "using this package more, you should directly use ",
                       "the functions offered in this package. ",
                       "For an introduction on how to use the package ",
                       "outside of this Shiny app, feel free to explore the ",
                       "vignette by running the following in R.")),
         tags$blockquote(tags$code("browseVignettes(\"clustsimlR\")")),
         br(),

         # Instructions
         tags$h3("How to use this app:"),
         tags$ol(tags$li(
             paste0("Upload your dataset. Note that your data must be ",
                    "formatted such that the first row is a header row, ",
                    "all other rows are individual cells, and each ",
                    "column represents a feature ",
                    "(e.g. a gene or protein). Alternatively, skip ",
                    "this step to use the default dataset. For more ",
                    "details on the default dataset, please consult ",
                    "the package documentation. To explore your dataset, ",
                    "click the right-most tab (Data table). ")),
             br(),
           tags$li(
            paste0("To remove an uploaded file and use the default ",
                    "dataset instead, click the 'Restore default' button ",
                    "below the file upload field. ")),
           br(),
            tags$li(
             paste0("For this analysis, categorical and non-numerical ",
                    "features must be omitted. Input the names of the ",
                    "columns that represent ",
                    "non-numerical or categorical data. Leave as default ",
                    "if using the default dataset. Input must be ",
                    "comma-separated, without spaces between each ",
                    "column name, and spelled identically to the column ",
                    "in the dataset it is referring to. If there are ",
                    "no columns to omit, leave blank. ")),
           br(),
            tags$li(
             paste0("Input the number of clusters to cluster the dataset ",
                    "into. Pro tip: smaller datasets are unsuitable for ",
                    "large numbers of clusters. For a more ",
                    "rigorous way to choose clusters, consider ",
                    "using the functions in the package to ",
                    "evaluate clustering performance over a range of ",
                    "numbers of clusters. ")),
           br(),
            tags$li(
             paste0("By default, PCA scores are used to reduce noise. ",
                    "Toggle the switch to use the original data for ",
                    "clustering instead of scores from PCA. ",
                    "This may be helpful if you think it would be ",
                    "easier to interpret results if using ",
                    "the original data.")),
           br(),
            tags$li(
             paste0("When satisfied with your inputs, click the ",
                    "bottom-most button to start analysis. It is ",
                    "recommended to only start when you are ready ",
                    "to avoid unnecessary re-computation, as ",
                    "clustering can be computationally heavy. ")),
           br(),
            tags$li(
             paste0("Look at the results you have by clicking the PCA, ",
                    "GMM and Cluster distances tabs. "))

         ),

         # Line breaks for spacing
         br(),
         br(),
         br(),
         br(),
         br(),
         br()
        ),

        # Panel for PCA-related plots
        tabPanel("PCA",
                 br(),
                 tags$h1("Plots related to PCA results"),
                 div(
                     style = paste0(" overflow-y: scroll;",
                                    " padding-right:20px;"),

                     # Scree and explanation
                     tags$p(paste0("The scree plot shows ",
                                   "the 'explained variance' for each ",
                                   "principal component. ")),
                     withSpinner(plotOutput("scree", height = "600px"),
                                 type = 6, color = "maroon",
                                 caption = "Just a moment..."),
                     br(),

                     # Heatmap and explanation
                     tags$p(paste0("This heatmap plot shows ",
                                   "the coefficients for each ",
                                   "principal component. ",
                                   "In a sense, this gives a rough idea ",
                                   "on how each ",
                                   "feature contributes to the variance ",
                                   "explained by each PC.")),
                     withSpinner(plotOutput("pc_heatmap", height = "600px"),
                                 type = 6, color = "maroon",
                                 caption = "Just a moment..."),
                     br(),

                     # Scatterplot and explanation
                     tags$p(paste0("This scatterplot shows ",
                                   "each datapoint as a PC score, ",
                                   "with respect to the first two PCs. ")),
                     withSpinner(plotOutput("scatter", height = "600px"),
                                 type = 6, color = "maroon",
                                 caption = "Just a moment...")),

        ),

        # Panel for TSNE plot of GMM clusters
        tabPanel("GMM",
                 br(),
                 tags$h1("GMM clustering results"),
                 tags$p(paste0("This t-SNE plot shows ",
                               "your data, reduced to 2D-space ",
                               "using t-SNE. Each point is coloured based ",
                               "on cluster membership, with clustering being ",
                               "done by GMM. ",
                               "If the toggle on the sidebar is OFF, your ",
                               "original data will be used for GMM. Otherwise,",
                               " PC scores from the PCA step will be used.")),


                 div(
                     withSpinner(plotOutput("tsne",
                                            height = "600px", width = "600px"),
                                 type = 6, color = "maroon",
                                 caption = "Just a moment...")),

                 # Line breaks for spacing
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br()
        ),

        # Panel for heatmap of cluster distances
        tabPanel("Cluster distances",
                 br(),
                 tags$h1("Cluster distances"),
                 tags$p(paste0("This heatmap shows the intercluster ",
                               "distances for the clusters computed for ",
                               "your data. Higher values indicate that two ",
                               "clusters are further apart from each other, ",
                               "i.e. more different. The distances are ",
                               "computed using the within-cluster ",
                               "covariances using the following formula: ")),
                 uiOutput("math_output"),
                 tags$p(paste0("where A and B refer to two ",
                               "covariance matrices. For more details, ",
                               "please refer to the package documentation. ")),
                 withSpinner(plotOutput(
                     outputId = "clust_diff", height = "600px"),
                     type = 6, color = "maroon",
                     caption = "Just a moment..."),

                 # Line breaks for spacing
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br()
        ),


        # Panel to show the data
        tabPanel("Data table",
                 withSpinner(DT::DTOutput(outputId = "table"),
                 type = 6, color = "maroon",
                 caption = "Just a moment..."))
        )
)

# [END]
