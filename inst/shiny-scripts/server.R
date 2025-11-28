# For Shiny app: server side
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
# Attali, D., Sali, A. (2024). _shinycssloaders: Add Loading Animations to a 'shiny'
#   Output While It's Recalculating_. doi:10.32614/CRAN.package.shinycssloaders
#   <https://doi.org/10.32614/CRAN.package.shinycssloaders>, R package version 1.1.0,
#   <https://CRAN.R-project.org/package=shinycssloaders>.
#
# Chang, W., Cheng, J., Allaire, J., Sievert, C., Schloerke, B., Xie, Y.,
#  Allen, J., McPherson, J., Dipert, A., Borges, B. (2025). _shiny: Web
#  Application Framework for R_. doi:10.32614/CRAN.package.shiny
#  <https://doi.org/10.32614/CRAN.package.shiny>, R package version 1.11.1,
#  <https://CRAN.R-project.org/package=shiny>.
#
# Sievert, C., Cheng, J., Aden-Buie, G. (2025). _bslib: Custom 'Bootstrap' 'Sass' Themes
#   for 'shiny' and 'rmarkdown'_. doi:10.32614/CRAN.package.bslib
#   <https://doi.org/10.32614/CRAN.package.bslib>, R package version 0.9.0,
#   <https://CRAN.R-project.org/package=bslib>.
#
library(shiny)
library(bslib)
library(shinyalert)
library(pheatmap)
library(grid)
library(clustsimlR)

server <- function(input, output) {

    # Toggle dark mode
    toggle_dark_mode()

    # Load default dataset
    data(dasatinib, package="clustsimlR")

    # Logic for restoring default file
    # Update state if a file has been uploaded
    # Default is no file uploaded
    r <- reactiveValues(uploaded = FALSE)

    observeEvent(input$data_file, {
        r$uploaded <- TRUE
    })

    observeEvent(input$reset_file, {
        r$uploaded <- FALSE
    })

    # Update data_table if a file has been uploaded or removed
    data_table <- reactive({
        fp <- input$data_file$datapath

        # Set as data_table iff correct file type uploaded
        # Else set dasatinib and warn
        if (r$uploaded) {
            if (grepl("\\.csv$", fp)) {
                data_table <- read.csv(fp)
            } else if (grepl("\\.rds$", fp)) {
                data_table <- readRDS(fp)
            } else {
                data_table <- dasatinib
                shinyalert(
                    title = "Unsupported file type",
                    text = paste0("Please reupload a .csv. or .rds file, ",
                                  "or click 'Restore default' ",
                                  "to use the default dataset. "),
                    size = "s",
                    closeOnEsc = FALSE,
                    type = "error",
                    showConfirmButton = TRUE,
                    confirmButtonText = "OK",
                    confirmButtonCol = "maroon",
                    timer = 0,
                    animation = TRUE
                )
            }
        } else {
            data_table <- dasatinib
        }

        return(data_table)
    })

    removed_cols_data <- reactive({
        removed_cols_data <- data_table()[ , !(names(data_table()) %in%
                                unlist(strsplit(input$cols_to_remove,
                                            split = ",")))]
        return(removed_cols_data)
    })

    # Run PCA only when button pressed
    pca <- eventReactive(input$go_button, {
        pca <- clustsimlR::PCA(removed_cols_data(),
                        pc_heatmap = FALSE,
                        scree_plot = FALSE)
        return(pca)
    })

    # Output the PCA scree plot after PCA done
    output$scree <- renderPlot({
        req(input$go_button, pca())
        return(stats::screeplot(x = pca(),
                         type = "lines",
                         main = "Scree plot of explained variance per PC"))
    })

    # Output the PC coefficient heatmap only after PCA done
    output$pc_heatmap <- renderPlot({
        req(input$go_button, pca())
        heatmap <- pheatmap::pheatmap(pca()$loadings,
                           cluster_rows = FALSE,
                           cluster_cols = FALSE,
                           angle_col = 45,
                           main = "PC coefficients")
        return(heatmap)
    })

    # Render cluster distance function: do this regardless of whether
    # analysis has been run or not
    output$math_output <- renderUI({
        withMathJax(paste0("$$\\text{tr}\\left(\\ln^{2}\\left",
                        "(\\sqrt{\\textbf{A}^{-1}}\\textbf{B}",
                        "\\sqrt{\\textbf{A}^{-1}}\\right)\\right)$$"))
        })

    # Output scatterplot of PC scores for 2 PCs
    output$scatter <- renderPlot({
        req(input$go_button, pca())
        return(clustsimlR::plot_PC(pca()))
    })

    # Run GMM clustering only when button pressed and PCA done
    gmm <- eventReactive(input$go_button, {
        req(pca())

        if (input$use_pca) {
            data_for_gmm <- pca()$scores
        } else {
            data_for_gmm <- removed_cols_data()
        }

        gmm <- clustsimlR::fit_GMM(data_for_gmm,
                                   num_clust = input$clusters,
                                   check_zero = FALSE,
                                   inform_progress = FALSE,
                                   clean_zero = FALSE,
                                   clean_NA = FALSE)
        return(gmm)
    })

    # Plot TSNE of GMM clusters only after GMM done
    output$tsne <- renderPlot({
        req(input$go_button, pca(), gmm())
        tsne <- plot_GMM_clusters(gmm(), size=10)
        return(tsne)
    })

    # Compute intercluster distances after PCA and GMM done
    cluster_distances <- eventReactive(input$go_button, {
        req(input$go_button, pca(), gmm())
        clust_dist <- clustsimlR::intercluster_dist(gmm())
        return(clust_dist)
    })

    # Plot heatmap of intercluster distances
    output$clust_diff <- renderPlot({
        req(input$go_button, pca(), gmm(), cluster_distances())
        clust_heatmap <- pheatmap::pheatmap(cluster_distances(),
                                           cluster_rows = FALSE,
                                           cluster_cols = FALSE,
                                           cellwidth = 45,
                                           cellheight = 45,
                                           main = "Inter-cluster distance")
        grid::grid.text("Cluster",
                        y=0.05,
                        x=0.5)
        return(clust_heatmap)
    })

    # Plot the data table
    output$table <- DT::renderDT({
        DT::datatable(data_table(),
                      options = list(pageLength = 10),
                      rownames = FALSE)
    })

}

# [END]
