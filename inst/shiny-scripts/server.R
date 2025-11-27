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
library(grid)
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
        tsne <- plot_GMM_clusters(gmm())
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
