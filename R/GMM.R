# Functions relating to GMM analysis

# helper
plot_bic <- function(gmm_clusters,
                     num_clust = 1:10,
                     color = "blue") {

    bic <- gmm_clusters$bic
    bic_plot <- ggplot2::geom_line(ggplot2::aes(x = num_clust,
                                                y = bic),
                                   color = color)
    return(bic_plot)
}

# helper
plot_icl <- function(gmm_clusters,
                     num_clust = 1:10,
                     color = "red") {

    icl <- gmm_clusters$icl
    icl_plot <- ggplot2::geom_line(ggplot2::aes(x = num_clust,
                                                y = icl),
                                   color = color)
    return(icl_plot)
}

plot_loss <- function(data,
                     num_clust = 1:10,
                     colors = c("blue", "red"),
                     check_zero = TRUE,
                     overall_zero_threshold = 0.5,
                     cell_zero_threshold = 0.5,
                     gene_zero_threshold = 0.5,
                     clean_zero = TRUE,
                     clean_NA = TRUE,
                     copy = FALSE) {

    # check for missing data and zeroes(if user wants)
    # stops if found missing data but not fixed
    tryCatch(checkEverything(data,
                             check_zero,
                             overall_zero_threshold,
                             cell_zero_threshold,
                             gene_zero_threshold,
                             clean_zero,
                             clean_NA,
                             copy),
             error = function(e) {
                 stop("Stopping function due to unhandled missing data.\n")
             })

    gmm_clusters <- mclust::Mclust(data = data,
                                   G = num_clust,
                                   verbose = FALSE)

    plt <- ggplot2::ggplot() +
        plot_bic(gmm_clusters, num_clust, color[1])
        plot_icl(gmm_clusters, num_clust, color[2])
        ggplot2::ggtitle("BIC and ICL values for GMM clusters") +
        ggplot2::xlab("Number of clusters") +
        ggplot2::ylab("Value") +
        ggplot2::legend("BIC" = colors[1], "ICL" = colors[2])

}

fit_GMM <- function(data,
                num_clust = 5:5,
                seed = NULL,
                plot_BIC = TRUE,
                inform_progress = TRUE,
                check_zero = TRUE,
                overall_zero_threshold = 0.5,
                cell_zero_threshold = 0.5,
                gene_zero_threshold = 0.5,
                clean_zero = TRUE,
                clean_NA = TRUE,
                copy = FALSE) {

    # check for missing data and zeroes(if user wants)
    # stops if found missing data but not fixed
    tryCatch(checkEverything(data,
                             check_zero,
                             overall_zero_threshold,
                             cell_zero_threshold,
                             gene_zero_threshold,
                             clean_zero,
                             clean_NA,
                             copy),
             error = function(e) {
                 stop("Stopping function due to unhandled missing data.\n")
             })

    # if seed provided, set seed
    if(!(is.null(seed))) {
        set.seed(seed)
    }

    gmm_clustering <- mclust::Mclust(data = data,
                                     G = num_clust,
                                     verbose = inform_progress)

    cat("Number of Gaussians fitted: ", gmm_clustering$G, "\n")
    return(gmm_clustering)
}


# function to plot TSNE coloured by cluster

plot_GMM_clusters <- function(gmm_clustering,
                              seed = NULL
                              ) {
    X <- gmm_clustering$data
    tsne_data <- Rtsne(X, pca = FALSE)$Y
    clusters <- gmm_clustering$classification
    tsne_df <- data.frame(tsne1 = tsne_data[,1],
                          tsne2 = tsne_data[,2],
                          cluster = as.factor(clusters))

    tsne_plot <- ggplot2::ggplot(tsne_df) +
        ggplot2::geom_point(ggplot2::aes(x = tsne1,
                                         y = tsne2,
                                         color = cluster)) +
        ggplot2::ggtitle("TSNE plot of GMM clusters") +
        ggplot2::xlab("TSNE1") +
        ggplot2::ylab("TSNE2") +
        ggplot2::labs(color = "Cluster")

    return(tsne_plot)
}


