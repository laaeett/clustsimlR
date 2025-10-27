# Functions relating to GMM analysis

plot_bic <- function(data,
                     num_clust = 1:10) {
    bic_values <- mclust::mclustBIC(data = data,
                                    G = num_clust)
    ggplot2::autoplot(bic_values) +
        ggplot2::ggtitle("BIC values for GMM clusters") +
        ggplot2::xlab("Number of clusters") +
        ggplot2::ylab("BIC")
}

fit_GMM <- function(data,
                num_clust = 5:5,
                seed = NULL,
                plot_BIC = TRUE,
                inform_progress = TRUE) {

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




