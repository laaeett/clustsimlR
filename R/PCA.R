# Functions relating to PCA


PCA <- function(data,
                scree_plot = TRUE,
                pc_heatmap = TRUE) {
    if(checkZeroInflation(data)) {
        message("Data appears to be zero-inflated.\n")
        message("Please interpret results with this in mind.\n")
    }

    pca_data <- stats::princomp(x = data, scores = scores)
    stats::screeplot(x = pca_data)
    pheatmap::pheatmap(pca_data.loadings)
    return(pca_data)
}

# function to plot out stuff wrt the PCAs





