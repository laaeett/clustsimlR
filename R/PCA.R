# Functions relating to PCA


PCA <- function(data,
                scree_plot = TRUE,
                pc_heatmap = TRUE) {

    if(checkZeroInflation(data)) {
        message("Data appears to be zero-inflated.\n")
        message("Please interpret results with this in mind.\n")
    }

    if(checkMissingData(data)) {
        message("Data contains missing values or NaN values.\n")
        stop("Please impute or remove missing data before proceeding.\n")
    }

    pca_data <- stats::princomp(x = data, scores = scores)

    if(scree_plot) {
        stats::screeplot(x = pca_data)
    }
    if(pc_heatmap) {
        pheatmap::pheatmap(pca_data.loadings)
    }
    return(pca_data)
}

# function to plot out stuff wrt the PCAs
plot_PC <- function(pca_data,
                    pci = 1,
                    pcj = 2) {
    pc_scores <- as.data.frame(pca_data$scores)
    ggplot2::ggplot(pc_scores) +
    ggplot2::geom_point(ggplot2::aes_string(x = sprintf("Comp. %d", pci),
                                            y = sprintf("Comp. %d", pcj))) +
    ggplot2::xlab(sprintf("PC%d", pci)) +
    ggplot2::ylab(sprintf("PC%d", pcj)) +
    ggplot2::ggtitle(sprintf("Scatter plot of PC%d vs. PC%d", pci, pcj))

}





