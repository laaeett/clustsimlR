# Functions relating to PCA


PCA <- function(data,
                scree_plot = TRUE,
                pc_heatmap = TRUE,
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





