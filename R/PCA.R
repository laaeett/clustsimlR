# Functions relating to PCA


#' Conduct Principal Component Analysis
#'
#' Conduct PCA on the given data. Unless specified otherwise, will also generate
#' a scree plot of cumulative explained variance and a heatmap comparing the
#' weights of each variable in the principal component. Also, unless
#' specified otherwise, will check for missing data and zeroes, and clean them.
#'
#' @inheritParams checkEverything
#' @param data A data frame where each column represents a gene and each row
#'  represents a single cell. Data frame passed into this parameter must be
#'  numeric i.e. if a data frame has non-numeric columns, they must be
#'  removed first.
#' @param scree_plot A boolean indicating whether to generate a scree plot.
#' Default is TRUE.
#' @param pc_heatmap A boolean indicating whether to generate a heatmap of the
#' principal components and weights of original features. Default is TRUE.
#' @returns A list of class `princomp` containing the results of the PCA.
#' The components of the list are as described in \code{?stats::princomp}.
#'
#' @examples
#' # load dasatinib dataset
#' data(dasatinib)
#' df <- dasatinib[, -c(1,2)]  # remove non-numeric and categorical cols
#' pca_results <- PCA(df)
#'
#' # access first 5 cells and their scores for each PC
#' pca_results$scores[1:5, ]
#'
#' @importFrom stats princomp screeplot
#' @importFrom pheatmap pheatmap
#'
#' @references
#' Jolliffe, I. T. (2002). \emph{Principal Component Analysis}, 2nd Edition,
#' New York: Springer.
#'
#' Upton, G., & Cook, I. (2014). Scree plot. In \emph{A Dictionary of Statistics
#' (3rd ed.)}. Oxford University Press.
#'
#' @export
#'
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

    if(!is.numeric(as.matrix(data))) {
        stop("Data frame contains non-numeric values. Please use a subset
             of the data frame with only numeric values.\n")
    }

    pca_data <- stats::princomp(x = data, scores = TRUE)

    if(scree_plot) {
        stats::screeplot(x = pca_data)
    }
    if(pc_heatmap) {
        pheatmap::pheatmap(pca_data$loadings)
    }
    return(pca_data)
}

#' Plot data in PC-space
#'
#' Plot the scores of the data in the space defined by two principal components
#' as a scatter plot.
#'
#' @param pca_data A list of class `princomp` containing the results of PCA as
#' outputted by \code{PCA}. The components of the list are as described in
#' \code{?stats::princomp}.
#' @param pci An integer indicating which principal component to plot as the
#' x-axis. Default value is 1, i.e. PC1 as the x-axis
#' @param pcj An integer indicating which principal component to plot as the
#' y-axis. Default value is 2, i.e. PC2 as the y-axis.
#'
#' @returns A ggplot2 scatter plot of of the data in the space defined by
#' two principal componentsm PCi and PCj.
#'
#' @examples
#' # load dasatinib dataset
#' data(dasatinib)
#' df <- dasatinib[, -c(1,2)]  # remove non-numeric and categorical cols
#' pca_results <- PCA(df)
#'
#' # access first 5 cells and their scores for each PC
#' pca_results$scores[1:5, ]
#'
#' # Plot data in PC1 vs PC2 space
#' plot_PC(pca_results)
#'
#' @references
#' Jolliffe, I. T. (2002). \emph{Principal Component Analysis}, 2nd Edition,
#' New York: Springer.
#'
#' @importFrom ggplot2 ggplot geom_point aes_string xlab ylab ggtitle
#' @export
#'
plot_PC <- function(pca_data,
                    pci = 1,
                    pcj = 2) {
    if(pci > ncol(pca_data$scores) || pcj > ncol(pca_data$scores)) {
        stop("Requested principal component exceeds number of components
             in PCA results.\n")
    }

    if(pci == 0 || pcj == 0){
        stop("PC indices must be >= 1.\n")
    }

    if(!(is.integer(pci) && is.integer(pcj))) {
        if(is.numeric(pci)) {
            pci <- as.integer(pci)
        }
        if(is.numeric(pcj)) {
            pcj <- as.integer(pcj)
        }
        else {
            stop("PC indices must be integers.\n")
        }
    }

    pc_scores <- as.data.frame(pca_data$scores)
    ggplot2::ggplot(pc_scores) +
    ggplot2::geom_point(ggplot2::aes_string(x = sprintf("PC%d", pci),
                                            y = sprintf("PC%d", pcj))) +
    ggplot2::xlab(sprintf("PC%d", pci)) +
    ggplot2::ylab(sprintf("PC%d", pcj)) +
    ggplot2::ggtitle(sprintf("Scatter plot of PC%d vs. PC%d", pci, pcj))

}





