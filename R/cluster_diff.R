# Function for computing cluster differences


#' Compute the distance between two covariance matrices
#'
#' Compute the distance between two covariance matrices as described by Förstner
#'  & Moonen, 2003. In particular, the squared distance
#'  \eqn{d^{2}\left(\textbf{A, B}\right)} between two symmetric positive
#'  matrices \eqn{\textbf{A}} and \eqn{\textbf{B}} are defined as follows:
#'  \deqn{\text{tr}\left(\ln^{2}\left(\sqrt{\textbf{A}^{-1}}\textbf{B}
#'  \sqrt{\textbf{A}^{-1}}\right)\right)}.
#'  In layman's terms, this equation computes how much 'stretching' would be
#'  needed to transform matrix \eqn{B} to \eqn{A}, and sums this stretching up
#'  for all directions. This sum is then the distance between the two matrices.
#'  Though intended to be applied to covariance matrices, this function
#'  would work for any two positive, real, square, symmetric (definite)
#'  matrices.
#'
#' @param covA A positive definite, real, square, symmetric matrix.
#' @param covB A positive definite, real, square, symmetric matrix.
#' @returns The distance between clustA and clustB, based on the metric defined
#'  by Förstner & Moonen, 2003.
#'
#' @importFrom expm sqrtm logm
#' @import Matrix
#'
#' @examples
#' # initialise two positive definite symmetric square matrices
#' matA <- matrix(data = c(2, 1, 1, 2), nrow = 2, ncol = 2)
#' matB <- matrix(data = c(3, 2, 2, 3), nrow = 2, ncol = 2)
#' matC <- matrix(data = c(2, 1, 1, 2), nrow = 2, ncol = 2) # same as A
#'
#' # compute the distance between the two matrices
#' calculate_dist(matA, matB)
#'
#' # this should return 0
#' calculate_dist(matA, matC)
#'
#' @references
#' Förstner, W., & Moonen, B. (2003). A Metric for Covariance Matrices.
#' In Geodesy-The Challenge of the 3rd Millennium (pp. 299–309).
#' Springer Berlin Heidelberg. https://doi.org/10.1007/978-3-662-05296-9_31
#'
#' @export
#'
calculate_dist <- function(covA, covB) {

    # must be square and same dimensions
    if (!(ncol(covA) == nrow(covA) && nrow(covA) == nrow(covB)
         && ncol(covA) == ncol(covB))) {
        stop("Matrices must both be square, and have the same dimensions. \n")
    }

    # must be real
    if (is.complex(covA) || is.complex(covB)) {
        stop("Matrices must only have real numbers. \n")
    }

    # must be symmetric
    if (! Matrix::isSymmetric(covA) || ! Matrix::isSymmetric(covB)) {
        stop("Matrices must be symmetric. \n")
    }

    # must be positive definite
    if (any(eigen(covA)$values < 0) || any(eigen(covB)$values < 0)) {
        stop("Matrices must be positive definite. \n")
    }

    inverse_A <- solve(covA)
    sqrt_inv_A <- expm::sqrtm(inverse_A)
    sqrt_inv_AxB <- sqrt_inv_A %*% covB

    ABA <- sqrt_inv_AxB %*% sqrt_inv_A

    lnABA <- expm::logm(ABA)
    ln2ABA <- lnABA %*% lnABA

    tr = sum(diag(ln2ABA))

    return(tr)
}


#' Compute a distance matrix for within-cluster covariances
#'
#' Compute a distance matrix for within-cluster covariances from Gaussian
#'  Mixture Modelling, where each element \eqn{a_{i,j}} is the distance between
#'  the within-cluster covariance matrices of cluster \eqn{i} and \eqn{j}.
#'  Also can plot a heatmap based on this distance matrix. Distances are
#'  computed based on the metric described by Förstner & Moonen, 2003.
#'
#' @param gmm_clustering The output of \code{clustsimlR::fit_GMM}. More
#'  specifically, an object of class `Mclust` (see \code{?mclust::Mclust}).
#' @param plot A logical value indicating whether to plot a heatmap of the
#'  resulting distance matrix. Default is TRUE.
#' @returns A \eqn{\left(G \times G\right)} distance matrix, where \eqn{G} is
#'  the number of clusters. Distances are based on the metric described by
#'  Förstner & Moonen, 2003.
#'
#' @importFrom pheatmap pheatmap
#' @importFrom grid grid.text
#'
#' @examples
#' # load dasatinib dataset
#' data(dasatinib)
#' dframe <- dasatinib[ , -c(1,2)]  # remove non-numeric and categorical cols
#' dframe_PCA <- PCA(dframe, scree_plot = FALSE, pc_heatmap = FALSE)$scores
#'
#' # run GMM clustering on the data after PCA
#' gmm_results <- fit_GMM(dframe_PCA, num_clust = 10)
#'
#' # compute inter-cluster distances and plot heatmap
#' intercluster_dist(gmm_results)
#'
#' @references
#' Förstner, W., & Moonen, B. (2003). A Metric for Covariance Matrices.
#' In Geodesy-The Challenge of the 3rd Millennium (pp. 299–309).
#' Springer Berlin Heidelberg. https://doi.org/10.1007/978-3-662-05296-9_31
#'
#' @export
#'
intercluster_dist <- function(gmm_clustering,
                              plot = TRUE) {


    n_clust <- gmm_clustering$G
    covs <- gmm_clustering$parameters$variance$sigma
    distance_matrix <- matrix(nrow = n_clust, ncol = n_clust,
                              dimnames = list(seq(1, n_clust), seq(1, n_clust)))

    for (i in seq(1, n_clust)) {
        for (j in seq(1, n_clust)) {
            if (i == j) {
                distance_matrix[i, j] <- 0
            } else if ( !is.na(distance_matrix[j, i])) {
                distance_matrix[i, j] <- distance_matrix[j, i]
            } else {
                distance_matrix[i, j] <- calculate_dist(covs[ ,,i], covs[ ,,j])
            }
        }
    }

    if (plot) {
        heatmap <- pheatmap::pheatmap(distance_matrix,
                                      cluster_rows = FALSE,
                                      cluster_cols = FALSE,
                                      cellwidth = 45,
                                      cellheight = 45,
                                      main = "Inter-cluster distance")
        grid::grid.text("Cluster",
                        y=0.05,
                        x=0.5)
    }

    return(distance_matrix)
}

# [END]
