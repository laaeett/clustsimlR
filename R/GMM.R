# Functions relating to GMM analysis

#' Fit a Gaussian Mixture Model to the data
#'
#' Function to conduct Gaussian Mixture Modelling (GMM) onto data. User can
#'  input a seed, otherwise initial centroids are selected at random. This
#'  function can also check the data for missing values or zero-inflation,
#'  and this feature can be turned off if desired. It is recommended to run
#'  \code{clustsimlR::plot_loss()} first, for hyperparameter (`num_clust` and
#'  `modelNames`) selection. It is also recommended to conduct **PCA** on the
#'  data before GMM, for dimensionality reduction and to reduce the impact
#'  of noise and zero-inflation.
#'
#' @param data A data frame where each column represents a gene and each row
#'  represents a single cell. Data frame passed into this parameter must be
#'  numeric i.e. if a data frame has non-numeric columns, they must be
#'  removed first.
#' @param num_clust An integer indicating the numbers of clusters used for GMM
#'  fitting. If NULL, will use the number of features divided by 2. Default is
#'  NULL.
#' @param seed An integer value for the seed to be used for random number
#'  generation. Default is NULL, which means that no seed is set and initial
#'  centroids are selected at random.
#' @param modelNames A character vector specifying the names of the models to
#'  be used for GMM fitting, as described in \code{?mclust::mclustModelNames}.
#'  Default is NULL, which uses all available models that are suitable for the
#'  data. See \code{?mclust::mclustBIC}, \code{?mclust::mclustICL} or
#'  \code{?mclust::Mclust} for more details.
#' @param inform_progress A logical indicating whether to report progress
#'  of GMM fitting while the function is running. Default is TRUE for
#'  interactive sessions, and FALSE otherwise.
#' @inheritParams check_everything
#' @returns The results of GMM clustering, as an object of class `Mclust`. If
#'  multiple models were fitted, the results will provide the optimal model
#'  based on BIC. The components of this object are as described in
#'  \code{?mclust::Mclust}.
#'
#' @importFrom mclust Mclust
#' @examples
#' # load dasatinib dataset
#' data(dasatinib)
#' if_data <- dasatinib[ , -c(1,2)]  # remove non-numeric and categorical cols
#'
#' # run GMM clustering on the data
#' gmm_results <- fit_GMM(if_data)
#'
#' # access a summary of the results
#' summary(gmm_results)
#'
#' @references
#'
#' Central Limit Theorem. (2008). In The Concise Encyclopedia of Statistics
#' (pp. 66–68). Springer New York.
#' https://doi.org/10.1007/978-0-387-32833-1_50
#'
#' Fraley, C., & Raftery, A. E. (2002). Model-based clustering, discriminant
#' analysis and density estimation. \emph{Journal of the American Statistical
#' Association}, 97(458), 611-631.
#'
#' Fraley, C., & Raftery, A. E. (2007). Bayesian regularization for normal
#' mixture estimation and model-based clustering. \emph{Journal of
#' Classification}, 24(2), 155-181.
#'
#' Jolliffe, I. T. (2002). \emph{Principal Component Analysis}, 2nd Edition,
#' New York: Springer.
#'
#' @export
#'
fit_GMM <- function(data,
                    num_clust = NULL,
                    seed = NULL,
                    modelNames = NULL,
                    inform_progress = TRUE,
                    check_zero = TRUE,
                    overall_zero_threshold = 0.5,
                    cell_zero_threshold = 0.5,
                    gene_zero_threshold = 0.5,
                    clean_zero = TRUE,
                    clean_NA = TRUE) {

    # check for missing data and zeroes(if user wants)
    # stops if found missing data but not fixed
    tryCatch(check_everything(data,
                             check_zero,
                             overall_zero_threshold,
                             cell_zero_threshold,
                             gene_zero_threshold,
                             clean_zero,
                             clean_NA),
             error = function(e) {
                 stop("Stopping function due to unhandled missing data.\n")
             })

    if ( !is.numeric(as.matrix(data))) {
        stop("Data frame contains non-numeric values. Please use a subset
             of the data frame with only numeric values.\n")
    }

    # if seed provided, set seed
    if ( !(is.null(seed))) {
        set.seed(seed)
    }

    if (is.null(num_clust)) {
        num_clust <- ncol(data) / 2
    }

    gmm_clustering <- mclust::Mclust(data = data,
                                     G = num_clust,
                                     modelNames = modelNames,
                                     verbose = inform_progress)

    cat("Number of Gaussians fitted: ", gmm_clustering$G, "\n")
    return(gmm_clustering)
}


#' Plot the BIC and ICL values for different numbers of clusters and different
#'  models
#'
#' Plot the BIC value (Bayesian Information Criterion) the ICL value (Integrated
#'  Complete-data Likelihood) for different numbers of clusters, and different
#'  models used in Gaussian Mixture Modelling (GMM). These values can be used as
#'  a regularised loss function for determining the optimal number of clusters
#'  and optimal model for GMM fitting. User can choose to plot either value,
#'  or both. User can input a seed, otherwise initial centroids are selected at
#'  random.This function can also check the data for missing values or
#'  zero-inflation, and this feature can be turned off if desired. It is also
#'  recommended to conduct PCA on the data before running GMM, for
#'  dimensionality reduction and to reduce the impact of noise and
#'  zero-inflation.
#'
#' @inheritParams fit_GMM
#' @param num_clust A vector indicating the numbers of clusters used for the GMM
#'  fitting. Default is 1:10, indicating that GMM will be conducted for 1 to 10
#'  clusters.
#' @param plot_bic A logical indicating whether to plot BIC values. Default is
#' TRUE.
#' @param plot_icl A logical indicating whether to plot ICL values. Default is
#' TRUE.
#' @returns Returns `NULL`. Generates BIC and/or ICL plots as specified by user.
#'
#' @importFrom mclust mclustBIC mclustICL
#' @examples
#' # load dasatinib dataset
#' data(dasatinib)
#' if_data <- dasatinib[ , -c(1,2)]  # remove non-numeric and categorical cols
#'
#' # plot BIC and ICL values for 1 to 10 clusters
#' plot_loss(if_data)
#'
#' @references
#'
#' Central Limit Theorem. (2008). In The Concise Encyclopedia of Statistics
#' (pp. 66–68). Springer New York.
#' https://doi.org/10.1007/978-0-387-32833-1_50
#'
#' Biernacki, C., Celeux, G., & Govaert, G. (2000). Assessing a mixture model
#' for clustering with the integrated completed likelihood.
#' \emph{IEEE Transactions on Pattern Analysis and Machine Intelligence}, 22(7),
#' 719–725. https://doi.org/10.1109/34.865189
#'
#' Fraley, C., & Raftery, A. E. (2002). Model-based clustering, discriminant
#' analysis and density estimation. \emph{Journal of the American Statistical
#' Association}, 97(458), 611-631.
#'
#' Fraley, C., & Raftery, A. E. (2007). Bayesian regularization for normal
#' mixture estimation and model-based clustering. \emph{Journal of
#' Classification}, 24(2), 155-181.
#'
#' Jolliffe, I. T. (2002). \emph{Principal Component Analysis}, 2nd Edition,
#' New York: Springer.
#'
#' @export
#'
plot_loss <- function(data,
                     num_clust = 1:10,
                     seed = NULL,
                     modelNames = NULL,
                     inform_progress = interactive(),
                     plot_bic = TRUE,
                     plot_icl = TRUE,
                     check_zero = TRUE,
                     overall_zero_threshold = 0.5,
                     cell_zero_threshold = 0.5,
                     gene_zero_threshold = 0.5,
                     clean_zero = TRUE,
                     clean_NA = TRUE) {

    # check for missing data and zeroes(if user wants)
    # stops if found missing data but not fixed
    tryCatch(check_everything(data,
                             check_zero,
                             overall_zero_threshold,
                             cell_zero_threshold,
                             gene_zero_threshold,
                             clean_zero,
                             clean_NA),
             error = function(e) {
                 stop("Stopping function due to unhandled missing data.\n")
             })

    if ( !is.numeric(as.matrix(data))) {
        stop("Data frame contains non-numeric values. Please use a subset
             of the data frame with only numeric values.\n")
    }

    # won't break the function, but really just renders it useless if both FALSE
    if ( !(plot_bic || plot_icl)) {
        message("At least one of plot_bic or plot_icl should be TRUE.\n")
        return()
    }

    # if seed provided, set seed
    if( !(is.null(seed))) {
        set.seed(seed)
    }

    if (plot_bic) {
        bic <- mclust::mclustBIC(data,
                                 G = num_clust,
                                 modelNames = modelNames,
                                 verbose = inform_progress)
        plot(bic)
    }

    if (plot_icl) {
        icl <- mclust::mclustICL(data,
                                 G = num_clust,
                                 modelNames = modelNames,
                                 verbose = inform_progress)
        plot(icl)
    }

    return(invisible(NULL))
}

#' Plot a t-SNE visualisation for GMM clusters
#'
#' Plot a t-SNE visualisation of the data, coloured by GMM clusters.
#'
#' @param gmm_clustering An object of class `Mclust`, as outputted by
#' \code{clustsimlR::fit_GMM()}. The components of this object are as described
#'  in \code{?mclust::Mclust}.
#' @param seed An integer value for the seed to be used for random number
#'  generation. Default is NULL, which means that no seed is set and initial
#'  embedding points for t-SNE are selected at random. As a result, if not set,
#'  two different t-SNE plots of the same data and same hyperparameters may not
#'  look the same.
#' @param size An integer value denoting the size of the points for the t-SNE
#'  plot, in millimeters. Default is 5. For more information see
#'  \code{?ggplot2::size}.
#' @returns Returns a t-SNE plot in two dimensions, coloured based on
#'  cluster membership.
#' @importFrom Rtsne Rtsne
#' @importFrom ggplot2 ggplot geom_point aes ggtitle xlab ylab labs theme_bw
#'  scale_fill_viridis_d
#'
#' @examples
#' # load dasatinib dataset
#' data(dasatinib)
#' if_data <- dasatinib[ , -c(1,2)]  # remove non-numeric and categorical cols
#'
#' # run GMM clustering on the data
#' gmm_results <- fit_GMM(if_data)
#'
#' # plot t-SNE
#' plot_GMM_clusters(gmm_results, seed = 42)
#'
#' @references
#'
#' Fraley, C., & Raftery, A. E. (2002). Model-based clustering, discriminant
#' analysis and density estimation. \emph{Journal of the American Statistical
#' Association}, 97(458), 611-631.
#'
#' L.J.P. van der Maaten. (2014). Accelerating t-SNE using tree-based
#' algorithms. \emph{Journal of Machine Learning Research} 15(Oct), 3221-3245.
#'
#' van der Maaten, L.J.P. & Hinton, G.E., (2008). Visualizing data using t-SNE.
#' \emph{Journal of Machine Learning Research}, 9(86), 2579-2605.
#'
#' @export
#'
plot_GMM_clusters <- function(gmm_clustering,
                              seed = NULL,
                              size = 5) {

    if ( !(is.null(seed))){
        set.seed(seed)
    }

    X <- gmm_clustering$data
    tsne_data <- Rtsne::Rtsne(X, pca = FALSE)$Y
    clusters <- gmm_clustering$classification
    tsne_df <- data.frame(tsne1 = tsne_data[ , 1],
                          tsne2 = tsne_data[ , 2],
                          cluster = clusters)

    tsne_plot <- ggplot2::ggplot(tsne_df) +
        ggplot2::geom_point(size = size, ggplot2::aes(x = tsne1,
                                         y = tsne2,
                                         color = as.factor(cluster))) +
        ggplot2::labs(title = "TSNE plot of GMM clusters",
                      x = "TSNE1",
                      y = "TSNE2",
                      color = "Cluster") +
        ggplot2::scale_color_viridis_d() +
        ggplot2::theme_bw()

    return(tsne_plot)
}

# [END]
