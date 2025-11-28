library(clustsimlR)


test_that("raises error if PC no. given to plot exceeds total PCs", {
    ok_df <- data.frame(matrix(runif(30)), nrow = 5, ncol = 6)
    pca <- PCA(ok_df,
               scree_plot = FALSE,
               pc_heatmap = FALSE)
    testthat::expect_error(plot_PC(pca, pci = 100))
    testthat::expect_error(plot_PC(pca, pcj = 100))
    testthat::expect_error(plot_PC(pca, pci = 100, pcj = 100))

})

test_that("raises error if PC given to plot less than 1", {

    ok_df <- data.frame(matrix(runif(30)), nrow = 5, ncol = 6)
    pca <- PCA(ok_df,
               scree_plot = FALSE,
               pc_heatmap = FALSE)
    testthat::expect_error(plot_PC(pca, pci = -1))
    testthat::expect_error(plot_PC(pca, pcj = -1))
    testthat::expect_error(plot_PC(pca, pci = -1, pcj = -1))
    testthat::expect_error(plot_PC(pca, pci = 0))
    testthat::expect_error(plot_PC(pca, pcj = 0))
    testthat::expect_error(plot_PC(pca, pci = 0, pcj = -1))
    testthat::expect_error(plot_PC(pca, pci = -1, pcj = 0))
    testthat::expect_error(plot_PC(pca, pci = 0, pcj = 0))

})

test_that("raises error if non-numeric pci or pcj", {
    ok_df <- data.frame(matrix(runif(30)), nrow = 5, ncol = 6)
    pca <- PCA(ok_df,
               scree_plot = FALSE,
               pc_heatmap = FALSE)
    testthat::expect_error(plot_PC(pca, pci = 'a'))
    testthat::expect_error(plot_PC(pca, pcj = 'a'))
    testthat::expect_error(plot_PC(pca, pci = "a", pcj = "a"))

})

test_that("does not raise error if non-integer numeric pci or pcj", {

    ok_df <- data.frame(matrix(runif(30)), nrow = 5, ncol = 6)
    pca <- PCA(ok_df,
               scree_plot = FALSE,
               pc_heatmap = FALSE)
    testthat::expect_no_condition(plot_PC(pca, pci = 1.1))
    testthat::expect_no_condition(plot_PC(pca, pcj = 2.2))
    testthat::expect_no_condition(plot_PC(pca, pci = 1.1, pcj = 2.2))

})

test_that("runs properly on intended data and inputs", {
    ok_df <- data.frame(matrix(runif(30)), nrow = 5, ncol = 6)
    pca <- PCA(ok_df,
               scree_plot = FALSE,
               pc_heatmap = FALSE)
    testthat::expect_no_condition(plot_PC(pca))
    testthat::expect_no_condition(plot_PC(pca, pci = 1, pcj = 3))

})

# [END]
