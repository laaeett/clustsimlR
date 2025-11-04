library(clustsimlR)

test_that("distance matrix is symmetric", {
    ok_df <- data.frame(matrix(runif(400), nrow = 100, ncol = 4))
    gmm <- fit_GMM(ok_df)
    dist_mat <- intercluster_dist(gmm, plot = FALSE)
    testthat::expect_true(isSymmetric(dist_mat))
})

test_that("diagonal of distance matrix is 0", {
    ok_df <- data.frame(matrix(runif(400), nrow = 100, ncol = 4))
    gmm <- fit_GMM(ok_df)
    dist_mat <- intercluster_dist(gmm, plot = FALSE)
    testthat::expect_equal(sum(diag(dist_mat)), 0)
})

#TODO: add other tests
