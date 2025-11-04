library(clustsimlR)


test_that("runs properly on intended data and inputs", {

    ok_df <- data.frame(matrix(runif(100)), nrow = 25, ncol = 4)
    gmm <- fit_GMM(ok_df, num_clust = 3)
    testthat::expect_no_condition(plot_GMM_clusters(gmm))
})
