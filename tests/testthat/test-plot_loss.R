library(clustsimlR)


test_that("stops when missing data not handled", {

    missing_data_NA <- data.frame(c(1, 2, 3), c(4, 5, 6), c(7, NA, 9))
    missing_data_NaN <- data.frame(c(1, 2, 3), c(4, NaN, 6), c(7, 8, 9))

    testthat::expect_error(plot_loss(missing_data_NA))
    testthat::expect_error(plot_loss(missing_data_NaN))

})

test_that("stops when there is non-numeric data", {

    non_numeric_data <- data.frame(c('a', 'b', 'c'), c(4, 5, 6), c(7, 8, 9))

    testthat::expect_error(plot_loss(non_numeric_data))
})

test_that("throws message if both plot_icl and plot_bic FALSE", {

    ok_df <- data.frame(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9))
    testthat::expect_message(plot_loss(ok_df,
                                       plot_icl = FALSE,
                                       plot_bic = FALSE),
                     "At least one of plot_bic or plot_icl should be TRUE.\n")
})

test_that("runs successfully with well-formatted data", {
    ok_df <- data.frame(matrix(runif(30)), nrow = 5, ncol = 6)
    testthat::expect_no_condition(plot_loss(ok_df))
})
