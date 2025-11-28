library(clustsimlR)


test_that("doesn't accept threshold values outside 0 <= x <= 1", {

    ok_threshold <- 0.6
    high_threshold <- 1.1
    low_threshold <- -0.1
    ok_df <- data.frame(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9))

    testthat::expect_error(clustsimlR:::check_zero_inflation(ok_df,
                                                          high_threshold))
    testthat::expect_error(clustsimlR:::checkZeroIn(ok_df,
                                                          low_threshold))
    testthat::expect_no_error(clustsimlR:::check_zero_inflation(ok_df,
                                                          ok_threshold))
})

test_that("identifies possible zero-inflation and not zero-inflated data", {

    ok_df <- data.frame(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9))
    inflated_df <- data.frame(c(1, 2, 0), c(0, 0, 0), c(0, 0, 0))

    testthat::expect_true(clustsimlR:::check_zero_inflation(inflated_df))
    testthat::expect_false(clustsimlR:::check_zero_inflation(ok_df))

})

test_that("works even with NA", {

    ok_df_NA <- data.frame(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9), c(NA, NA, NA))
    inflated_df_NA <- data.frame(c(1, 2, 0), c(0, 0, 0), c(0, 0, 0),
                                 c(NA, NA, NA))

    testthat::expect_true(clustsimlR:::check_zero_inflation(inflated_df_NA))
    testthat::expect_false(clustsimlR:::check_zero_inflation(ok_df_NA))

})

test_that("works even with NaN", {

    ok_df_NaN <- data.frame(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9),
                            c(NaN, NaN, NaN))
    inflated_df_NaN <- data.frame(c(1, 2, 0), c(0, 0, 0), c(0, 0, 0),
                                  c(NaN, NaN, NaN))

    testthat::expect_true(clustsimlR:::check_zero_inflation(inflated_df_NaN))
    testthat::expect_false(clustsimlR:::check_zero_inflation(ok_df_NaN))

})

# [END]
