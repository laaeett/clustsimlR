library(clustsimlR)


test_that("doesn't accept threshold values outside 0 <= x <= 1", {

    ok_threshold <- 0.6
    high_threshold <- 1.1
    low_threshold <- -0.1
    ok_df <- data.frame(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9))

    testthat::expect_error(clustsimlR:::clean_zero_inflation(ok_df,
                                        cell_zero_threshold = high_threshold,
                                        gene_zero_threshold = high_threshold))
    testthat::expect_error(clustsimlR:::clean_zero_inflation(ok_df,
                                        cell_zero_threshold = low_threshold,
                                        gene_zero_threshold = low_threshold))
    testthat::expect_no_error(clustsimlR:::clean_zero_inflation(ok_df,
                                        cell_zero_threshold = ok_threshold,
                                        gene_zero_threshold = ok_threshold))
})

test_that("data passes zero-inflation check after cleaning", {

    inflated_df <- data.frame(c(1, 0, 3), c(2, 0, 0), c(3, 0, 0))
    inflated_df_NA <- data.frame(c(1, 2, 3), c(0, 0, 0), c(0, 0, 0),
                                 c(NA, NA, NA))
    inflated_df_NaN <- data.frame(c(1, 2, 3), c(0, 0, 0), c(0, 0, 0),
                                  c(NaN, NaN, NaN))

    fixed <- clustsimlR:::clean_zero_inflation(inflated_df)
    fixed_NA <- clustsimlR:::clean_zero_inflation(inflated_df_NA)
    fixed_NaN <- clustsimlR:::clean_zero_inflation(inflated_df_NaN)

    testthat::expect_false(clustsimlR:::check_zero_inflation(fixed))
    testthat::expect_false(clustsimlR:::check_zero_inflation(fixed_NA))
    testthat::expect_false(clustsimlR:::check_zero_inflation(fixed_NaN))

})

test_that("does not modify non zero-inflated data", {

    ok_df <- data.frame(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9))
    ok_df_NA <- data.frame(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9), c(NA, NA, NA))
    ok_df_NaN <- data.frame(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9),
                            c(NaN, NaN, NaN))

    fixed <- clustsimlR:::clean_zero_inflation(ok_df)
    fixed_NA <- clustsimlR:::clean_zero_inflation(ok_df_NA)
    fixed_NaN <- clustsimlR:::clean_zero_inflation(ok_df_NaN)

    testthat::expect_equal(fixed, ok_df)
    testthat::expect_equal(fixed_NA, ok_df_NA)
    testthat::expect_equal(fixed_NaN, ok_df_NaN)

})


# [END]
