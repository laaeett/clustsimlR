library(clustsimlR)


test_that("cleans NA values", {

  missing_data_NA <- data.frame(c(1, 2, 3), c(4, 5, 6), c(7, NA, 9))

  fixed <- clustsimlR:::clean_missing_data(missing_data_NA)

  testthat::expect_false(anyNA(fixed))
})

test_that("cleans NaN values", {

    missing_data_NaN <- data.frame(c(1, 2, 3), c(4, NaN, 6), c(7, 8, 9))

    fixed <- clustsimlR:::clean_missing_data(missing_data_NaN)

    testthat::expect_false(anyNA(fixed))
})

test_that("cleans NA and NaN values", {

    missing_data_NA_NaN <- data.frame(c(1, 2, 3), c(4, NaN, 6), c(7, NA, 9))

    fixed <- clustsimlR:::clean_missing_data(missing_data_NA_NaN)

    testthat::expect_false(anyNA(fixed))
})

test_that("leaves data unchanged if no NA or NaN", {

    ok_df <- data.frame(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9))

    fixed <- clustsimlR:::clean_missing_data(ok_df)

    testthat::expect_false(anyNA(fixed))
    testthat::expect_equal(fixed, ok_df)
})

test_that("cleans data with whole NA columns", {

    missing_data_whole_col_NA <- data.frame(c(1, 2, 3), c(4, 5, 6),
                                            c(NA, NA, NA))
    missing_data_whole_col_NaN <- data.frame(c(1, 2, 3), c(4, 5, 6),
                                             c(NaN, NaN, NaN))
    fixed <- clustsimlR:::clean_missing_data(missing_data_whole_col_NA)
    fixed2 <- clustsimlR:::clean_missing_data(missing_data_whole_col_NaN)

    testthat::expect_false(anyNA(fixed))
    testthat::expect_false(anyNA(fixed2))
})
