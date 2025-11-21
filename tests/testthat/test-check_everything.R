library(clustsimlR)


test_that("stops when detect NA but not fixed", {

    missing_data_NA <- data.frame(c(1, 2, 3), c(4, 5, 6), c(7, NA, 9))
    testthat::expect_error(clustsimlr::check_everything(missing_data_NA,
                                                       clean_NA = FALSE))
})

test_that("stops when detect NaN but not fixed", {

    missing_data_NaN <- data.frame(c(1, 2, 3), c(4, NaN, 6), c(7, 8, 9))
    testthat::expect_error(clustsimlr::check_everything(missing_data_NaN,
                                                       clean_NA = FALSE))
})


test_that("calls cleanMissingData", {
    mockr::local_mock(cleanMissingData = function(data) {
        # mock fx for testing purposes
        message("correct function call :)")
        data.frame(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9))
    })

    missing_data_NA <- data.frame(c(1, 2, 3), c(4, 5, 6), c(7, NA, 9))
    missing_data_NaN <- data.frame(c(1, 2, 3), c(4, NaN, 6), c(7, 8, 9))
    testthat::expect_message(check_everything(missing_data_NA),
                             "correct function call :)")
    testthat::expect_message(check_everything(missing_data_NaN),
                             "correct function call :)")
})

test_that("skips checking zero-inflation if check_zero FALSE", {
    inflated_df <- data.frame(c(1, 0, 3), c(2, 0, 0), c(3, 0, 0))
    testthat::expect_no_message(clustsimlR::check_everything(inflated_df,
                                                            check_zero = FALSE),
                                message = "Data appears to be zero-inflated.\n")
})

test_that("calls checkZeroInflation", {

    mockr::local_mock(checkZeroInflation = function(data,
                                                    overall_zero_threshold) {
        # mock fx for testing purposes
        message("correct function call :)")
        TRUE
    })

    inflated_df <- data.frame(c(1, 0, 3), c(2, 0, 0), c(3, 0, 0))
    testthat::expect_message(check_everything(inflated_df),
                             "correct function call :)")

})

test_that("raise warning if chosen not to clean zero-inflation", {

    inflated_df <- data.frame(c(1, 0, 3), c(2, 0, 0), c(3, 0, 0))
    testthat::expect_warning(clustsimlR::check_everything(inflated_df,
                                                         clean_zero = FALSE))

})

test_that("calls cleanZeroInflation", {

    mockr::local_mock(checkZeroInflation = function(data,
                                                    overall_zero_threshold) {
        TRUE
    })

    mockr::local_mock(cleanZeroInflation = function(data,
                                                    cell_zero_threshold,
                                                    gene_zero_threshold) {
        message("correct function call :)")
        data.frame(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9))
    })

    inflated_df <- data.frame(c(1, 0, 3), c(2, 0, 0), c(3, 0, 0))

    testthat::expect_message(check_everything(inflated_df),
                             "correct function call :)")
})

test_that("well-formatted data (no missing value, not zero-inflated) passes", {

    ok_df <- data.frame(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9))
    testthat::expect_no_condition(clustsimlR::check_everything(ok_df))
})

