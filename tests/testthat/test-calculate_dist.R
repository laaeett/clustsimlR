library(clustsimlR)


test_that("raise error if matrices are not square", {
    matA <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2)
    testthat::expect_error(calculate_dist(matA, matA),
            "Matrices must both be square, and have the same dimensions. \n")
})

test_that("raise error if matrices have different dimensions", {
    matA <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
    matB <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, ncol = 3)

    testthat::expect_error(calculate_dist(matA, matB),
            "Matrices must both be square, and have the same dimensions. \n")
})

test_that("raise error if matrices are not positive definite", {
    matA <- matrix(c(1, 2, 2, 1), nrow = 2, ncol = 2, byrow = TRUE)
    matB <- matrix(c(1, 1, 1, 1), nrow = 2, ncol = 2, byrow= TRUE)

    testthat::expect_error(calculate_dist(matA, matB),
                           "Matrices must be positive definite. \n")
    testthat::expect_error(calculate_dist(matB, matA),
                           "Matrices must be positive definite. \n")
    testthat::expect_error(calculate_dist(matA, matA),
                           "Matrices must be positive definite. \n")
})

test_that("raise error if matrices have complex numbers", {
    matA <- matrix(c(1, 2i, 3, 4), nrow = 2, ncol = 2)
    matB <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)

    testthat::expect_error(calculate_dist(matA, matB),
                           "Matrices must only have real numbers. \n")
    testthat::expect_error(calculate_dist(matB, matA),
                           "Matrices must only have real numbers. \n")
    testthat::expect_error(calculate_dist(matA, matA),
                           "Matrices must only have real numbers. \n")
})
