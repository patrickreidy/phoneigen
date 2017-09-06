
library(magrittr)


testthat::context(":: Degree matrices")

testthat::test_that("Degree matrix is computed only for symmetric matrix", {
  testthat::expect_error(
    object = phoneigen::D(matrix(1:16, ncol = 4, nrow = 4)),
    regexp = "is not symmetric"
  )
  failure1 <- matrix(1:16, ncol = 4, nrow = 4)
  testthat::expect_error(
    object = phoneigen::D(failure1),
    regexp = "failure1 is not symmetric"
  )
  failure2 <- matrix(rep(1, times = 16), ncol = 4, nrow = 4)
  rownames(failure2) <- c("a", "b", "c", "d")
  testthat::expect_error(
    object = phoneigen::D(failure2),
    regexp = "failure2 is not symmetric"
  )
})

testthat::test_that("Degree matrix is computed as row-sums", {
  matrix1 <- matrix(rep(1, times = 16), nrow = 4, ncol = 4)
  rownames(matrix1) <- colnames(matrix1) <- c("a", "b", "c", "d")
  diag1 <- diag(rep(4, times = 4))
  rownames(diag1) <- colnames(diag1) <- c("a", "b", "c", "d")
  testthat::expect_identical(
    object = phoneigen::D(matrix1),
    expected = diag1
  )
  matrix2 <- matrix(1:16, nrow = 4, ncol = 4)
  diag2 <- diag(c(28, 32, 36, 40))
  testthat::expect_identical(
    object = phoneigen::D(matrix2, symmetric = TRUE),
    expected = diag2
  )
  matrix3 <- matrix(rep(1, times = 16), nrow = 4, ncol = 4)
  rownames(matrix3) <- c("a", "b", "c", "d")
  diag3 <- diag(rep(4, times = 4))
  rownames(diag3) <- colnames(diag3) <- c("a", "b", "c", "d")
  testthat::expect_identical(
    object = phoneigen::D(matrix3, symmetric = TRUE),
    expected = diag1
  )
})
