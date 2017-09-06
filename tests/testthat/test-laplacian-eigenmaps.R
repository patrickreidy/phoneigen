
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
})

testthat::test_that("Degree matrix inherits row-names of w", {
  matrix1 <- matrix(rep(1, times = 16), nrow = 4, ncol = 4)
  rownames(matrix1) <- c("a", "b", "c", "d")
  diag1 <- diag(rep(4, times = 4))
  rownames(diag1) <- colnames(diag1) <- c("a", "b", "c", "d")
  testthat::expect_identical(
    object = phoneigen::D(matrix1, symmetric = TRUE),
    expected = diag1
  )
})










testthat::context(":: Laplcian matrices")

testthat::test_that("Laplacian matrix is computed only for symmetric matrix", {
  testthat::expect_error(
    object = phoneigen::L(matrix(1:16, ncol = 4, nrow = 4)),
    regexp = "is not symmetric"
  )
  failure1 <- matrix(1:16, ncol = 4, nrow = 4)
  testthat::expect_error(
    object = phoneigen::L(failure1),
    regexp = "failure1 is not symmetric"
  )
  failure2 <- matrix(rep(1, times = 16), ncol = 4, nrow = 4)
  rownames(failure2) <- c("a", "b", "c", "d")
  testthat::expect_error(
    object = phoneigen::L(failure2),
    regexp = "failure2 is not symmetric"
  )
})

testthat::test_that("Diagonal entries of Laplacian matrix", {
  matrix1 <- matrix(rep(1, times = 16), nrow = 4, ncol = 4)
  rownames(matrix1) <- colnames(matrix1) <- c("a", "b", "c", "d")
  diag1 <- diag(rep(4, times = 4))
  rownames(diag1) <- colnames(diag1) <- c("a", "b", "c", "d")
  testthat::expect_identical(
    object = diag(phoneigen::L(matrix1)),
    expected = diag(diag1) - diag(matrix1)
  )
})

testthat::test_that("Off-diagonal entries of Laplacian matrix", {
  matrix1 <- matrix(rep(1, times = 16), nrow = 4, ncol = 4)
  rownames(matrix1) <- colnames(matrix1) <- c("a", "b", "c", "d")
  L1 <- phoneigen::L(matrix1)
  diag(L1) <- 0
  matrix2 <- matrix(rep(-1, times = 16), nrow = 4, ncol = 4)
  rownames(matrix2) <- colnames(matrix2) <- c("a", "b", "c", "d")
  diag(matrix2) <- 0
  testthat::expect_identical(
    object = L1,
    expected = matrix2
  )
})
