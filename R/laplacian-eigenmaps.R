

#' Degree Matrix
#'
#' Compute the degree of each vertex in a weighted adjacency matrix.
#'
#' @param w A symmetric weighted adjacency matrix.
#' @param symmetric A logical. If \code{FALSE}, then \code{w} is checked for
#'   symmetry. If it is known beforehand that \code{w} is symmetric, then it
#'   is faster to call with \code{symmetric = TRUE}.
#'
#' @return A diagonal matrix, whose values on the main diagonal represent
#'   the weighted degrees of the vertices in \code{w}---i.e., the values along
#'   the main diagonal are the row-sums of \code{w}.
#'
#' @export
D <- function(w, symmetric = FALSE) {
  if (!symmetric && !isSymmetric(w)) {
    stop(stringr::str_c(substitute(w), " must be a symmetric matrix."))
  }
  d <- w %>% rowSums() %>% diag()
  rownames(d) <- colnames(d) <- rownames(w)
  return(d)
}



#' Laplacian Matrix
#'
#' Compute the Laplacian matrix from a weighted adjacency matrix.
#'
#' @param w A symmetric weighted adjacency matrix.
#' @param symmetric A logical. If \code{FALSE}, then \code{w} is checked for
#'   symmetry. If it is known beforehand that \code{w} is symmetric, then it
#'   is faster to call with \code{symmetric = TRUE}.
#'
#' @return A symmetric matrix, whose values on the main diagonal represent the
#'   degree of the vertices and whose values off the main diagonal represent
#'   the negative weights in the adjacency matrix.
#'
#' @seealso \code{\link{D}}
#'
#' @export
L <- function(w, symmetric = FALSE) {
  l <- D(w, symmetric = symmetric) - w
  rownames(l) <- colnames(l) <- rownames(w)
  return(l)
}



#' Laplacian Eigenmaps
#'
#' Compute the Laplacian-Eigenmaps embedding of a weighted adjacency matrix.
#'
#' @param w A symmetric weighted adjacency matrix.
#' @param symmetric A logical. If \code{FALSE}, then \code{w} is checked for
#'   symmetry. If it is known beforehand that \code{w} is symmetric, then it
#'   is faster to call with \code{symmetric = TRUE}.
#'
#' @return The eigenvalues \code{lambda} and corresponding eigenvectors \code{e}
#'   that are solutions to the generalized eigenvector problem:
#'   \code{L(w)*f = lambda*D(w)*e}.
#'
#'   The eigenvalues and eigenvectors are returned as a list with two elements:
#'   \itemize{
#'     \item \code{values}: a numeric vector of the eigenvalues, in increasing
#'       order.
#'     \item \code{vectors}: a \code{\link[tibble]{tibble}} with columns
#'       \code{x, e1, ..., eN}, where \code{N} is the number of rows in the
#'       input matrix \code{w}. The column \code{x} contains the row names of
#'       \code{w}. The columns \code{e1, ..., eN} are the eigenvectors associated
#'       with the eigenvalues (e.g., \code{vectors$e1} is associated with
#'       \code{values[1]}). Columns \code{e2, ..., eJ} give an embedding, in
#'       \code{J-1}-dimensinal space, for the data identified by \code{x}.
#'   }
#'
#' @export
LaplacianEigenmaps <- function(w, symmetric = FALSE) {
  if (!symmetric && !isSymmetric(w)) {
    stop(stringr::str_c(substitute(w), " must be a symmetric matrix."))
  }
  if (rownames(w) %>% is.null()) {
    rownames(w) <- colnames(w) <- 1:nrow(w)
  }
  le <- geigen::geigen(A = L(w, symmetric = TRUE),
                       B = D(w, symmetric = TRUE),
                       symmetric = TRUE)
  values <- le[["values"]]
  vectors <-
    le[["vectors"]] %>%
    tibble::as_tibble() %>%
    purrr::set_names(nm = stringr::str_c("e", 1:ncol(w))) %>%
    dplyr::bind_cols(tibble::tibble(x = rownames(w))) %>%
    dplyr::select(x, dplyr::everything())
  embedding <- list(values = values, vectors = vectors)
  return(embedding)
}
