

#' Cartesian Square
#'
#' Compute the Cartesian Square of a data frame or a tibble.
#'
#' This is a thin wrapper for \code{\link[tidyr]{crossing}} that also performs
#' some string formatting on the column names of the data frame or tibble.
#'
#' @param x A data frame or \code{\link[tibble]{tibble}}.
#'
#' @return A \code{\link[tibble]{tibble}} whose rows denote the values of the
#'   Cartesian production of \code{x} with itself. If \code{x} has \code{n}
#'   columns, then the returned Cartesian square has \code{2n} columns: columns
#'   \code{1, ..., n} are the column names of \code{x} with \code{_i} appended;
#'   columns \code{n+1, ..., 2n} are the column names of \code{x} with
#'   \code{_j} appended.
#'
#' @seealso \code{\link[tidyr]{crossing}} for a description of how the rows of
#'   the Cartesian square are ordered.
#'
#' @export
CartesianSquare <- function(x) {
  i <- purrr::set_names(x, nm = stringr::str_c(names(x), "i", sep = "_"))
  j <- purrr::set_names(x, nm = stringr::str_c(names(x), "j", sep = "_"))
  tidyr::crossing(i, j)
}



#' Adjacency Matrix
#'
#' Shape a numeric column of a data frame or tibble into a square adjacency
#' matrix.
#'
#' @param x A data frame or \code{\link[tibble]{tibble}}.
#' @param ... Unquoted basenames that are used to arrange the rows of \code{x}
#'   and to form the rownames of the returned matrix. For example,
#'   if \code{... = a, b, c}, then the rows of \code{x} are first arranged
#'   by \code{a_i, b_i, c_i, a_j, b_j, c_j}; and the row names of the returned
#'   matrix match the pattern \code{"{a}_{b}_{c}"}.
#' @param weights The unquoted name of a numeric column in \code{x} whose values
#'   will be shaped into a square adjacency matrix.
#'
#' @return A numeric matrix.
#'
#' @export
AdjacencyMatrix <- function(x, ..., weights = .data$W_ij) {
  weights_quo <- rlang::enquo(weights)
  nodes <- purrr::map_chr(rlang::quos(...), rlang::f_text)
  i <- stringr::str_c(nodes, "i", sep = "_")
  j <- stringr::str_c(nodes, "j", sep = "_")
  ij <- c(i, j)
  node_names <-
    x %>%
    dplyr::distinct(rlang::UQS(rlang::syms(i))) %>%
    dplyr::arrange(rlang::UQS(rlang::syms(i))) %>%
    dplyr::mutate(
      Node = stringr::str_c(rlang::UQS(rlang::syms(i)), sep = "_")
    ) %>%
    dplyr::pull(.data$Node)
  adjacency_matrix <-
    x %>%
    dplyr::arrange(rlang::UQS(rlang::syms(ij))) %>%
    dplyr::pull(rlang::UQ(weights_quo)) %>%
    matrix(nrow = length(node_names), ncol = length(node_names),
           dimnames = list(node_names, node_names))
  return(adjacency_matrix)
}
