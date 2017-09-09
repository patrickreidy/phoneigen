

#' Cartesian Square
#'
#' Compute the Cartesian Square of a data frame or a tibble.
#'
#' This is a thin wrapper for \code{\link[tidyr]{crossing}} that also performs
#' some string formatting on the column names of the data frame or tibble.
#'
#' @param x A data frame or \code{\link[tibble]{tibble}}.
#' @param ... Unquoted names of variables in \code{x}. These are first
#'   \code{dplyr::select}ed from \code{x}, and only these variables are
#'   square with \code{tidyr::crossing}.
#' @param weights An optional unquoted name of a weights variable.
#'   Default value is \code{rlang::missing_arg()}, which
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
CartesianSquare <- function(x, ..., weights = rlang::missing_arg()) {
  vars <- rlang::quos(...)
  x <- dplyr::select(x, rlang::UQS(vars))
  i <- purrr::set_names(x, nm = stringr::str_c(names(x), "i", sep = "_"))
  j <- purrr::set_names(x, nm = stringr::str_c(names(x), "j", sep = "_"))
  square <- tidyr::crossing(i, j)
  weights_name <- rlang::quo_name(rlang::enquo(weights))
  if (weights_name != "rlang::missing_arg()") {
    square <- dplyr::mutate(square, rlang::UQ(weights_name) := 0)
  }
  return(square)
}



#' Weight the Edges Between Nodes
#'
#' Set the edge weights between nodes in a graph that is represented like
#' a \code{\link{CartesianSquare}} data frame or tibble.
#'
#' @param x A data frame or \code{\link[tibble]{tibble}}.
#' @param weights An unquoted name of the output variable where the weights are
#'   populated.
#' @param condition An unquoted expression that, within \code{x}, evaluates to
#'   a logical vector.
#' @param values An unquoted expression that, within \code{x}, evaluates to a
#'   numeric vector. The values of the \code{name} variable when
#'   \code{condition} evaluates to \code{TRUE}.
#'
#' @return A tibble that is like \code{x} except that it has a variable
#'   \code{name} whose values are determined by \code{condition} and
#'   \code{weights}.
#'
#' @export
WeightEdgesIf <- function(x, weights = rlang::missing_arg(), condition, values) {
  weights_quo <- rlang::enquo(weights)
  if (rlang::quo_name(weights_quo) == "rlang::missing_arg()") {
    weights_quo <- rlang::quo(W_ij)
  }
  weights_name <- rlang::quo_name(weights_quo)
  condition_quo <- rlang::enquo(condition)
  values_quo <- rlang::enquo(values)
  if (! (weights_name %in% names(x))) {
    x <- dplyr::mutate(x, rlang::UQ(weights_name) := 0)
  }
  dplyr::mutate(
    x,
    rlang::UQ(weights_name) := ifelse(rlang::eval_tidy(condition_quo, data = x),
                                      rlang::eval_tidy(values_quo, data = x),
                                      rlang::eval_tidy(weights_quo, data = x))
  )
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
AdjacencyMatrix <- function(x, ..., weights = rlang::missing_arg()) {
  weights_quo <- rlang::enquo(weights)
  if (rlang::quo_name(weights_quo) == "rlang::missing_arg()") {
    weights_quo <- rlang::quo(W_ij)
  }
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
  # weights_quo <- rlang::enquo(weights)
  # nodes <- purrr::map_chr(rlang::quos(...), rlang::f_text)
  # i <- stringr::str_c(nodes, "i", sep = "_")
  # j <- stringr::str_c(nodes, "j", sep = "_")
  # ij <- c(i, j)
  # node_names <-
  #   x %>%
  #   dplyr::distinct(rlang::UQS(rlang::syms(i))) %>%
  #   dplyr::arrange(rlang::UQS(rlang::syms(i))) %>%
  #   dplyr::mutate(
  #     Node = stringr::str_c(rlang::UQS(rlang::syms(i)), sep = "_")
  #   ) %>%
  #   dplyr::pull(.data$Node)
  # adjacency_matrix <-
  #   x %>%
  #   dplyr::arrange(rlang::UQS(rlang::syms(ij))) %>%
  #   dplyr::pull(rlang::UQ(weights_quo)) %>%
  #   matrix(nrow = length(node_names), ncol = length(node_names),
  #          dimnames = list(node_names, node_names))
  # return(adjacency_matrix)
}
