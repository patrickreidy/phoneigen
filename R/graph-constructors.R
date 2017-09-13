

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
    weights_quo <- rlang::sym("W_ij")
  }
  weights_name <- rlang::quo_name(weights_quo)
  condition_quo <- rlang::enquo(condition)
  values_quo <- rlang::enquo(values)
  if (! (weights_name %in% names(x))) {
    x <- dplyr::mutate(x, rlang::UQ(weights_name) := 0)
  }
  dplyr::mutate(
    x,
    rlang::UQ(weights_quo) := ifelse(rlang::eval_tidy(condition_quo, data = x),
                                     rlang::eval_tidy(values_quo, data = x),
                                     rlang::eval_tidy(weights_quo, data = x))
  )
}


#' Normalize Weights
#'
#' @param x A data frame or tibble.
#' @param weights The unquoted name of a numeric column in \code{x} whose values
#'   will be shaped into a square adjacency matrix.
#'
#' @return A data frame that is like \code{x}, except that the values of the
#'   \code{weights} variable, \code{W(i,j)} have been normalized such that
#'   \code{W_tilde(i, j) = (i/n) * (W(i,j) / sqrt(E_x[W(i,x)] * E_x[W(j,x)]))}.
#'
#' @export
Normalize <- function(x, weights = rlang::missing_arg()) {
  weights_quo <- rlang::enquo(weights)
  if (rlang::quo_name(weights_quo) == "rlang::missing_arg()") {
    weights_quo <- rlang::sym("W_ij")
  }
  i_syms <-
    setdiff(names(x), rlang::quo_name(weights_quo)) %>%
    stringr::str_subset(pattern = "_i$") %>%
    rlang::syms()
  i_classes <-
    x %>%
    dplyr::select(rlang::UQS(i_syms)) %>%
    purrr::map_chr(class)
  i_group_by <- i_syms[i_classes != "list"]
  j_syms <-
    setdiff(names(x), rlang::quo_name(weights_quo)) %>%
    stringr::str_subset(pattern = "_j$") %>%
    rlang::syms()
  j_classes <-
    x %>%
    dplyr::select(rlang::UQS(j_syms)) %>%
    purrr::map_chr(class)
  j_group_by <- j_syms[j_classes != "list"]
  n <- dplyr::distinct(x, rlang::UQS(i_group_by)) %>% nrow()
  x %>%
    dplyr::group_by(rlang::UQS(i_group_by)) %>%
    dplyr::mutate(E_i = mean(rlang::UQ(weights_quo))) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(rlang::UQS(j_group_by)) %>%
    dplyr::mutate(E_j = mean(rlang::UQ(weights_quo))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      rlang::UQ(weights_quo) := (1/n) * rlang::UQ(weights_quo) / sqrt(.data$E_i * .data$E_j)
    ) %>%
    dplyr::select(-.data$E_i, -.data$E_j)
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
    weights_quo <- rlang::sym("W_ij")
  }
  vars_quos <- rlang::quos(...)
  if (length(vars_quos) > 0) {
    vars <- purrr::map_chr(vars_quos, rlang::quo_name)
    i <- stringr::str_c(vars, "_i")
    j <- stringr::str_c(vars, "_j")
    ij <- c(i, j)
  } else {
    weights_name <- rlang::quo_name(weights_quo)
    x_classes <- purrr::map_chr(x, class)
    x_var_names <-
      names(x)[x_classes != "list"] %>%
      setdiff(weights_name)
    i <- stringr::str_subset(x_var_names, pattern = "_i$")
    j <- stringr::str_subset(x_var_names, pattern = "_j$")
    ij <- c(i, j)
  }
  nodes <-
    x %>%
    dplyr::distinct(rlang::UQS(rlang::syms(i))) %>%
    dplyr::arrange(rlang::UQS(rlang::syms(i))) %>%
    dplyr::mutate(Node = stringr::str_c(rlang::UQS(rlang::syms(i)), sep = "_")) %>%
    dplyr::pull(.data$Node)
  adjacency_matrix <-
    x %>%
    dplyr::arrange(rlang::UQS(rlang::syms(ij))) %>%
    dplyr::pull(rlang::UQ(weights_quo)) %>%
    matrix(nrow = length(nodes), ncol = length(nodes), dimnames = list(nodes, nodes))
  return(adjacency_matrix)
}

