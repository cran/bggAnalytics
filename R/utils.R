.to_string <- function(x) paste0("'", x, "'", collapse = ", ")


#' Compress vector into single string
#'
#' Returns string which shows \code{n_show} first elements and \code{vec} and
#' amount of truncated elements.
#'
#' @param vec Atomic vector.
#' @param n_show Positive integer.
#' @param collapse Single string, how to collapse the given vector.
#'
#' @return Single string.
#' @keywords internal
#'
.compress <- function(vec, n_show = 5, collapse = ", ")
{
    # Assertions
    assert_atomic(vec)
    assert_count(n_show)
    assert_string(collapse)

    n <- length(vec)
    extra <- n - n_show
    vec <- vec[seq_len(min(n, n_show))]

    string <- paste0(vec, collapse = collapse)
    if (extra > 0) {
        string <- paste0(string, "... (", extra, " more)")
    }

    return(string)
}


#' Add '-s' to the end of a string based on a count
#'
#' Does exactly what the title says.
#'
#' @param string a single string.
#' @param count a single number that determines the addition of "-s".
#'
#' @return A single string.
#' @keywords internal
#'
.plural <- function(string, count)
{
    # Assertions
    assert_string(string)
    assert_integerish(count, lower = 0)

    if (count > 1) {
        string <- paste0(string, "s")
    }

    return(string)
}


#' Split according to a list
#'
#' This splits \code{x} using \code{\link[base]{split}} by making sure that
#' the returned list is of the same length as \code{list} and every element
#' is of the same length as \code{list}'s elements.
#'
#' @param x an \code{R} object to split.
#' @param list a list.
#'
#' @return A list.
#' @keywords internal
#'
.split_acc2list <- function(x, list)
{
    assert_list(list)

    vec <- seq_along(list)
    lens <- lengths(list)
    splitter <- factor(rep(vec, lens), levels = vec)
    result <- unname(split(x, splitter))
    return(result)
}

#' Get one of the internal functions from the package
#'
#' This makes sure internal functions from this package can be accessed which
#' sometimes fails when using \code{match.fun}.
#'
#' @param fun_name a single string.
#'
#' @return A function.
#' @keywords internal
#'
.internal_fun <- function(fun_name)
{
    get(fun_name, envir = asNamespace("bggAnalytics"))
}
