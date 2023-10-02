# process the argument as a tidyselect expression and return a list of expressions
.process_selectors_as_exprs <- function(data, x) {
  # if argument is already a list of expressions, return unaltered
  if (.is_expr_list(x)) return(x)

  # perform selection and return column names as a list of expressions
  dplyr::select(data, {{ x }}) %>%
    names() %>%
    rlang::syms()
}

# this currently does not handle the case when merging on differently named columns
.process_by_arg_as_exprs <- function(data, x) {
  # if argument is already a list of expressions, return unaltered
  if (.is_expr_list(x)) return(x)

  # if user passed `dplyr::join_by(USUBJID)`, process as list of expressions
  if (.is_join_by(x)) {
    return(x$exprs)
  }

  # this handles the classic character specification of the by variables,
  # e.g. `left_join(..., by = "USUBJID")`
  rlang::syms(x)
}

# predicate whether arg is a list of expressions (as is returned from `rlang::exprs()`)
.is_expr_list <- function(x) {
  tryCatch(
    rlang::is_list(x) && all(unlist(lapply(x, function(e) rlang::is_expression(e)))),
    error = function(e) FALSE
  )
}

# predicate whether arg is a `dplyr::join_by()` object
.is_join_by <- function(x) {
  tryCatch(
    inherits(x, "dplyr_join_by"),
    error = function(e) FALSE
  )
}
