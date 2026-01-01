
reused <- function(x,ny = 1) {

  nx <- length(x)

  ngr <- (ny %/% nx) + 1

  ind <- rep(1:nx, ngr) %>% head(ny)

  x[ind]

}

rewrite_quosure <- function(q) {
  # get expression and environment from the quosure
  expr <- rlang::quo_get_expr(q)
  env  <- rlang::quo_get_env(q)

  # require a call like `num < 6`
  if (!is.call(expr) || length(expr) < 2) {
    abort("quosure does not contain a binary call-like expression")
  }

  # replace the LHS (typically [[2]] for `num < 6`) with .x
  expr[[2]] <- rlang::sym(".x")

  # create function(.x) expr with the original quosure environment
  # alist(.x=) makes a pairlist with a single argument `.x`
  rlang::new_function(alist(.x = ), expr, env)

}
