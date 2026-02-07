#' @export
#'
build_with_stamp <- function() {
  cat("\nBuild started:", format(Sys.time()), "\n")
  devtools::build()
  cat("Build finished:", format(Sys.time()), "\n")
}
