
reused <- function(x,ny = 1) {

  nx <- length(x)

  ngr <- (ny %/% nx) + 1

  ind <- rep(1:nx, ngr) %>% head(ny)

  x[ind]

}

