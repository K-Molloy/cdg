
wilcoxonHolmTest <- function(data, ...){

}

wilcoxonSignedTest <- function (x, y, ...) {
  if (length(x) != length(y)) {
    stop("This is a paired test, so the two vectors have to have the same length")
  }
  N  <- length(x)
  d  <- x - y
  # Compute the statistic based on the ordering of the differences.
  # We need to assign the highest rank to the biggest value, so we change the
  # sign of the absolute difference.
  o  <- rank(abs(d), ties.method = "average")
  rp <- sum(o[d > 0]) + 0.5 * sum(o[d == 0])
  rn <- sum(o[d < 0]) + 0.5 * sum(o[d == 0])
  t  <- min(rp, rn)

  num    <- t - 0.25 * (N * (N + 1))
  den    <- sqrt((N * (N + 1) * (2 * N + 1)) / 24)
  z      <- num / den
  pvalue <- pnorm(z)

  names(t)  <- "T"
  method    <- "Wilcoxon Signed-Rank test"
  data.name <- deparse(substitute(data))

  # Create and return the htest object
  res <-
    list(
      statistic = t,
      p.value = pvalue,
      method = method,
      data.name = data.name
    )
  class(res) <- "htest"
  return(res)
}
