friedmanTest <- function (data, ...) {
  N <- dim(data)[1]
  k <- dim(data)[2]
  mr <- colMeans(rankMatrix(data))

  friedman.stat <- 12 * N / (k * (k + 1)) * (sum(mr^2) - (k * (k + 1)^2) / 4)
  p.value <- 1 - pchisq(friedman.stat, df=(k - 1))

  names(friedman.stat) <- "Friedman's chi-squared"
  parameter <- (k - 1)
  names(parameter) <- c("df")
  method <- "Friedman's rank sum test"
  data.name <- deparse(substitute(data))
  htest.result <- list(statistic=friedman.stat, parameter=parameter,
                       p.value=p.value, method=method, data.name=data.name)
  class(htest.result) <- "htest"
  return(htest.result)
}
