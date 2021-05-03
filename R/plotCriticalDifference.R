
#' Critical Difference Diagram
#'
#' @description Creates a critical difference diagram as shown in Demsar (2006)
#' @param data Data Frame or Matrix with results of each algorithm / scenario
#' @param method Method by which to perform comparisons
#' @param alpha Significance level to get the critical difference.
#' @param cex Font Size
#' @param ... Additional arguments for \code{\link{rankMatrix}}
#' @references Demsar, J. (2006) Statistical Comparisons of Classifiers over Multiple Data Sets.
#' @return Plot (TODO: silent object with pairs)
#'
#' @examples
#' # Not Run
#' # criticalDifferenceDiagram(someData)
#' @export
criticalDifferenceDiagram <-
  function (data,
            method = "NemenyiCD",
            alpha = 0.05,
            cex = 0.75,
            ...) {
    # prep plot by defining margins in inches
    opar <- par(mai = c(0, 0, 0, 0))
    # set exit to reset plot environment
    on.exit(par(opar))


    # number of algorithms
    k <- dim(data)[2]
    # number of scenarios
    N <- dim(data)[1]

    # Friedman test is performed to reject the null hypothesis
    friedman.htest <- friedmanTest(data)
    if (friedman.htest$p.value >= alpha) {
      warning("The null hypothesis over the entire set of classifiers cannot be rejected")
    }

    # calculate Nemenyi Critical Difference Value
    cd <- nemenyiCDVal(alpha = alpha,
                       num.alg = k,
                       num.problems = N)

    # Reorder results
    mean.rank <- sort(colMeans(rankMatrix(data, ...)))

    # Separate the algorithms in left and right parts
    lp <- round(k / 2)
    left.algs <- mean.rank[1:lp]
    right.algs <- mean.rank[(lp + 1):k]
    max.rows <- ceiling(k / 2)

    ## Setting Basic dimensions and definitions
    # Character size
    char.size    <- 0.001
    # Line spacing for the algorithm name
    line.spacing <- 0.25
    # Integer Smallest Value to plot on axis
    m            <- floor(min(mean.rank))
    # Integer Largest Value to plot on axis
    M            <- ceiling(max(mean.rank))
    # Longest length of a label
    max.char     <- max(sapply(colnames(data), FUN = nchar))
    # Calculate text width of all labels
    text.width   <- (max.char + 4) * char.size
    w            <- (M - m) + 2 * text.width
    # The upper part is fixed. Extra space is for the CD
    h.up         <- 2.5 * line.spacing
    # The lower part depends on the no. of algorithms.
    h.down       <- (max.rows + 2.25) * line.spacing
    # The 2 extra spaces are for the lines that join algorithms
    tick.h       <- 0.25 * line.spacing

    # Displacement of the label with respect to the axis
    label.displacement <- 0.25
    # Displacement for the lines that join algorithms
    line.displacement  <- 0.025

    # Background of the plot
    plot(
      0,
      0,
      type = "n",
      xlim = c(m - w / (M - m), M + w / (M - m)),
      ylim = c(-h.down, h.up),
      xaxt = "n",
      yaxt = "n",
      xlab = "",
      ylab = "",
      bty = "n"
    )

    # Draw the x-axis
    lines (c(m, M), c(0, 0))
    dk <- sapply(
      m:M,
      FUN = function(x) {
        lines(c(x, x), c(0, tick.h))
        text(x, 3 * tick.h, labels = x, cex = cex)
      }
    )

    # Draw the critical difference
    # lines(c(m, m + cd), c(1.75 * line.spacing, 1.75 * line.spacing))
    # text(m + cd / 2, 2.25 * line.spacing, "CD", cex=cex)
    # lines(c(m, m), c(1.75 * line.spacing - tick.h / 4,
    #                  1.75 * line.spacing + tick.h / 4))
    # lines(c(m + cd, m + cd), c(1.75 * line.spacing - tick.h / 4,
    #                            1.75 * line.spacing + tick.h / 4))

    # Left part, labels
    dk <- sapply (
      1:length(left.algs),
      FUN = function(x) {
        line.h <- -line.spacing * (x + 2)
        text(
          x = m - label.displacement,
          y = line.h,
          labels = names(left.algs)[x],
          cex = cex,
          adj = 1
        )
        lines(c(m - label.displacement * 0.75, left.algs[x]),
              c(line.h, line.h))
        lines(c(left.algs[x], left.algs[x]), c(line.h, 0))
      }
    )

    # Right part, labels
    dk <- sapply (
      1:length(right.algs),
      FUN = function(x) {
        line.h <- -line.spacing * (x + 2)
        text(
          x = M + label.displacement,
          y = line.h,
          labels = names(right.algs)[x],
          cex = cex,
          adj = 0
        )
        lines(c(M + label.displacement * 0.75, right.algs[x]),
              c(line.h, line.h))
        lines(c(right.algs[x], right.algs[x]), c(line.h, 0))
      }
    )

    # Draw the lines to join algorithms
    getInterval <- function (x) {
      from <- mean.rank[x]
      diff <- mean.rank - from
      ls <- which(diff > 0 & diff < cd)
      if (length(ls) > 0) {
        c(from, mean.rank[max(ls)])
      }
    }

    intervals <- mapply (1:k, FUN = getInterval)
    aux <- do.call(rbind, intervals)
    if (NROW(aux) > 0) {
      # With this strategy, there can be intervals included into bigger ones
      # We remove them in a sequential way
      to.join <- aux[1, ]
      if (nrow(aux) > 1) {
        for (r in 2:nrow(aux)) {
          if (aux[r - 1, 2] < aux[r, 2]) {
            to.join <- rbind(to.join, aux[r,])
          }
        }
      }

      row <- c(1)
      # Determine each line in which row will be displayed
      if (!is.matrix(to.join)) {
        # To avoid treating vector separately
        to.join <- t(as.matrix(to.join))
      }
      nlines <- dim(to.join)[1]

      for (r in 1:nlines) {
        id <- which(to.join[r, 1] > to.join[, 2])
        if (length(id) == 0) {
          row <- c(row, tail(row, 1) + 1)
        } else {
          row <- c(row, min(row[id]))
        }
      }

      step <- max(row) / 2

      # Draw the line
      dk <- sapply (
        1:nlines,
        FUN = function(x) {
          y <- -line.spacing * (0.5 + row[x] / step)
          lines(c(
            to.join[x, 1] - line.displacement,
            to.join[x, 2] + line.displacement
          ),
          c(y, y),
          lwd = 3)
        }
      )
    }
  }
