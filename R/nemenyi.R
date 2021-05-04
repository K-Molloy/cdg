#' Calculate Nemenyi Critical Difference Value
#'
#' @description The Nemenyi test is similar to the Tukey test for ANOVA and is used when
#' classifiers are compared to each other. The performance of two classifiers is
#' significantly different if the corresponding ranks differ by at least the
#' critical difference \eqn{CD = q_\alpha \frac{k(k+1)}{6N}^{-2}}}
#' where critical values \eqn{q_\alpha} are based on the Studentised range
#' statistic divided by \eqn{\sqrt{2}}.
#' @param alpha The significance size. E.g 95\% is 0.05
#' @param num.classifiers Number of classifiers tested
#' @param num.scenarios Number of scenarios of implementation
#'
#' @return Critical Difference Value
#'
#' @details Not to be mistaken with the Nemenyi Test
#' @references Nemenyi, P.B. (1963) Distribution-free Multiple Comparisons. PhD thesis, Princeton University.
#' @references Demsar, J. (2006) Statistical Comparisons of Classifiers over Multiple Data Sets.
#' @examples
#' cd = nemenyiCDVal(num.classifiers = 3, num.scenarios = 4)
#' @export
nemenyiCDVal <-
  function (alpha = 0.05, num.classifiers, num.scenarios) {
    # degrees of freedom
    df <- num.classifiers * (num.scenarios - 1)
    # q_alpha value using Tukey helper function
    qa <- qtukey(p = 1 - alpha,
                 nmeans = num.classifiers,
                 df = df) / sqrt(2)
    # critical difference using formula described in details
    cd <-
      qa * sqrt((num.classifiers * (num.classifiers + 1)) / (6 * num.scenarios))
    # return critical difference
    return(cd)
  }



#' Nemenyi Test
#' The Nemenyi test is a post-hoc test intended to find the groups of data that
#' differ aftera  statistical test of multiple comparisons (such as the
#' Friedman test) has rejected the null hypothesis that the performance of the
#' comparisons on the groups of data is similar.
nemenyiTest <- function() {
  # still need to add this :p
}
