
#' @export

CP.upper <-
function (x, n, a = 0.05)
{
    #       FUNCTION COMPUTES EXACT CONFIDENCE INTERVAL FOR
    #       BINOMIAL PROBABILITY BY INVERSION OF THE EXACT TEST.
    #
    #       REQUIRED ARGUMENTS:
    #       x - observed value (scalar)
    #       n - number of trials
    #
    if (x > 0 & x < n) {upper <- stats::qbeta(1-a,x+1,n-x)}
    if (x == 0) {upper <- 1 - a^(1/n)}
    if (x == n) {upper <- 1}
    upper
}
