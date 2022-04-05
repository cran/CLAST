
#' @export

CP.lower <-
function (x, n, a = 0.05)
{
    #       FUNCTION COMPUTES EXACT CONFIDENCE INTERVAL FOR
    #       BINOMIAL PROBABILITY BY INVERSION OF THE EXACT TEST.
    #
    #       REQUIRED ARGUMENTS:
    #       x - observed value (scalar)
    #       n - number of trials
    #
    if (x > 0 & x < n) {lower <- stats::qbeta(a,x,n-x+1)}
    if (x == 0) {lower <- 0}
    if (x == n) {lower <- a^(1/n)}
    lower
}
