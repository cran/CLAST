
#' @export

ML.rank.SM <-
function(obj){
    # Calculated ML estimator of p for each element of sample space.
    # This is a potential ranking for exact limits.
    # REQUIRED ARGUMENT
    # Object returned from function sample.space
    #
    out=obj
    out$P=obj$S/cumsum(obj$design[,1])[obj$M]
    out$lims=signif(out$P,6)
    out
}
