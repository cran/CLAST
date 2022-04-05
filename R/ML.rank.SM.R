
#' @export

ML.rank.SM <-
function(obj){
    # Calculated ratio ranking of sample space
    # REQUIRED ARGUMENT
    # Object returned from function sample.space
    #
    out=obj
    out$P=obj$S/cumsum(obj$design[,1])[obj$M]
    out$lims=signif(out$P,6)
    out
}
