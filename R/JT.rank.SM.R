
#' @export

JT.rank.SM <-
function(obj){
    # Calculates Jennison & Turnbull (1983) ranking of sample space
    # REQUIRED ARGUMENT
    # Object returned from function sample.space
    #
    r.s=RANK(obj$S+obj$decision*(dim(obj$design)[1]+10-obj$M)*100)
    out=obj
    out$lims=r.s
    out
}
