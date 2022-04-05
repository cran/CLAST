
#' @export

CP.stats.SM <-
function(obj,alpha=0.05,type="upper"){
    #  Calculates all possible upper limits for p from an object
    #  that contains all possible data sets from a group sequential trial.
    lims=NULL
    for(h in 1:length(obj$S)){
        if(type=="upper"){lims=c(lims,CP.upper(obj$S[h],sum(obj$design[,1][1:obj$M[h]]),a=alpha))}
        if(type=="lower"){lims=c(lims,CP.lower(obj$S[h],sum(obj$design[,1][1:obj$M[h]]),a=alpha))}
    }
    out=obj
    out$lims=signif(lims,6)
    out
}
