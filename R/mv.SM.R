
#' @export

mv.SM <-
function(obj,lims=NULL,p=NULL,B=99,offset=TRUE,wgt=TRUE){
    # Calculates mean value of upper limits in lims as a function of p
    # If offset=T then MLE is subtracted.
    # If wgt=T then outcome of all successes is given probability zer0.
    #
    probs.SM=function(obj,p){obj$count*p^obj$S*(1-p)^(obj$N-obj$S)}
    if(missing(lims)){lims=obj$lims}
    if(missing(p)){p=(1:B)/(B+1)}
    take=(lims==1)
    lims=lims-offset*obj$S/obj$N
    mv=NULL
    for(P in as.vector(p)){
        prob.dist=probs.SM(obj,P)
        if(wgt){
            prob.dist[take]=0
            prob.dist=prob.dist/sum(prob.dist)
        }
        mv=c(mv,sum(prob.dist*lims))
    }
    list(x=p,y=mv)
}
