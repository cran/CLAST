
#' @export

exact.lower.limits.SM <-
function(obj,lims=NULL,alpha=0.05,set=FALSE){
    #   Calculates Buehler upper limit for all possible data sets.
    #   There is protection for no solution. Largest value is automatically set to 1.
    #
    #   ARGUMENTS:
    #    obj - produced from sample.space.SM,must have components data and lims
    #    set - if true then artificial limits of 0 are modified to min(u:u>0) as described in Kabaila and Lloyd (2003).
    #
    probs.SM=function(obj,p){obj$count*p^obj$S*(1-p)^(obj$N-obj$S)}
    pr.tail.SM=function(p,obj,lims,J,alpha=0.05){sum(probs.SM(obj,p)[lims<=lims[J]])-alpha}
    #
    if(missing(lims)){lims=obj$lims}
    lims=-lims
    exact.lims=NULL
    for(J in 1:(length(lims))){
        if(lims[J]==max(lims)){exact.lims=c(exact.lims,0)}
        if(lims[J]<max(lims)){
            test=sign(pr.tail.SM(0,obj,lims,J,alpha)*pr.tail.SM(1,obj,lims,J,alpha))
            if(test>=0){exact.lims=c(exact.lims,1)}
            if(test<0){exact.lims=c(exact.lims,uniroot(pr.tail.SM,c(0,1),obj=obj,lims=lims,J=J,alpha=alpha)$root)}
        }
    }
    if(set){
        exact.lims[exact.lims==1]=max(exact.lims[exact.lims<1])
    }
    for(v in 1:length(lims)){lims[lims==lims[v]]=exact.lims[v]}
    signif(exact.lims,6)
}
