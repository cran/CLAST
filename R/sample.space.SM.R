
#' @export

sample.space.SM <-
function(n,a,b){
    # Creates a sample space object at the level of individual stage counts (for K=2 or K>2)
    #
    K=length(n)
    if(b[K]-a[K]>1){stop("Last boundary is not exclusive")}
    if(sum(a<b)<K){stop("Check valid vectors")}
    if(sum(a<b)<K){stop("Check valid vectors")}
    if(min(a)<(-1)){stop("Check valid vectors")}
    if(b[1]>n[1]+1){stop("Check valid vectors")}
    #
    if(K==2){obj=sample.space.2(n,a,b)}
    if(K>2){obj=sample.space(n,a,b)}

    tab=table(obj$S,obj$M)
    mvals=as.numeric(colnames(tab))
    svals=as.numeric(rownames(tab))
    tab.M=matrix(rep(mvals,length(svals)),nrow=length(svals),byrow=T)
    tab.S=matrix(rep(svals,length(mvals)),nrow=length(svals))
    out=NULL
    out$M=tab.M[tab>0]
    out$S=tab.S[tab>0]
    count=subcount=decision=N=NULL
    for(g in 1:length(out$M)){
        N=c(N,cumsum(obj$design[,1])[out$M[g]])
        tmp=prob.SM(obj,0,m=out$M[g],s=out$S[g])
        count=c(count,tmp$count)
        subcount=c(subcount,tmp$subcount)
        decision=c(decision,mean(obj$decision[obj$M==out$M[g]&obj$S==out$S[g]]))
    }
    out$N=N
    out$count=count
    out$subcount=subcount
    out$decision=decision
    out$design=obj$design
    out
}
