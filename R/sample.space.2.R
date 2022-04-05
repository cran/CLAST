
#' @export

sample.space.2 <-
function(n,a,b){
    # Creates all possible samples from a TWO group sequential trial with possible one-sided stopping at stage 1.
    #
    # VALUE
    # list with components
    #         Y - matrix with K columns listing all possible data sets. Outcomes where the trial does not proceed are coded -1.
    #         M - number of executed trials corresponding to each data set in Y
    #         S - total successes corresponding to each data set in Y
    #  decision - to conclude H1 or H0
    #
    # Error checks
    if(var(c(length(n),length(a),length(b)))>0){stop("Unequal lengths")}
    K=length(n)
    if(b[K]-a[K]>1){stop("Last boundary is not exclusive")}
    if(sum(a<b)<K){stop("Check valid vectors")}
    if(sum(a<b)<K){stop("Check valid vectors")}
    if(min(a)<(-1)){stop("Check valid vectors")}
    if(b[1]>n[1]+1){stop("Check valid vectors")}
    #
    if(a[1]<0){Y=as.matrix(c(b[1]:n[1]))}
    if(b[1]>n[1]){Y=as.matrix(c(0:a[1]))}
    if((a[1]>=0)&(b[1]<=n[1])){Y=as.matrix(c(0:a[1],b[1]:n[1]))}
    S=Y
    M=0*Y+1
    decision=1*(Y>a[1])
    #
    Y=cbind(Y,0*Y-1)
    #
    for(s in (a[1]+1):a[2]){
        for(y1 in max((a[1]+1),s-n[2]):min(s,(b[1]-1))){
            decision=c(decision,0)
            S=c(S,s)
            M=c(M,2)
            Y=rbind(Y,c(y1,s-y1))
        }
    }

    for(s in b[2]:(b[1]-1+n[2])){
        for(y1 in max((a[1]+1),s-n[2]):min(s,(b[1]-1))){
            decision=c(decision,1)
            S=c(S,s)
            M=c(M,2)
            Y=rbind(Y,c(y1,s-y1))
        }
    }
    #
    cbind(Y,S,M,decision)
    #
    list(Y=Y,M=M,S=S,decision=decision,design=cbind(n,a,b))
}
