
#' @export

sample.space <-
function(n,a,b){
    # Creates all possible samples from a multi-stage group sequential trial.
    #
    # VALUE
    # list with components
    #         Y - matrix with K columns listing all possible data sets. Outcomes where the trial does not proceed are coded -1.
    #         M - number of executed trials corresponding to each data set in Y
    #         S - total successes corresponding to each data set in Y
    #  decision - to conclude H1 or H0
    #
    # Error checks
    if(stats::var(c(length(n),length(a),length(b)))>0){stop("Unequal lengths")}
    K=length(a)
    if(b[K]-a[K]>1){stop("Last boundary is not exclusive")}
    if(sum((a<=cumsum(n))*(b<=cumsum(n)*(a<b)))<K){stop("Check valid vectors")}
    #
    K=length(a)
    if(a[1]<0){Y=as.matrix(c(b[1]:n[1]))}
    if(a[1]>=0){Y=as.matrix(c(0:a[1],b[1]:n[1]))}
    for(j in 1:(K-1)){Y=cbind(Y,rep(-1,dim(Y)[1]))}
    decision=c(rep(0,a[1]+1),rep(1,n[1]-b[1]+1))
    #
    tmp=as.matrix(0:n[1])
    H0=(tmp<=a[1])
    H1=(tmp>=b[1])
    cont=!(H0|H1)
    #
    if(K>=2){
        for(k in 2:(K-1)){
            tmp=cross(tmp[cont,],0:n[k])
            S=apply(tmp,1,sum)
            H0=(S<=a[k])
            H1=(S>=b[k])
            cont=!(H0|H1)
            out=as.matrix(tmp[H0|H1,])
            decision=c(decision,apply(out,1,sum)>=b[k])
            for(j in 1:(K-dim(out)[2])){out=cbind(out,rep(-1,dim(out)[1]))}
            Y=rbind(Y,out)
        }
    }
    out=cross(tmp[cont,],0:n[K])
    decision=c(decision,apply(out,1,sum)>=b[K])
    Y=rbind(Y,out)
    #
    M=apply(Y>=0,1,sum)
    S=apply(Y,1,sum)+(K-M)
    Y=Y[order(S),]
    M=M[order(S)]
    decision=decision[order(S)]
    S=sort(S)
    Y=Y[order(M),]
    S=S[order(M)]
    decision=decision[order(M)]
    M=sort(M)
    #
    list(Y=Y,M=M,S=S,decision=decision,design=cbind(n,a,b))
}
