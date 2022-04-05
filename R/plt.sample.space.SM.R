
#' @export

plt.sample.space.SM <-
function(n,a,b,p0=NULL,p1=NULL){
    # Plots decision function in S-M space with boundary vectors "a" and "b". If p0 and p1 are
    # provided then the type 1 and type 2 error are displayed in the graphic title
    #
    space.SM=sample.space.SM(n,a,b)
    K=dim(space.SM$design)[1]
    graphics::plot(space.SM$M,space.SM$S,type="n",xlab="stage stopped",ylab="total responses",lab=c(K,6,K))
    graphics::points(space.SM$M[space.SM$decision==1],space.SM$S[space.SM$decision==1],pch=20)
    graphics::points(space.SM$M[space.SM$decision==0],space.SM$S[space.SM$decision==0])
    graphics::lines(1:K,space.SM$design[,2],lty=3)
    graphics::lines(1:K,space.SM$design[,3],lty=3)
    if(!missing(p0)&!missing(p1)){
        err=errors.SM(n,a,b,p0,p1)
        err$type1=round(err$type1*1000)/1000
        err$type2=round(err$type2*1000)/1000
        lab=paste("alpha=",err$type1,", beta=",err$type2,sep="")
        graphics::title(main=lab)
    }
    NULL
}
