
#' @export

mv.plots.SM <-
function(n,a,b,type="interval",B=100,offset=TRUE,plt=c(1,1,1),p0=NULL,p1=NULL,focus=FALSE){

    # reset "par" using on.exit()
    init_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(init_par))

    data.SM=sample.space.SM(n,a,b)
    #
    if(stats::var(c(length(n),length(a),length(b)))>0){stop("Unequal lengths")}
    K=length(n)
    if(b[K]-a[K]>1){stop("Last boundary is not exclusive")}
    if(sum(a<b)<K){stop("Check valid vectors")}
    if(sum(a<b)<K){stop("Check valid vectors")}
    if(min(a)<(-1)){stop("Check valid vectors")}
    if(b[1]>n[1]+1){stop("Check valid vectors")}
    #
    x=(1:B)/(B+1)
    if(focus){x=p0+(p1-p0)*x}
    #
    if((type=="upper")|(type=="interval")){
        exact.lr.upper.lims=exact.upper.limits.SM(LR.stats.SM(data.SM,type="upper"),set=T)
        exact.cp.upper.lims=exact.upper.limits.SM(CP.stats.SM(data.SM,type="upper"),set=T)
        jt.lims=JT.rank.SM(data.SM)$lims
        exact.jt.upper.lims=exact.upper.limits.SM(data.SM,lims=jt.lims,set=T)
        ml.lims=ML.rank.SM(data.SM)$lims
        exact.ml.upper.lims=exact.upper.limits.SM(data.SM,lims=ml.lims,set=T)
    }
    #
    if((type=="lower")|(type=="interval")){
        exact.lr.lower.lims=exact.lower.limits.SM(LR.stats.SM(data.SM,type="lower"),set=T)
        exact.cp.lower.lims=exact.lower.limits.SM(CP.stats.SM(data.SM,type="lower"),set=T)
        jt.lims=JT.rank.SM(data.SM)$lims
        exact.jt.lower.lims=exact.lower.limits.SM(data.SM,lims=jt.lims,set=T)
        ml.lims=ML.rank.SM(data.SM)$lims
        exact.ml.lower.lims=exact.lower.limits.SM(data.SM,lims=ml.lims,set=T)
    }
    #
    value=NULL
    if((type=="upper")|(type=="interval")){
        cp.upper.mv=mv.SM(data.SM,exact.cp.upper.lims,B=B,p=x,offset=offset)$y
        lr.upper.mv=mv.SM(data.SM,exact.lr.upper.lims,B=B,p=x,offset=offset)$y
        jt.upper.mv=mv.SM(data.SM,exact.jt.upper.lims,B=B,p=x,offset=offset)$y
        ml.upper.mv=mv.SM(data.SM,exact.ml.upper.lims,B=B,p=x,offset=offset)$y
        value$cp.upper.mv=cp.upper.mv
        value$lr.upper.mv=lr.upper.mv
        value$jt.upper.mv=jt.upper.mv
        value$ml.upper.mv=ml.upper.mv
    }
    if((type=="lower")|(type=="interval")){
        cp.lower.mv=mv.SM(data.SM,exact.cp.lower.lims,B=B,p=x,offset=offset)$y
        lr.lower.mv=mv.SM(data.SM,exact.lr.lower.lims,B=B,p=x,offset=offset)$y
        jt.lower.mv=mv.SM(data.SM,exact.jt.lower.lims,B=B,p=x,offset=offset)$y
        ml.lower.mv=mv.SM(data.SM,exact.ml.lower.lims,B=B,p=x,offset=offset)$y
        value$cp.lower.mv=cp.lower.mv
        value$lr.lower.mv=lr.lower.mv
        value$jt.lower.mv=jt.lower.mv
        value$ml.lower.mv=ml.lower.mv
    }
    # Plotting follows
    graphics::par(mfrow=c(1,sum(plt)))
    #
    if(plt[1]==1){
        graphics::plot(range(x),range(c(jt.upper.mv-ml.upper.mv,cp.upper.mv-ml.upper.mv,lr.upper.mv-ml.upper.mv)),
             type="n",xlab="p",ylab="",las=1)
        graphics::lines(x,jt.upper.mv-ml.upper.mv,col="red")
        graphics::lines(x,cp.upper.mv-ml.upper.mv,col="green")
        graphics::lines(x,lr.upper.mv-ml.upper.mv,col="blue")
        graphics::abline(h=0,lty=3)
        graphics::title(main="Mean exact upper limits")
        graphics::title(sub="JT(red),LR(blue),CP(green)")
        if(!missing(p0)){graphics::abline(v=p0,lty=3)}
        if(!missing(p1)){graphics::abline(v=p1,lty=3)}  }
    #
    if(plt[2]==1){
        graphics::plot(range(x),range(c(jt.lower.mv-ml.lower.mv,cp.lower.mv-ml.lower.mv,lr.lower.mv-ml.lower.mv)),type="n",xlab="p",
             ylab="",las=1)
        graphics::lines(x,jt.lower.mv-ml.lower.mv,col="red")
        graphics::lines(x,cp.lower.mv-ml.lower.mv,col="green")
        graphics::lines(x,lr.lower.mv-ml.lower.mv,col="blue")
        graphics::abline(h=0,lty=3)
        graphics::title(main="Mean exact lower limits")
        graphics::title(sub="JT(red),LR(blue),CP(green)")
        if(!missing(p0)){graphics::abline(v=p0,lty=3)}
        if(!missing(p1)){graphics::abline(v=p1,lty=3)}  }
    #
    ml.int.mv=ml.upper.mv-ml.lower.mv
    cp.int.mv=cp.upper.mv-cp.lower.mv
    lr.int.mv=lr.upper.mv-lr.lower.mv
    jt.int.mv=jt.upper.mv-jt.lower.mv
    #
    if(plt[3]==1){
        y1=jt.int.mv-ml.int.mv
        y2=lr.int.mv-ml.int.mv
        y3=cp.int.mv-ml.int.mv
        graphics::plot(range(x),range(c(y1,y2,y3)),type="n",las=1,xlab="p",ylab="",las=1)
        graphics::lines(x,jt.int.mv-ml.int.mv,col="red")
        graphics::lines(x,lr.int.mv-ml.int.mv,col="blue")
        graphics::lines(x,cp.int.mv-ml.int.mv,col="green")
        graphics::abline(h=0,lty=3)
        graphics::title(main="Mean interval width")
        if(!missing(p0)){graphics::abline(v=p0,lty=3)}
        if(!missing(p1)){graphics::abline(v=p1,lty=3)}
        graphics::title(sub="JT(red),LR(blue),CP(green)")
        }
    value
}
