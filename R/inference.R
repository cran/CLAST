
#' @export

inference <-
    function(n,a,b,y,alpha=.05,type="LR"){
    # Calculated upper and lower limits after sequential trial
    #
    m=length(y)
    s=sum(y)
    data.SM=sample.space.SM(n,a,b)
    i=(data.SM$M==m)&(data.SM$S==s) #Select data set from sample space
    if(sum(i)==0){stop("Impossible observation vector for given design")}
    #
    if(type=="CP"){
        upper.lims=exact.upper.limits.SM(CP.stats.SM(data.SM,alpha=alpha,type="upper"),alpha=alpha,set=T)
        lower.lims=exact.lower.limits.SM(CP.stats.SM(data.SM,alpha=alpha,type="lower"),alpha=alpha,set=T)
    }
    #
    if(type=="LR"){
        upper.lims=exact.upper.limits.SM(LR.stats.SM(data.SM,alpha=alpha,type="upper"),alpha=alpha,set=T)
        lower.lims=exact.lower.limits.SM(LR.stats.SM(data.SM,alpha=alpha,type="lower"),alpha=alpha,set=T)
    }
    #
    if(type=="JT"){
        upper.lims=exact.upper.limits.SM(JT.rank.SM(data.SM),alpha=alpha,set=T)
        lower.lims=exact.lower.limits.SM(JT.rank.SM(data.SM),alpha=alpha,set=T)
    }
    #
    if(type=="ML"){
        upper.lims=exact.upper.limits.SM(ML.rank.SM(data.SM),alpha=alpha,set=T)
        lower.lims=exact.lower.limits.SM(ML.rank.SM(data.SM),alpha=alpha,set=T)
    }
    #
    list(lower=lower.lims[i],upper=upper.lims[i],est=s/sum(n[1:m]),type=type)
}


