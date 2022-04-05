
#' @export

prob.SM <-
function(data,p,m=NULL,s=NULL){
    data.take=data$Y[(data$M==m)&(data$S==s),]
    data.take=matrix(data.take,ncol=dim(data$Y)[2])
    value=NULL
    if(length(data.take)>0){
        count=0
        for(g in 1:dim(data.take)[1]){
            count=count+prod(choose(data$design[1:m,1],data.take[g,data.take[g,]>=0]))
        }
    }
    list(prob=count*p^s*(1-p)^(sum(data$design[1:m,1])-s),
         count=count,
         subcount=dim(data.take)[1],
         data=data.take)
}
