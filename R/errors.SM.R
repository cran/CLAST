
#' @export

errors.SM <-
function(n,a,b,p0,p1){
    data=sample.space.SM(n,a,b)
    probs.SM=function(obj,p){obj$count*p^obj$S*(1-p)^(obj$N-obj$S)}
    list(type1=sum(probs.SM(data,p0)*data$decision),
         type2=sum(probs.SM(data,p1)*(1-data$decision)))
}
