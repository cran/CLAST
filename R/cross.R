
#' @export

cross <-
function(A,v){
    out=NULL
    for(i in 1:length(v)){out=rbind(out,cbind(as.matrix(A),v[i]))}
    out
}
