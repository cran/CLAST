
#' @export

RANK <-
function(x){
    # Produces ranking of consecutive integers from (1:unique(x))-1
    r.x=rank(x)
    index=1:length(r.x)
    index=index[order(r.x)]
    r.x=sort(r.x)
    r.x=c(0,cumsum(sign(diff(rank(r.x)))))
    r.x[order(index)]+1
}
