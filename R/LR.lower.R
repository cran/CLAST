
#' @export

LR.lower <-
function(x,n,a=0.05,epsilon=1e-18){
    if(x==0){lower = 0}
    if(x>0){
        loglik=function(p,x,n,offset=0,epsilon=1e-18){
            lmax=x*log((x+epsilon)/(n+2*epsilon))+(n-x)*log((n-x+epsilon)/(n+2*epsilon))
            2*(lmax-x*log(p+epsilon)-(n-x)*log(1-p+epsilon))-offset
        }
        lower=stats::uniroot(loglik,c(0,x/n),x=x,n=n,offset=stats::qnorm(1-a)^2,epsilon=epsilon)$root
    }
    lower
}
