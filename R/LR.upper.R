
#' @export

LR.upper <-
function(x,n,a=0.05,epsilon=1e-18){
    if(x==n){upper = 1}
    if(x<n){
        loglik=function(p,x,n,offset=0,epsilon=1e-18){
            lmax=x*log((x+epsilon)/(n+2*epsilon))+(n-x)*log((n-x+epsilon)/(n+2*epsilon))
            2*(lmax-x*log(p+epsilon)-(n-x)*log(1-p+epsilon))-offset
        }
        upper=stats::uniroot(loglik,c(x/n,1),x=x,n=n,offset=stats::qnorm(1-a)^2,epsilon=epsilon)$root
    }
    upper
}
