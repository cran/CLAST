\name{inference}
\alias{inference}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Exact limits from outcome}
\description{
Calculates exact upper and lower confidence limits from the outcome of a multi-stage group sequential trial.
}
\usage{
inference(n, a, b, y, alpha, type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
Design vector of planned sample sizes
}
  \item{a}{
Design vector of lower futility boundaries
}
  \item{b}{
Design vector of upper superiority boundaries
}
  \item{y}{
Actual outcome of experiment
}
  \item{alpha}{
exact coverage error
}
\item{type}{
character, either "LR", "CP", "JT" or "ML"
}
}

\value{
list with components
\item{lower}{exact lower limit, type dependent}
\item{upper}{exact lower limit, type dependent}
\item{est}{ML estimate}
\item{type}{selected ordering function "LR", "CP", "JT" or "ML"}
}
\references{
Lloyd, C.J. (2020) Exact confidence limits after a group sequential single arm binary trial. Statistics in Medicine, Volume 38, 2389-2399.
}
\author{Chris J. Lloyd}

\examples{
# Example from table 1 of Lloyd (2020)
  n=c(5,6,5,9)
  a=c(2,4,5,12)
  b=c(5,9,11,13)
  y=c(4,2,5) # This is the actual outcome. The trial stopped for
#              superiority at stage m=3 with s=11 successes.
  inference(n,a,b,y,type="CP")
# Results happen to be identical for type="LR" but different
# for the alternative ordering functions "JT" and "ML"
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
