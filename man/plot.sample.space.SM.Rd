\name{plt.sample.space.SM}
\alias{plt.sample.space.SM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    Sample space for given sequential design.
}
\description{
    Plots decision function in S-M space with boundary vectors "a" and "b". If p0 and p1 are
    provided then the type 1 and type 2 error are displayed in the main graphic title
}
\usage{
plt.sample.space.SM(n, a, b, p0 = NULL, p1 = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
Design vector of sample sizes
}
  \item{a}{
Design vector of lower boundaries
}
  \item{b}{
Design vector of upper boundaries
}
\item{p0}{
Lower benchnmark for success probability
}
  \item{p1}{
Upper benchnmark for success probability
}
}
\value{
NULL
}
\references{
Lloyd, C.J. (2020) Exact confidence limits after a group sequential single arm binary trial. Statistics in Medicine, Volume 38, 2389-2399. \doi{10.1002/sim.8909}
}
\author{Chris J. Lloyd}
\examples{
# Figure 1 of Lloyd (2020)
  n=c(5,6,5,9)
  a=c(2,4,5,12)
  b=c(5,9,11,13)
  plt.sample.space.SM(n,a,b)
# produces plots in Figure 1 or reference.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
