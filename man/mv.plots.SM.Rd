\name{mv.plots.SM}
\alias{mv.plots.SM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Diagnostic mean values plots.
}
\description{
Plots mean value of upper limit, lower limit and interval width for four different ranking methods. This function is basically a wrapper for mv.plot.
}
\usage{
mv.plots.SM(n, a, b, type = "interval",
B = 100, offset = TRUE, plt = c(1, 1, 1), p0 = NULL, p1 = NULL, focus = FALSE)
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
  \item{type}{
Either "upper", "lower" or "interval" (default)
}
  \item{B}{
Integer controlling fineness of plot (default=100)
}
  \item{offset}{
if TRUE then ML mean value is subtracted
}
\item{plt}{
Logical vector indicating output plots of upper, lower and interval (default=c(1,1,1))
}
\item{p0}{
Lower (null) benchmark for success probability
}
  \item{p1}{
Upper (alternative) benchmark for success probability
}
  \item{focus}{
Logical. If true, plots are restricted to p between p0 and p1. (default=FALSE)
}
}
\value{
NULL
}
\references{Lloyd, C.J. (2021) Exact confidence limits after a group sequential single arm binary trial. Statistics in Medicine, Volume 38, 2389-2399. \doi{10.1002/sim.8909}
}
\author{Chris J. Lloyd}


\examples{
# Figure 2 in Lloyd (2020)
  n=c(5,6,5,9)
  a=c(2,4,5,12)
  b=c(5,9,11,13)
  p0=.4
  p1=.75
  mv.plots.SM(n,a,b,p0=p0,p1=p1)
# Produces three panel graphic identical to Figure 2 in reference
  mv.plots.SM(n,a,b,p0=p0,p1=p1,focus=TRUE)
# Produces alternative graphic focussed on relevant values of p.
# In both cases LR (in blue) appears best. CP can perform poorly
# for values of p outside the range of interest.

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
