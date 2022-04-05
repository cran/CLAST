\name{sample.space}
\alias{sample.space}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Sample space enumeration (K>2)}
\description{
Creates all possible samples from a multi-stage group sequential trial with K>2. If K=2, use the function sample.space.2 instead.
}
\usage{
sample.space(n, a, b)
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
}

\value{
list with components $Y, $M, $S, $decision and $design
}
\references{
Lloyd, C.J. (2020) Exact confidence limits after a group sequential single arm binary trial. Statistics in Medicine, Volume 38, 2389-2399. \doi{10.1002/sim.8909}
}
\author{
Chris J. Lloyd
}

\examples{
n=c(5,6,5,9)
a=c(2,4,5,12)
b=c(5,9,11,13)
# There are 364 possible outcomes from this design which are
# listed in a natural systematic order by function sample.space.
all.samples=sample.space(n,a,b)
attributes(all.samples)
# Y contains the 364 possible sequential binary outcomes;
# M contains how many stages before the decision;
# S contains the total number of success that produces the decision;
# decision the final binary test result of H0 or H1.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
