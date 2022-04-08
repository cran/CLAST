\name{prob.SM}
\alias{prob.SM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Probability of sufficient statistics (S,M).
}
\description{
Calculates probability distribution of sufficient statistics (S,M) from sample space object.
}
\usage{
prob.SM(data, p, m = NULL, s = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
  list with components $Y, $S, $M, $design, $count. Thsi will typically be the output of sample.space or sample.space.2.

}
  \item{p}{
value of binary probability
}
  \item{m}{
number of stages at end of sequential trial
}
  \item{s}{
number of successes at end of sequential trial
}
}

\value{
list with component $prob, $count, $subcount and matrix $data giving the $subcount different binary sequential outcomes that lead to $prob.
}

\author{
Chris J. Lloyd
}
\references{
Lloyd, C.J. (2020) Exact confidence limits after a group sequential single arm binary trial. Statistics in Medicine, Volume 38, 2389-2399. \doi{10.1002/sim.8909}
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
prob.SM(all.samples,p=.5,m=3,s=11)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
