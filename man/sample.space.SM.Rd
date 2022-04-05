\name{sample.space.SM}
\alias{sample.space.SM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Sample space enumeration}
\description{
Creates all possible samples from a multi-stage group sequential trial for K>=2 by calling
    sample.space or sample.space.2.
}
\usage{
sample.space.SM(n,a,b)
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
list with components
  \item{M}{number of trials for each possible trial outcome}
  \item{S}{number of responses for each possible trial outcome}
  \item{N}{number of patients for each possible trial outcome}
  \item{count}{combinatoric multiplier for probability distribution}
  \item{subcountM}{combinatoric components for probability distribution}
  \item{decision}{test decision for each possible trial outcome}
  \item{design}{list giving design vectors}
}
\references{
Lloyd, C.J. (2020) Exact confidence limits after a group sequential single arm binary trial. Statistics in Medicine, Volume 38, 2389-2399. \doi{10.1002/sim.8909}
}
\author{Chris J. Lloyd}

\examples{
# Example 1 in table 1 of Lloyd (2020)
  n=c(5,6,5,9)
  a=c(2,4,5,12)
  b=c(5,9,11,13)
  sample.space.SM(n,a,b)
# Object describing all 26 possible outcomes for (s,m).
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
