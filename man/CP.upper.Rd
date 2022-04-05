\name{CP.upper}
\alias{CP.upper}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculates Clopper-Pearson upper limit
}
\description{
Calculates the exact Clopper-Pearson upper limit for a binomial probability based on x successes from n trials
}
\usage{
CP.upper(x, n, a = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
number of successes (integer between 0 and n inclusive)
}
  \item{n}{
number of binary trials
}
  \item{a}{
coverage error of limit
}
}
\value{
A numeric value between 0 and 1
}
\references{
Clopper, C. and Pearson, E. S. (1934). The use of confidence or fiducial limits illustrated in the case of the binomial. Biometrika, 26 (4): 404-413. \doi{10.2307/2331986}
}
\author{
Chris J. Lloyd
}
\examples{
# Find exact 99\% upper limit for probability based on 20 successes from 55 trials.
CP.upper(20,55,a=0.01)
# Find exact (95\%) upper limit for probability from all successes.
CP.lower(5,5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
