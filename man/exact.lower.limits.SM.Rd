\name{exact.lower.limits.SM}
\alias{exact.lower.limits.SM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculates all exact lower limits.
}
\description{
Calculates Buehler lower limit for all possible data sets based on a provided ranking function.
}
\usage{
exact.lower.limits.SM(obj, lims = NULL, alpha = 0.05, set = FALSE)
}
\arguments{
  \item{obj}{
list with components $count, $S, $N, typically the output of LR.stats.SM, CP.stats.SM, JT.rank.SM or ML.rank.SM. In this case, it will also have element $lims.
}
  \item{lims}{
a vector of same length as obj$count, most often an approximate lower limits. If this is missing, then it should be an element of obj$lims.
}
  \item{alpha}{
exact coverage error
}
  \item{set}{
  If true, exact limits of 1 are replaced by the largest limit less than 1. This will never be chosen by the user and is there for certain numerical investigations.
}
}

\value{
A numeric vector of same length as obj$lims, giving the exact lower limit for each possible outcome as listed in components obj$M and obj$S.
}
\references{
Lloyd, C.J. (2020) Exact confidence limits after a group sequential single arm binary trial. Statistics in Medicine, Volume 38, 2389-2399.
}
\author{
Chris J. Lloyd
}
\examples{
# Example 1 in table 1 of Lloyd (2020)
n=c(5,6,5,9)
a=c(2,4,5,12)
b=c(5,9,11,13)
# There are 364 possible outcomes from this design which are
# listed in a natural systematic order by function sample.space.
all.samples=sample.space.SM(n,a,b)
attributes(all.samples)
# Y contains the 364 possible sequential binary outcomes;
# M contains how many stages before the decision;
# S contains the total number of success that produces the decision;
# decision the final binary test result of H0 or H1.
all.CP=CP.stats.SM(all.samples,type="lower")
all.CP$lims # These limits are based on fixed sample size so are not edxact.
all.exact=exact.lower.limits.SM(all.CP)
plot(all.CP$lims,all.exact)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
