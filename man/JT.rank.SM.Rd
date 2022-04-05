\name{JT.rank.SM}
\alias{JT.rank.SM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculates Jennison & Turnbull ranking of sample space
}
\description{
Calculates the Jennison & Turnbull (1983) ranking of each possible outcome of the sample space generated from a sequential design.
}
\usage{
JT.rank.SM(obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
a list with elements S, M and design, containing all possible values of total successes S and stopping stage M based on the given design. Typically this is the output of function sample.space or sample.space.2 and will also have elements Y and decision.
}
}
\value{
a list with element "lims" as well as all elements of argument obj. The extra element contains the JT rank of each possible the outcome.
}
\references{
Jennison C, Turnbull BW. Confidence intervals for a binomial parameter following a multistage test with application to MIL-STD 105D and medical trials. Technometrics. 1983(25), 49-58. \doi{10.1080/00401706.1983.10487819}
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
JT.rank.SM(obj=all.samples)
# Component "lims" contains the rank of each sequential binary
# outcome. The same rank is assigned to outcomes with the same
# values of (S,M).

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
