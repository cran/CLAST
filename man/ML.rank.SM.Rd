\name{ML.rank.SM}
\alias{ML.rank.SM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Maximum likelihood estimator of p.
}
\description{
Calculates the ML estimator for each possible outcome of the sample space generated from a sequential design.
}
\usage{
ML.rank.SM(obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
a list with elements S, M and design, containing all possible values of total successes S and stopping stage M based on the given design. Typically this is the output of function sample.space or sample.space.2 and will also have elements Y and decision.
}
}
\value{
a list with element "lims" as well as all elements of argument obj. The extra element contains the ML estimator of p for each possible outcome.

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
ML.rank.SM(obj=all.samples)$lims
# Component "lims" contains the value of the ML estimate i.e.
# total successes S divided by total trials N. The same ML
# estimator occurs for outcomes with the same values of (S,M).
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
