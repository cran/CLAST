\name{CP.stats.SM}
\alias{CP.stats.SM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    Calculates all possible Clopper-Pearson limits.
}
\description{
    Calculates all possible Clopper-Pearson limits for p from an object
    that contains all possible data sets from a group sequential trial.
}
\usage{
CP.stats.SM(obj, alpha = 0.05, type = "upper")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
a list with elements S, M and design, containing all possible values of total successes S and stopping stage M based on the given design. Typically this is the output of function sample.space or sample.space.2 and will also have elements Y and decision.
}
  \item{alpha}{
coverage error of exact limits
}
  \item{type}{
character, either "upper" or "lower"
}
}

\value{
a list with element "lims" as well as all elements of argument obj. The extra element contains the CP limit for each possible outcome.
}
\references{
Clopper, C. and Pearson, E. S. (1934). The use of confidence or fiducial limits illustrated in the case of the binomial. Biometrika, 26 (4): 404-413. \doi{10.2307/2331986}
}
\author{Chris J. LLoyd}

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
CP.stats.SM(obj=all.samples)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
