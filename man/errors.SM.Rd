\name{errors.SM}
\alias{errors.SM}
\alias{errors}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Exact error rates of specified sequential design
}
\description{
For a given sequential design defined by vectors a, b and n, calculate the probability of a type 1 error (assuming p=p0) and type 2 error (assuming p=p1)

}
\usage{
errors.SM(n, a, b, p0, p1)
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
\item{p0}{
Lower benchnmark for success probability
}
  \item{p1}{
Upper benchnmark for success probability
}
}
\value{
list with attributes type1 and type 2
}
\author{
Chris J. Lloyd
}

\examples{
# Example 1 in table 1 of Lloyd (2020)
n=c(5,6,5,9)
a=c(2,4,5,12)
b=c(5,9,11,13)
p0=.4
p1=.75
errors.SM(n,a,b,p0,p1)
# $type1
# [1] 0.09590162
# $type2
# [1] 0.1060701
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
