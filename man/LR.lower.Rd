\name{LR.lower}
\alias{LR.lower}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculates likelihood ratio based lower limit
}
\description{
Calculates approximate lower limit for a probability from x successes out of n binary trials based on the signed root likelihood ratio.
}
\usage{
LR.lower(x, n, a = 0.05, epsilon = 1e-18)
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
\item{epsilon}{
Tolerance supplied to uniroot.
}
}


\value{
A numeric values between 0 and 1
}

\author{
Chris J. Lloyd
}

\examples{
# Find approximate 99\% lower limit for probability based on 20 successes from 55 trials.
LR.lower(20,55,a=0.01)
# Find exact (95\%) lower limit for probability based on all failures.
LR.lower(0,5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
