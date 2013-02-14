\name{Summary}
\alias{Summary}
\alias{Summary,catenary-method}
\alias{Summary,fittedCatenary-method}
\title{Summary method for catenary}
\description{
  Nicely formatted output

  Nicely presented summary
}
\examples{
tmp <- catenary(c1=1,c2=3,lambda=1,x0=0,x1=4)
Summary(tmp)
x <- runif(100,0,4)
y <- f(x,c1=1,c2=2,lambda=3) + rnorm(100,sd=0.1)
tmp <- fittedCatenary(x,y,R=10)
Summary(tmp)
}
\author{
  Jono Tuke, Matthew Roughan

  Jono Tuke, Matthew Roughan
}

