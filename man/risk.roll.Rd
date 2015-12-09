\name{risk.roll}
\alias{risk.roll}
\title{
Computes risk measures through rolling scheme
}
\description{
Computes risk measures (StD, VaR, EL, ELD, ES, SDR, EVaR, DEVaR, ENT, DENT, ML) 
from empirical data using a rolling estimation window.
}
\usage{
risk.roll(x, N = length(x) - 1, alpha = c(0.05), beta = 1, p = 2)
}
\arguments{
  \item{x}{
a vector of observations.
}
  \item{N}{
an integer representing estimation window size. Very small values are not recommended.
}
    \item{alpha}{
a vector of probabilities for significance level.
}
  \item{beta}{
a positive risk aversion parameter.
}
  \item{p}{
a positive value for the power of deviation terms.
}
}

\value{
An array with values for each risk measure at all probabilities of interest for every point of the rolling scheme.
}

\examples{
## computes risk measures for SP500 using one year of daily data.

data(returns)
s <- returns[, 2]
risk.roll(s, 250, c(0.01, 0.05))
}

