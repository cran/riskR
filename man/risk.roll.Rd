\name{risk.roll}
\alias{risk.roll}
\title{
Computes risk measures through rolling scheme
}
\description{
Computes risk measures (EL, VAR, StD, AbD, SeD, E.StD, E.AbD, E.SeD, VaR, ES, EVaR, ENT, SD, SDR, ML) 
from empirical data using a rolling estimation window.
}
\usage{
risk.roll(x, N = length(x) - 1, alpha = c(0.05), beta = 1)
}
\arguments{
  \item{x}{
a vector of observations.
}
  \item{N}{
an integer representing estimation window size. Very small values are not recommended.
}
  \item{alpha}{
a vector of probabilities for quantiles.
}
  \item{beta}{
a [0,1] risk aversion parameter for mean-deviation measures.
}
}

\value{
An array with values for each risk measure at all quantiles of interest for every point of the rolling scheme.
}

\examples{
## computes risk measures using one year of daily data.

s <- rt(1000, df = 6)
risk.roll(s, 250, c(0.01, 0.05))
}

