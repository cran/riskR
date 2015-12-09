\name{risk.hedge}
\alias{risk.hedge}
\title{
Computes optimal hedging ratios based on risk measures
}
\description{
Determines optimal hedging ratios based on risk measures (StD, VaR, EL, ELD, ES, SDR, EVaR, DEVaR, ENT, DENT, ML) 
by minimization of position risk.
}
\usage{
risk.hedge(x, y, alpha = c(0.05), beta = 1, p = 2)
}
\arguments{
  \item{x}{
a vector of observations.
}
  \item{y}{
a vector of observations of the asset used for hedging.
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
A matrix with values of optimal hedging ratios for each risk measure at all probabilities of interest.
}
\examples{
## computes optimal hedging ratios between AAPL and SP500.

data(returns)
s <- returns[, 3]
h <- returns[, 2]
risk.hedge(s, h, c(0.01, 0.05))
}

