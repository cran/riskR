\name{risk.port2}
\alias{risk.port2}
\title{
Computes optimal weights of portfolio based on risk measures
}
\description{
Computes optimal weights of portfolio strategy based on risk measures 
(StD, VaR, EL, ELD, ES, SDR, EVaR, DEVaR, ENT, DENT, ML) 
by maximization of the ratio between composed position return and risk. Weights are restricted to be non-negative and with unit sum.
}
\usage{
risk.port2(x, alpha = c(0.05), beta = 1, p = 2)
}
\arguments{
  \item{x}{
a matrix of observations with each column representing an asset.
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
An array with optimal weight for each risk measure at all probabilities of interest for every asset in the portfolio.
}
\examples{
## Computes optimal weights of a portfolio strategy composed by AAPL, BAC, DOW and SUNE.

data(returns)
s <- returns[1:100, 3:6]
risk.port2(s, 0.05)
}