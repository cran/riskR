\name{risk.port}
\alias{risk.port}
\title{
Computes optimal weights of portfolio based on risk measures
}
\description{
Computes optimal weights of portfolio strategy based on risk measures 
(EL, VAR, StD, AbD, SeD, E.StD, E.AbD, E.SeD, VaR, ES, EVaR, ENT, SD, SDR, ML) 
by minimization of the composed position risk. Weights are restricted to be non-negative and with unit sum.
}
\usage{
risk.port(x, alpha = c(0.05), beta = 1)
}
\arguments{
  \item{x}{
a matrix of observations with each column representing an asset.
}
  \item{alpha}{
a vector of probabilities for quantiles.
}
  \item{beta}{
a [0,1] risk aversion parameter for mean-deviation measures. 
}
}
\value{
An array with optimal weight for each risk measure at all quantiles of interest for every asset in the portfolio.
}
\examples{
## Computes optimal weights of a portfolio strategy.

s <- matrix(rt(400, df = 6), ncol = 4)
risk.port(s, 0.05)
}

