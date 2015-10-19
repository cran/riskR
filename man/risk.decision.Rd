\name{risk.decision}
\alias{risk.decision}
\title{
Decides the best alternative based on risk measures
}
\description{
Decides the best alternative based on risk measures (EL, VAR, StD, AbD, SeD, E.StD, E.AbD, E.SeD, VaR, ES, EVaR, ENT, SD, SDR, ML) 
by choosing that with maximum ratio between mean and risk.
}
\usage{
risk.decision(x, alpha = c(0.05), beta = 1)
}
\arguments{
  \item{x}{
a matrix of observations with each column representing an investment alternative.
}
  \item{alpha}{
a vector of probabilities for quantiles.
}
  \item{beta}{
a [0,1] risk aversion parameter for mean-deviation measures.
}
}
\value{
A matrix with a numeric indicating which is column that represents the best investment alternative for each risk measure at all quantiles of interest.
}
\examples{
## Decides which is the best investment alternative.

s <- matrix(rt(4000, df = 6), ncol = 4)
risk.decision(s, c(0.01, 0.05))
}

