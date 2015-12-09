\name{risk.decision}
\alias{risk.decision}
\title{
Decides the best alternative based on risk measures
}
\description{
Decides the best alternative based on risk measures (StD, VaR, EL, ELD, ES, SDR, EVaR, DEVaR, ENT, DENT, ML) 
by choosing that with maximum ratio between mean and risk.
}
\usage{
risk.decision(x, alpha = c(0.05), beta = 1, p = 2)
}
\arguments{
  \item{x}{
a matrix of observations with each column representing an investment alternative.
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
A matrix with a numeric indicating which is column that represents the best investment alternative for each risk measure at all probabilities of interest.
}
\examples{
## Decides which is the best investment alternative among AAPL, BAC, DOW and SUNE.

data(returns)
s <- returns[, 3:6]
risk.decision(s, c(0.01, 0.05))
}

