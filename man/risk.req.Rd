\name{risk.req}
\alias{risk.req}
\title{
Computes capital requirements based on risk measures
}
\description{
Determines capital requirements based on risk measures (StD, VaR, EL, ELD, ES, SDR, EVaR, DEVaR, ENT, DENT, ML) 
given initial capital and time period.
}
\usage{
risk.req(x, M = 10^6, T = 1, alpha = c(0.05), beta = 1, p = 2)
}
\arguments{
  \item{x}{
a vector of observations.
}
  \item{M}{
a numeric representing initial capital.
}
  \item{T}{
a numeric representing the period capital is required.
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
A matrix with values of required capital for each risk measure at all probabilities of interest.
}
\examples{
## computes capital requirement for a position of U$ 1,000 on SP500 for five days

data(returns)
s <- returns[, 2]
risk.req(s, 1000, 5, c(0.01, 0.05))
}

