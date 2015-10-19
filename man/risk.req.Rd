\name{risk.req}
\alias{risk.req}
\title{
Computes capital requirements based on risk measures
}
\description{
Determines capital requirements based on risk measures (EL, VAR, StD, AbD, SeD, E.StD, E.AbD, E.SeD, VaR, ES, EVaR, ENT, SD, SDR, ML) 
given initial capital and time period.
}
\usage{
risk.req(x, M = 10^6, T = 1, alpha = c(0.05), beta = 1)
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
a vector of probabilities for quantiles.
}
  \item{beta}{
a [0,1] risk aversion parameter for mean-deviation measures.
}
}
\value{
A matrix with values of required capital for each risk measure at all quantiles of interest.
}
\examples{
## computes capital requirement for a position of U$ 1,000 for five days.

s <- rt(1000, df = 6)
risk.req(s, 1000, 5, c(0.01, 0.05))
}

