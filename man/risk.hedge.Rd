\name{risk.hedge}
\alias{risk.hedge}
\title{
Computes optimal hedging ratios based on risk measures
}
\description{
Determines optimal hedging ratios based on risk measures (EL, VAR, StD, AbD, SeD, E.StD, E.AbD, E.SeD, VaR, ES, EVaR, ENT, SD, SDR, ML) 
by minimization of position risk.
}
\usage{
risk.hedge(x, y, alpha = c(0.05), beta = 1)
}
\arguments{
  \item{x}{
a vector of observations.
}
  \item{y}{
a vector of observations of the asset used for hedging.
}
  \item{alpha}{
a vector of probabilities for quantiles.
}
  \item{beta}{
a [0,1] risk aversion parameter for mean-deviation measures.
}
}
\value{
A matrix with values of optimal hedging ratios for each risk measure at all quantiles of interest.
}
\examples{
## computes optimal hedging ratios.

s <- rnorm(1000)
h <- rt(1000, df = 6)
risk.hedge(s, h, c(0.01, 0.05))
}

