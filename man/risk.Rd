\name{risk}
\alias{risk}
\title{
Computes risk measures
}
\description{
Computes risk measures (Standard Deviation (StD), Value at Risk (VaR), Expected Loss (EL), Expected Loss Deviation (ELD), Expected Shortfall (ES), 
Shortfall Deviation Risk (SDR), Expectile Value at Risk (EVaR), Deviation Expectile Value at Risk (DEVaR), Entropic (ENT), Deviation Entropic (DENT),
Maximum Loss (ML)) from empirical data.
}
\usage{
risk(x, alpha = c(0.05), beta = 1, p = 2)
}
\arguments{
  \item{x}{
a vector of observations.
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
A matrix with values for each risk measure at all probabilities of interest.
}

\examples{
# computes risk measures for the SP500 

data(returns)
s <- returns[, 2]
risk(s, c(0.01, 0.05))
}

