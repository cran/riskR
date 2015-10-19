\name{risk}
\alias{risk}
\title{
Computes risk measures
}
\description{
Computes risk measures (Expected Loss (EL), Variance (VAR), Standard Deviation (StD), Absolute Deviation (AbD), 
Semi Deviation (SeD), Mean-Standard Deviation (E_StD), Mean-Absolute Deviation (E_AbD), Mean-Semi Deviation (E_SeD), 
Value at Risk (VaR), Expected Shortfall (ES), Expectile Value at Risk (EVaR), Entropic (ENT), Shortfall Deviation (SD), 
Shortfall Deviation Risk (SDR), Maximum Loss (ML)) from empirical data.
}
\usage{
risk(x, alpha = c(0.05), beta = 1)
}
\arguments{
  \item{x}{
a vector of observations.
}
  \item{alpha}{
a vector of probabilities for quantiles.
}
  \item{beta}{
a [0,1] risk aversion parameter for mean-deviation measures. 
}
}
\value{
A matrix with values for each risk measure at all quantiles of interest.
}
\author{
Marcelo Brutti Righi
}

\examples{
# computes risk measures  

s <- rt(1000, df = 6)
risk(s, c(0.01, 0.05))
}

