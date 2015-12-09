\name{returns}
\alias{returns}
\docType{data}
\title{
Real market data for examples
}
\description{
Daily log-returns for Standard and Poor's 500 (SP500), Apple (AAPL), Bank of America (BAC), The DOW Chemical Company (DOW), Sun Edison (SUNE).
}
\usage{data("returns")}
\format{
  A data frame with 503 observations on the following 6 variables.
  \describe{
    \item{\code{Date}}{a vector of dates}
    \item{\code{SP500}}{a numeric vector}
    \item{\code{AAPL}}{a numeric vector}
    \item{\code{BAC}}{a numeric vector}
    \item{\code{DOW}}{a numeric vector}
    \item{\code{SUNE}}{a numeric vector}
  }
}

\examples{
data(returns)
head(returns)
}

