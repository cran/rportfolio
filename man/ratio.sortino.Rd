% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ratio_sortino.R
\name{ratio.sortino}
\alias{ratio.sortino}
\title{Sortino Ratio}
\usage{
ratio.sortino(R1, Rf = 0)
}
\arguments{
\item{R1}{Returns of the portfolio}

\item{Rf}{Risk Free rate of return, Default: 0}
}
\value{
Gives the Sortino ratio of the portfolio
}
\description{
Calculates the Sortino Ratio
}
\details{
The Sortino ratio is a variation of the Sharpe ratio that differentiates harmful volatility from total overall volatility by using the asset's standard deviation of negative portfolio returns, called downside deviation, instead of the total standard deviation of portfolio returns.
}
\examples{
ratio.sortino(funds$ret)
}
