% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/active_premium.R
\name{premium.active}
\alias{premium.active}
\title{Active Premium}
\usage{
premium.active(R1, R2)
}
\arguments{
\item{R1}{Returns of Portfolio as xts}

\item{R2}{Risk Free Return as xts}
}
\value{
Calculates the active premium of the portfolio
}
\description{
Calculates the active premium
}
\examples{
premium.active(funds$ret1, funds$rfr)
}
