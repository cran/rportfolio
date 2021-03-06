% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jenson_alpha.R
\name{jenson.alpha}
\alias{jenson.alpha}
\title{Jenson's Alpha}
\usage{
jenson.alpha(R1, R2, rf = 0)
}
\arguments{
\item{R1}{Portfolio Return}

\item{R2}{Benchmark Return}

\item{rf}{Risk Free Rate of Return, Default: 0}
}
\value{
The Jensen's measure, or Jensen's alpha, is a risk-adjusted performance measure that represents the average return on a portfolio or investment, above or below that predicted by the capital asset pricing model (CAPM), given the portfolio's or investment's beta and the average market return.
}
\description{
Calculates the Jenson's Alpha of the security
}
\examples{
jenson.alpha(funds$ret1, funds$rfr)
}
