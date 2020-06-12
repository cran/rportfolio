#' @title CAPM Beta
#' @description Returns the Beta of Security using the CAPM Model
#' @param R1 Returns data of the security
#' @param R2 Returns data of the benchmark security
#' @return Value of the beta of the security
#' @details Beta is a measure of the volatility–or systematic risk–of a security or portfolio compared to the market as a whole.
#' @examples
#' beta.capm(funds$ret1, funds$rfr)
#' @rdname beta.capm
#' @export
#' @import xts
#' @import stats
beta.capm = function(R1, R2){
  dat = merge(R1, R2)
  dat = as.data.frame(dat)
  dat = dat[-1,]
  colnames(dat) = c('R1', 'R2')
  m = lm(R1 ~ R2, data = dat)
  beta = m$coefficients[[2]]
  beta
}
