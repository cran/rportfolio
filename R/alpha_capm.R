#' @title CAPM Alpha
#' @description Calculates the portfolio alpha
#' @param R1 Portfolio return  as xts
#' @param R2 Benchmark Returns
#' @return Returns the alpha of the portfolio
#' @details Alpha is a term used in investing to describe a strategy's ability to beat the market, or it's "edge." Alpha is thus also often referred to as “excess return” or “abnormal rate of return,” which refers to the idea that markets are efficient, and so there is no way to systematically earn returns that exceed the broad market as a whole.
#' @examples
#' alpha.capm(funds$ret1, funds$rfr)
#' @import xts
#' @import zoo
#' @rdname alpha.capm
#' @export
alpha.capm = function(R1, R2){
  dat = merge(R1, R2)
  dat = as.data.frame(dat)
  dat = dat[-1,]
  colnames(dat) = c('R1', 'R2')
  m = lm(R1 ~ R2, data = dat)
  alpha = m$coefficients[[1]]
  alpha
}
