#' @title Risk Premium of a Security
#' @description This function is used to calculate the risk premium of excess return over the risk free rate
#' @param R1 The returns of the security as xts
#' @param Rf The risk free rate of return as xts
#' @return Returns the risk premium of the security
#' @details A risk premium is the return in excess of the risk-free rate of return an investment is expected to yield
#' @examples
#' risk.premium(funds$ret1, funds$rfr)
#' @rdname risk.premium
#' @export
#' @import xts
#' @import zoo
risk.premium = function(R1, Rf){
  dat = merge(R1, Rf)
  dat = dat[-1,]
  dates = index(dat)
  dat = as.data.frame(dat)
  colnames(dat) = c('R1', 'Rf')
  risk_premium = dat[,1] - dat[,2]
  xts(risk_premium, order.by = dates)
}
