#' @title Sortino Ratio
#' @description Calculates the Sortino Ratio
#' @param R1 Returns of the portfolio
#' @param Rf Risk Free rate of return, Default: 0
#' @return Gives the Sortino ratio of the portfolio
#' @details The Sortino ratio is a variation of the Sharpe ratio that differentiates harmful volatility from total overall volatility by using the asset's standard deviation of negative portfolio returns, called downside deviation, instead of the total standard deviation of portfolio returns.
#' @examples
#' ratio.sortino(funds$ret)
#' @import xts
#' @rdname ratio.sortino
#' @export
ratio.sortino = function(R1, Rf = 0){
  ret = risk.premium(R1, Rf)
  sortino_ratio = mean(ret)/semi.deviation(R1)
  sortino_ratio
}
