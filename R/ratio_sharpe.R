#' @title Sharpe Ratio
#' @description Calculates the Sharpe Ratio of the Portfolio
#' @param R1 Portfolio Returns
#' @param Rf Risk Free Rate of Return, Default: 0
#' @return Calculates the Sharpe Ratio of the portfolio
#' @details The Sharpe ratio was developed by Nobel laureate William F. Sharpe and is used to help investors understand the return of an investment compared to its risk.
#' @examples
#' \dontrun{
#' ratio.sharpe(funds$ret1)
#' @import xts
#' @import stats
#' @rdname ratio.sharpe
#' @export
ratio.sharpe = function(R1, Rf = 0){
  ret = risk.premium(R1, Rf)
  sharpe_ratio = mean(ret) / sd(R1)
  sharpe_ratio
}
