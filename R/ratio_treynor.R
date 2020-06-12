#' @title Treynor Ratio
#' @description Calculates the Treynor ratio of a particular portfolio
#' @param R1 Returns of the portfolio
#' @param Rf Returns of the benchmark portfolio
#' @return This function can be used to calculate the Treynor ratio of a portfolio.
#' @details The Treynor ratio, also known as the reward-to-volatility ratio, is a performance metric for determining how much excess return was generated for each unit of risk taken on by a portfolio.
#' @examples
#' ratio.treynor(funds$ret1)
#' @rdname ratio.treynor
#' @export
ratio.treynor = function(R1, Rf = 0){
  ret = risk.premium(R1, Rf)
  tr = returns.cal(ret)/ beta.capm(R1, Rf)
  return(tr)
}
