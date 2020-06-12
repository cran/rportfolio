#' @title Jenson's Alpha
#' @description Calculates the Jenson's Alpha of the security
#' @param R1 Portfolio Return
#' @param R2 Benchmark Return
#' @param rf Risk Free Rate of Return, Default: 0
#' @return The Jensen's measure, or Jensen's alpha, is a risk-adjusted performance measure that represents the average return on a portfolio or investment, above or below that predicted by the capital asset pricing model (CAPM), given the portfolio's or investment's beta and the average market return.
#' @examples
#' jenson.alpha(funds$ret1, funds$rfr)
#' @import xts
#' @import zoo
#' @rdname jenson.alpha
#' @export
jenson.alpha = function(R1, R2, rf = 0){
  portfolio = returns.cal(R1)
  benchmark = returns.cal(R2)
  jenson_alpha = portfolio - (rf + beta.capm(R1, R2)*(benchmark - rf))
  jenson_alpha
}
