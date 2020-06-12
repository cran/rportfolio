#' @title Active Premium
#' @description Calculates the active premium
#' @param R1 Returns of Portfolio as xts
#' @param R2 Risk Free Return as xts
#' @examples
#' premium.active(funds$ret1, funds$rfr)
#' @return Calculates the active premium of the portfolio
#' @import xts
#' @import zoo
#' @rdname premium.active
#' @export
premium.active = function(R1, R2){
  active_premium = returns.cal(R1) - returns.cal(R2)
  active_premium
}
