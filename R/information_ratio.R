#' @title Information Ratio
#' @description Calculates the information ratio of the portfolio
#' @param R1 Returns of the portfolio
#' @param R2 Returns of the benchmark portfolio
#' @details The information ratio (IR) is a measurement of portfolio returns beyond the returns of a benchmark, usually an index, compared to the volatility of those returns.
#' @examples
#' ratio.information(funds$ret1, funds$rfr)
#' @return Calculates the information ratio of the portfolio
#' @import xts
#' @impoprt zoo
#' @rdname ratio.information
#' @export
ratio.information = function(R1, R2){
  trackingerror = tracking.error(R1, R2)
  information_ratio = premium.active(R1, R2)/ trackingerror
  information_ratio
}
