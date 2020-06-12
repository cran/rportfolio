#' @title Tracking Error
#' @description Calculates the Tracking Error
#' @param R1 Returns of the portfolio
#' @param R2 Returns of the benchmark
#' @details Tracking error is the divergence between the price behavior of a position or a portfolio and the price behavior of a benchmark.
#' @examples
#' tracking.error(funds$ret1, funds$rfr)
#' @return Calculates the Tracking error of the security
#' @import xts
#' @import zoo
#' @rdname tracking.error
#' @export
tracking.error = function(R1, R2){
  te = sd(risk.premium(R1, R2)) * sqrt(252)
  te
}
