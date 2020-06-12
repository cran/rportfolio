#' @title Markowitz Mean-Variance Model
#' @description Calculates the optimum Portfolio weights
#' @param R1 Portfolio Returns
#' @param R2 Benchmark Returns
#' @return Returns the optimum portfolio weights and their risk and return profile.
#' @details Modern portfolio theory (MPT), or mean-variance analysis, is a mathematical framework for assembling a portfolio of assets such that the expected return is maximized for a given level of risk.
#' @examples
#' markowitz.model(funds$ret1, funds$rfr)
#' @import xts
#' @import stats
#' @rdname markowitz.model
#' @export
markowitz.model = function(R1, R2){
  cor = cor(R1, R2)[[1]]
  w1 = seq(from = 0, to = 1, by = 0.01)
  w2 = 1 - w1
  portfolio1 = mean(R1)
  portfolio2 = mean(R2)
  sd1 = sd(R1)
  sd2 = sd(R2)
  ret = portfolio1 * w1 + portfolio2 * w2
  variance = w1*sd1^2 + w2*sd2^2 + 2*w1*w2*sd1*sd2*cor
  risk = sqrt(variance)
  df = data.frame(w1 = w1, w2 = w2, returns = ret, risk = risk)
  df
}
