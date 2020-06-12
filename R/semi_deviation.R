#' @title Semi Deviation/ Down side Deviation
#' @description Calculates the semi deviation of the xts object
#' @param R1 Returns dataset of the portfolio
#' @return Calculates the semi deviation of the xts object
#' @details Semi-deviation is a method of measuring the below-mean fluctuations in the returns on investment.
#' @examples
#'  semi.deviation(funds$ret1)
#' @rdname semi.deviation
#' @import xts
#' @export
semi.deviation = function(R1){
  dat = subset(R1, R1 < 0)
  sum(0 - dat)/length(dat)
}
