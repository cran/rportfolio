#' @title Annualized Returns
#' @description Returns the annualized returns of a data returns data
#' @param R1 Returns dataset as xts
#' @param freq The periodicity of the dataset, Default: 252
#' @param geometric Boolean to control the geometric returns and mean annualized returns, Default: TRUE
#' @return Gives annualized returns of data
#' @details An annualized total return is the geometric average amount of money earned by an investment each year over a given time period.
#' @examples
#' returns.cal(funds$ret1)
#' @rdname returns.cal
#' @import xts
#' @export
returns.cal = function(R1, freq = 252, geometric = TRUE){
  dat = R1
  if(geometric == TRUE){
    ret = prod(1 + dat) ^ (freq/length(dat)) - 1
  }else if(geometric == FALSE){
    ret = mean(dat) * freq
  }
  ret
}
