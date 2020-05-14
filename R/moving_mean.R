#' Moving Averages
#'
#' Calculates moving averages or maximum moving average. For optimal speed, use
#' \code{integer = TRUE} if \code{x} is an integer vector and
#' \code{integer = FALSE} otherwise.
#'
#'
#' @param x Integer or numeric vector.
#' @param window Integer value specifying window length.
#' @param integer Logical value for whether \code{x} is an integer vector.
#' @param max Logical value for whether to return maximum moving average (as
#' opposed to vector of moving averages).
#'
#'
#' @return
#' Numeric value or vector depending on \code{max}.
#'
#'
#' @examples
#' # 5-unit moving average for integer vector of length 10
#' x <- rpois(10, lambda = 3)
#' moving_mean(x, 5)
#'
#'
#' @export
moving_mean <- function(x, window, integer = FALSE, max = FALSE) {

  # Call C++ function depending on 'integer' and 'max'
  if (integer) {
    if (! max) {
      return(.Call(`_stocks_moving_mean_i`, x, window))
    }
    return(.Call(`_stocks_moving_mean_i_max`, x, window))
  }
  if (! max) {
    return(.Call(`_stocks_moving_mean_n`, x, window))
  }
  return(.Call(`_stocks_moving_mean_n_max`, x, window))

}
