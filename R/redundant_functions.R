# This file contain functions that are redundant with fmri.pipeline and should eventually be merged


#' convert a number of hours to a days, hours, minutes, seconds format
#'
#' @importFrom lubridate day hour minute second seconds_to_period dhours
#' @keywords internal
#' @details REDUNDANT WITH fmri.pipeline
hours_to_dhms <- function(hours, frac = FALSE) {
  checkmate::assert_number(hours, lower = 0)
  dur <- lubridate::dhours(hours)
  period <- seconds_to_period(dur)

  if (isTRUE(frac)) {
    str <- sprintf("%02d:%02d:%.03f", hour(period), minute(period), second(period))
  } else {
    str <- sprintf("%02d:%02d:%02d", hour(period), minute(period), round(second(period)))
  }

  if (day(period) > 0) {
    str <- paste0(sprintf("%d-", day(period)), str)
  }

  return(str)
}
