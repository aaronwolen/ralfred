#' Convert timestamp to POSIXct format
#' 
#' @param x Vector of raw timestamps
#' @importFrom lubridate seconds ymd_hms with_tz
convert_timestamp <- function(x) {
  with_tz(
    seconds(x) + ymd_hms("2000-01-01 00:00:00"), 
    format(Sys.time(), "%Z"))
}

#' Adjust UTC times relative to current time zone
#' 
#' @param x POSIXct dates to be converted
#' @param hours a positive or negative integer indicating how many hours the
#'   times should be adjusted
#' @export
#' @importFrom lubridate hours
adjust_times <- function(x, hours) {
  stopifnot(is(x, "POSIXct"))
  x + hours(hours)
}