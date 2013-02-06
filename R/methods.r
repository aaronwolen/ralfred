#' Summarize alfred object
#' 
#' @param object alfred object
#' @param ... other arguments ignored (for compatibility with generic)
#' 
#' @importFrom lubridate round_date
#' @S3method summary alfred

summary.alfred <- function(object, ...) {
  pn <- function(num) prettyNum(num, big.mark = ",")
  day1 <- format(sort(object$ts)[1], "%b %d, %Y")
  avg.use <- mean(table(round_date(object$ts, "day")))
    
  cat("Since", day1, "Alfred has been used", pn(nrow(usage)), "times.\n",
           "Average", pn(avg.use), "times per day.\n")
}
