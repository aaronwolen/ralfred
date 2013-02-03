#' Alfred usage histogram
#' 
#' @param x alfred object
#' @param binwidth Bin width to use. Defaults to 1/30 of the range of the data.
#' @S3method plot alfred

plot.alfred <- function(x, binwidth, ...) {

  if (missing(binwidth)) {
    binwidth <- diff(range(seconds(x$ts))) / 30  
  }
  
  ggplot(x, aes_string(x = "ts")) + 
    geom_polygon(aes(y = ..count..), fill = "purple", color = "grey50",
      stat = "bin", binwidth = binwidth) +
    xlab("Date") + ylab("Times")
}



#' Punchcard plot of usage by time and time
#' 
#' @param x alfred object
#' @param all.day logical, should plot span 24 hours regardless of activity?
#' @export
#' @importFrom plyr ddply summarise
#' @importFrom lubridate wday hour

punchcard <- function(x, all.day = TRUE) {

  # Tabulate calls by day/hour
  x$day <- wday(x$ts, label = TRUE, abbr = FALSE)
  x$hour <- hour(x$ts)
  call.times <- ddply(x, ~ day +  hour, summarise, count = length(ts))
  
  # Reformat hours to match github x-axis
  call.times$hour <- format(strptime(call.times$hour, format = "%H"), "%l%p")
        
  call.times$hour <- factor(
    tolower(sub("M", "", sub("^\\s", "", call.times$hour))),
    levels = paste0(c(12, 1:12, 1:11), rep(c("a", "p"), each = 12)))
  
  
  p <- ggplot(call.times) + aes(hour, day) + 
    geom_point(aes(size = count)) +
    labs(x = "", y = "")
  
  if (all.day) p <- p + scale_x_discrete(drop = FALSE)
  return(p)
}
