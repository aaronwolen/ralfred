#' Read in your Alfred usage data
#' 
#' @param file Location of SQLite file containing usage data
#' @export

load_usage <- function(file = "~/Library/Application\ Support/Alfred/knowledge.alfdb") {

  if  (!file.exists(file)) {
    stop("Are you sure Alfred is installed?", call. = FALSE)
  }
    
  sql.con <- dbConnect("SQLite", file)
  data <- dbReadTable(sql.con, name = "knowledge")
  
  data$ts <- convert_timestamp(data$ts)
  
  class(data) <- c("alfred", class(data))
  return(data)
}