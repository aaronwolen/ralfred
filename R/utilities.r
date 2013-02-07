#' Categorize each alfred call
#' 
#' @param x alfred object
#'

categorize <- function(x) {
  rgrep <- function(pattern, ...) grep(pattern, x = x$item, ...)
  fgrep <- function(pattern, ...) rgrep(pattern, ..., fixed = TRUE)  
  
  category <- rep("Other", nrow(x))
  category[fgrep("alfred.metadata")]                    = "Directory"
  category[rgrep("alfred\\.metadata.*app$")]            = "Application"
  category[fgrep("alfred.web")]                         = "Web search"
  category[rgrep("alfred\\.(extension|core|terminal)")] = "Command"
  category[fgrep("alfred.spelling")]                    = "Spelling"
  category[fgrep("alfred.url")]                         = "Website"
  
  levels <- names(sort(table(category), decreasing = TRUE))
  x$category <- factor(category, levels = levels)
  return(x)
}
