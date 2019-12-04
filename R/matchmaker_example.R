#' show the path to a matchmaker example file
#'
#' @param name the name of a matchmaker example file
#'
#' @return a path to a matchmaker example file
#' @export
#' @author Zhian N. Kamvar
#'
#' @examples
#' matchmaker_example() # list all of the example files
#'
#' # read in example spelling dictionary
#' sd <- matchmaker_example("spelling-dictionary.csv")
#' read.csv(sd, stringsAsFactors = FALSE)
#'
#' # read in example coded data
#' coded_data <- matchmaker_example("coded-data.csv")
#' coded_data <- read.csv(coded_data, stringsAsFactors = FALSE)
#' str(coded_data)
#' coded_data$date <- as.Date(coded_data$date)
matchmaker_example <- function(name = NULL) {
  if (is.null(name)) {
    list.files(system.file("extdata", package = "matchmaker"))
  } else {
    system.file("extdata", name, package = "matchmaker", mustWork = TRUE)
  }
}
