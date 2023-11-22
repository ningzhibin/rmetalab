#' check if a string as a valid email address
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
isValidEmail <- function(x) {
  grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE)
}
