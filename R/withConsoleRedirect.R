#' redirect R output to the browser
#'
#' @param containerId shiny container id
#' @param expr
#' @param type Default is message, could be "output", see type in capture.output, passed to sink(), ?sink for more information
#'
#' @return
#' @export
#'
#' @examples
#'
#' # NOT run:
#' # UI end:
#' # verbatimTextOutput("console_error1",placeholder = FALSE),
#' # verbatimTextOutput("console_out1", placeholder = FALSE
#' #
#' # SERVER END:
#' # observeEvent(input$Apply1, {
#' #   withConsoleRedirect("console_error1", try(log("a")))
#' # })
#' #
#' # observeEvent(input$Apply2, {
#' #   withConsoleRedirect("console_out1",cat("Result of log(10):"), type = "output") # if you want to catch up the output, set type to ouput
#' # })
#'


withConsoleRedirect <- function(containerId, expr, type = "message") {

  txt <- capture.output(results <- expr, type = type)
  if (length(txt) > 0) {
    insertUI(paste0("#", containerId), where = "beforeEnd",
             ui = paste0(txt, "\n", collapse = "")
    )
  }
  results
}
