#' shiny module to download table, ui end
#'
#' @param id
#' @param label
#'
#' @return
#' @export
#'
#' @examples
DOWNLOAD_TABLE_UI <- function(id, label = "Download CSV") {
  ns <- NS(id)
  downloadButton(ns("download_table"), label)
}

#' shiny module to download table, server end
#'
#' @param input
#' @param output
#' @param session
#' @param data
#' @param filename_tag
#'
#' @return
#' @export
#'
#' @examples
DOWNLOAD_TABLE_SERVER <- function(input, output, session, data, filename_tag = NULL) {

  output$download_table <- downloadHandler(
    filename = function() {
      paste0(filename_tag,"_", Sys.time(), ".tsv")
    },
    content = function(file) {
      #write.csv(data, file)
      write.table(data,   # do not use data(),  here accepts either values of reactive expressions
                  file,
                  sep = "\t",
                  col.names = NA)
    }
  )
}
