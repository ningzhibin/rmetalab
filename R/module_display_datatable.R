#' shiny module for datatable display, ui end
#'
#' ui has download button
#'
#' @param id
#' @param boxtitle
#' @param boxwidth
#'
#' @return
#' @export
#'
#' @examples
#'
DISPLAY_TABLE_UI <- function(id, boxtitle = NULL,boxwidth = 12){
  ns <- NS(id)
  box(
    width = boxwidth,
    status = "primary",
    title = boxtitle,
    solidHeader = TRUE,
    DT::dataTableOutput(ns('table_output')),
    DOWNLOAD_TABLE_UI(ns("download_table"),
                      label = "Download table")
  )
}


#' shiny module for datatable display, server end
#'
#' @param input
#' @param output
#' @param session
#' @param data_table
#' @param filename_tag
#' @param height
#'
#' @return
#' @export
#'
#' @examples
DISPLAY_TABLE_SERVER <- function(input, output, session, data_table, filename_tag, height = 600){
  ns <- session$ns
  #library(DT) # for table
  callModule(DOWNLOAD_TABLE_SERVER,"download_table",
             data = data_table,
             filename_tag = filename_tag)


  output$table_output <- DT::renderDataTable(
    data_table,
    filter = 'top',
    extensions = c('Scroller'),
    options = list(
      autoWidth = TRUE,
      pageLength = 50,
      dom = 'Brtip',
      #buttons = c('colvis'),
      scrollY = height,
      scrollX = TRUE,
      initComplete = htmlwidgets::JS(
      "function(settings, json) {",
      paste0("$(this.api().table().container()).css({'font-size': '", "70%", "'});"),
      "}")
    )
  )


}



#' shiny module for table display, without download button, ui end
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
DISPLAY_TABLE_SIMPLE_UI <- function(id){
  ns <- NS(id)
  DT::dataTableOutput(ns('table_output'))

}

#'shiny module for table display, whithout download button, server end
#'
#' @param input
#' @param output
#' @param session
#' @param data_table
#' @param height
#'
#' @return
#' @export
#'
#' @examples
DISPLAY_TABLE_SIMPLE_SERVER <- function(input, output, session, data_table, height = 600){
  ns <- session$ns
  output$table_output <- DT::renderDataTable(
    data_table,
    filter = 'top',
    extensions = c('Scroller'),
    options = list(
      autoWidth = TRUE,
      pageLength = 50,
      dom = 'Brtip',
      #buttons = c('colvis'),
      scrollY = height,
      scrollX = TRUE,
      initComplete = htmlwidgets::JS(
        "function(settings, json) {",
         paste0("$(this.api().table().container()).css({'font-size': '", "70%", "'});"),
        "}")
    )
  )

}




