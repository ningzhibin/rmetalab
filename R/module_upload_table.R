#' shiny module for table upload, ui end
#'
#' UI includes table upload and display, with box and box with 4-8 ratio
#'
#' @param id
#' @param header
#' @param rownames
#' @param boxwidth
#' @param boxtitle
#'
#' @return
#' @export
#'
#' @examples
UPLOAD_TABLE_UI <-function(id,
                           header =  TRUE,
                           rownames =  TRUE,
                           boxwidth = 12,
                           boxtitle = "Upload data file (tsv/csv)" ){
  ns <- NS(id)

  box(title = boxtitle,
      status = "primary",
      width =  boxwidth,
      solidHeader = TRUE,
      collapsible = TRUE,
      fluidRow(
        column(6,
               checkboxInput(ns('header'), 'First line is Header?', header),
               selectInput(ns('quote'), 'Quote type',
                           c(None='',
                             'Double Quote'='"',
                             'Single Quote'="'"),
                           '')
        ),
        column(6,
               checkboxInput(ns('rowname'), 'First column is rownames?', rownames),
               selectInput(ns('sep'), 'Deliminator',
                           c(Comma=',',
                             Semicolon=';',
                             Tab='\t'),
                           '\t')
        )
      ),
      fileInput(ns('file'), 'Locate your data file: ',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv'
                )
      )
      ,
      conditionalPanel(
        condition = paste0("output['", ns("data_upload_status"), "']"),
        DISPLAY_TABLE_SIMPLE_UI(ns("upload_data_display")) #using the simple databale display module in the same ui
      )


  )
}

#' shiny module for table upload, server end
#'
#' @param input
#' @param output
#' @param session
#' @param display_after
#'
#' @return
#' @export
#'
#' @examples
UPLOAD_TABLE_SERVER <- function(input, output, session, display_after = TRUE){
  ns <- session$ns
  inFile <- input$file
  data_upload <- reactive({

    if(is.null(inFile$datapath)){ # only if there the path is a string
      return(NULL) # this is alway the first line when reading in a file
    }else{
      #print(inFile)
      if(input$rowname){
        try(read.delim(inFile$datapath, header=input$header, sep=input$sep,
                       quote=input$quote, row.names = 1))
      }else{
        try(read.delim(inFile$datapath, header=input$header, sep=input$sep,
                       quote=input$quote))
      }
    }
  })

  output$data_upload_status <- reactive({
    return(!is.null(data_upload()) && display_after)
  })
  outputOptions(output, 'data_upload_status', suspendWhenHidden = FALSE)
  # do not use ns() here,
  # do not use suspendWhenHidden = TRUE, because this status is always hidden

  if(!is.null(data_upload())){

    callModule(DISPLAY_TABLE_SIMPLE_SERVER, "upload_data_display",
               data_table = data_upload(),
               height = 200)
  }


  data_upload() # return the expression, not the value


}

#' shiny module for table upload, ui end, version2
#'
#' ui2 is a slight modification of the version 1, with addition of check.names option, while removing the quote options
#' @param id
#' @param header
#' @param rownames
#' @param boxwidth
#' @param check.names
#' @param collapsed
#' @param boxtitle
#'
#' @return
#' @export
#'
#' @examples
UPLOAD_TABLE_UI2 <-function(id,
                            header =  TRUE,
                            rownames =  TRUE,
                            boxwidth = 12,
                            check.names =  TRUE,
                            collapsed =  FALSE,
                            boxtitle = "Upload data file (tsv/csv)" ){
  ns <- NS(id)

  box(title = boxtitle,
      status = "primary",
      width =  boxwidth,
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = collapsed,
      fluidRow(
        column(6,
               checkboxInput(ns('header'), 'First line as Header?', header),
               checkboxInput(ns('check_names'), 'Follow R naming rules(to remove special symbols)?', check.names)
               # selectInput(ns('quote'), 'Quote type',
               #             c(None='',
               #               'Double Quote'='"',
               #               'Single Quote'="'"),
               #             '')
        ),
        column(6,
               checkboxInput(ns('rowname'), 'First column as rownames?', rownames),
               selectInput(ns('sep'), 'Deliminator',
                           c(Comma=',',
                             Semicolon=';',
                             Tab='\t'),
                           '\t')
        )
      ),

      fileInput(ns('file'), 'Locate your data file: ',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv'
                )
      )
      ,
      conditionalPanel(
        condition = paste0("output['", ns("data_upload_status"), "']"),
        DISPLAY_TABLE_SIMPLE_UI(ns("upload_data_display")) #using the simple databale display module in the same ui
      )


  )
}

#' shiny module for table upload, server end, version 2
#'
#' @param input
#' @param output
#' @param session
#' @param display_after
#'
#' @return
#' @export
#'
#' @examples
UPLOAD_TABLE_SERVER2 <- function(input, output, session, display_after = TRUE){
  ns <- session$ns
  inFile <- input$file
  data_upload <- reactive({

    if(is.null(inFile$datapath)){ # only if there the path is a string
      return(NULL) # this is alway the first line when reading in a file
    }else{
      #print(inFile)
      if(input$rowname){
        try(read.delim(inFile$datapath, header=input$header, sep=input$sep,
                       check.names =  input$check_names, row.names = 1))
      }else{
        try(read.delim(inFile$datapath, header=input$header, sep=input$sep,
                       check.names =  input$check_names,))
      }
    }
  })

  output$data_upload_status <- reactive({
    return(!is.null(data_upload()) && display_after)
  })
  outputOptions(output, 'data_upload_status', suspendWhenHidden = FALSE)
  # do not use ns() here,
  # do not use suspendWhenHidden = TRUE, because this status is always hidden

  if(!is.null(data_upload())){

    callModule(DISPLAY_TABLE_SIMPLE_SERVER, "upload_data_display",
               data_table = data_upload(),
               height = 200)
  }


  data_upload() # return the expression, not the value


}

