
#' shiny module for ID summary, ui end
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples

#' shiny module for ID summary, ui end
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
ID_SUMMARY_UI <-function(id){
  ns <- NS(id)
  fluidRow(
    br(),
    box(
      width = 4,
      solidHeader = TRUE,
      status = "primary",

      wellPanel(
        fluidRow(
          column(6,
                 uiOutput(ns("ui_select_data")),
                 checkboxInput(ns("if_vertical"),
                               "Transpose plot?",
                               value =  FALSE)
          ),
          column(6,
                 selectInput(ns("plot_type"),
                             "Select plot type",
                             c("scatter", "bar", "density", "histogram", "freqpoly", "box", "violin"),
                             selected = "scatter"
                 ),

                 textInput(ns("Threshold_line_value"),
                           "Set threshold line on the plot",
                           value = "20"
                 )
          )
        ),

        uiOutput(ns("ui_select_grouping"))
      ),

      fluidRow(
        column(6),
        column(6,
               actionButton(ns("button_plot"),
                            icon = icon("paper-plane"),
                            label = "Plot",
                            style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
               )
        )
      ),

      verbatimTextOutput(ns("console_error"),placeholder = FALSE),
      verbatimTextOutput(ns("console_out"), placeholder = FALSE)

    ),
    # here is the plot output, which is already in box
    uiOutput(ns("ui_plot_output"))

  )

}


#' shiny module for ID summary, server end
#'
#' @param input
#' @param output
#' @param session
#' @param data_table  a clean data_frame with rownames and columnames
#' @param meta_table
#'
#' @return
#' @export
#'
#' @examples
ID_SUMMARY_SERVER <- function(input, output, session, data_table, meta_table = NULL){
  # data_table is organized data.frame with first column as rawfile names, and then columns to plot
  # meta_table data is a data.frame, which should cover all rawfiles in the data_table
  # meta_table structure: first column as rawfile names, then column(s) of grouping information
  ns <- session$ns

  # pre process tables ----

  data_table <- data_table %>% column_to_rownames("Raw.file")
  meta_table <- meta_table %>% column_to_rownames("Raw.file")

  # generate ui ----
  output$ui_select_data <- renderUI({
    selectInput(ns("select_column"),
                "Select data to plot",
                colnames(data_table),
                selected = "Peptide.Sequences.Identified"
    )
  })

  if(!is.null(meta_table)){
    # generate ui if meta_table data provided
    output$ui_select_grouping <- renderUI({
      radioButtons(ns("select_group"),
                   "Select grouping",
                   colnames(meta_table)
      )
    })
  }else{
    # generate ui for highlight rows/points
    output$ui_select_grouping <- renderUI({
      selectInput(ns("select_row"),
                  "Select data to highlight",
                  rownames(data_table),
                  multiple=TRUE,
                  size = 10,
                  selectize=FALSE
      )
    })

  }

  # processing data according to the user settings----
  observe({
    if(!is.null(input$select_column)){
      data_frame_for_plot <- dplyr::select(data_table, input$select_column)
    }
    # if meta_table table provided
    if(!is.null(meta_table) && !is.null(input$select_group)){

      meta_selected <- dplyr::select(meta_table,input$select_group)
      # only if meta has all the rawfiles
      if(all(rownames(data_frame_for_plot) %in% rownames(meta_selected))){
        #
        data_frame_for_plot <- merge(data_frame_for_plot,meta_selected, by="row.names" )
        row.names(data_frame_for_plot) <- data_frame_for_plot[,1]

      }else{
        print("meta information not matching")
      }
    }


    # if highlight rows selected
    if(!is.null(input$select_row)){

      if_selected <- as.character(rownames(data_frame) %in% input$select_row)
      if_selected <- dplyr::recode(if_selected, "TRUE" = "selected", "FALSE" = "Not Selected")
      data_frame_for_plot$highlight <- if_selected
    }


    if(input$button_plot > 0){
      # put everything in isolate
      isolate({
        # if meta data provided, and  effective grouping information selected
        if(!is.null(input$select_group)){
          withConsoleRedirect("console_error",
                              try(mq_summary <-  MQ_QC_plot(data_frame_for_plot,
                                                            plot_type = input$plot_type,
                                                            group = input$select_group,
                                                            cutoff = as.numeric(input$Threshold_line_value),
                                                            maintitle = input$select_column,
                                                            vertical =  input$if_vertical,
                                                            xlabel = input$select_column)[[1]]))
          # if no meta data provided, but user select rows to highlight
        }else if(!is.null(input$select_row)){
          withConsoleRedirect("console_error",
                              try(mq_summary <-  MQ_QC_plot(data_frame_for_plot,
                                                            plot_type = input$plot_type,
                                                            group = "highlight",
                                                            cutoff = as.numeric(input$Threshold_line_value),
                                                            maintitle = input$select_column,
                                                            vertical =  input$if_vertical,
                                                            xlabel = input$select_column)[[1]]))
          # if no meta and no highlight
        }else if(!is.null(input$select_column)){
          #print(dim(data_frame_for_plot))
          withConsoleRedirect("console_error",
                              try(mq_summary <-  MQ_QC_plot(data_frame_for_plot,
                                                            plot_type = input$plot_type,
                                                            cutoff = as.numeric(input$Threshold_line_value),
                                                            maintitle = input$select_column,
                                                            vertical =  input$if_vertical,
                                                            xlabel = input$select_column)[[1]]))
        }

        # call the module to visualize in the browser
        withConsoleRedirect("console_error",
                            try(callModule(PLOT_IN_BOX_SERVER, "summary",
                                           plot_object = mq_summary,
                                           name_tag = "summary",
                                           plot_type = "ggplot2")))

        output$ui_plot_output <- renderUI({
          PLOT_IN_BOX_UI(ns("summary"), boxwidth = 8)
        })

      })
    }
  })
}
