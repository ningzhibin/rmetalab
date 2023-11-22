#' ADVANCDED_SCATTER_UI
#'
#' @param id
#' @param group_coloring
#' @param group_shaping
#' @param collapsed
#'
#' @return
#' @export
#'
#' @examples
#'
ADVANCDED_SCATTER_UI <- function(id,
                                 group_coloring = FALSE,
                                 group_shaping = FALSE,
                                 collapsed = FALSE){
  ns <- NS(id)
  library(colourpicker)
  fluidRow(
    column(4,
           fluidRow(

             box(
               title = "General options",
               width = 12,
               solidHeader = TRUE,
               status = "primary",
               collapsible = TRUE,
               collapsed = FALSE,

               fluidRow(
                 column(6,
                        uiOutput(ns("ui_x_index")),

                        sliderInput(ns("point_size"),
                                    "Point Size",
                                    min =0.5, max =10, step = 0.1,
                                    value = 4),
                        wellPanel(
                          checkboxInput(ns("if_color_groups"),
                                        "Coloring groups",
                                        value = group_coloring
                          ),
                          conditionalPanel(
                            condition = paste0("input['", ns("if_color_groups"), "']"),
                            uiOutput(ns("ui_point_color_index"))
                          )
                        ),
                        wellPanel(
                          checkboxInput(ns("if_shape_groups"),
                                        "Shape groups",
                                        value = group_shaping
                          ),
                          conditionalPanel(
                            condition = paste0("input['", ns("if_shape_groups"), "']"),
                            uiOutput(ns("ui_point_shape_index"))
                          )

                        ),

                 ),

                 column(6,
                        uiOutput(ns("ui_y_index")),
                        sliderInput(ns("point_transparence"),
                                    "Point transparency",
                                    min =0.1, max =1, step = 0.1,
                                    value = 0.75),


                        wellPanel(
                          checkboxInput(ns("if_label_points_on_plot"),
                                        "label points",
                                        value = FALSE
                          ),

                          conditionalPanel(
                            condition = paste0("input['", ns("if_label_points_on_plot"), "']"),
                            uiOutput(ns("ui_labeling_index")),
                            uiOutput(ns("ui_label_color_index"))
                          )

                        ),


                 )
               ),
             ),

             box(
               title = "More customizations",
               width = 12,
               solidHeader = TRUE,
               status = "primary",
               collapsible = TRUE,
               collapsed = TRUE,
               column(6,
                      textInput(ns("xy_plot_maintitle"),
                                "Main title",
                                value = NULL),
                      checkboxInput(ns("plot_interactive"),
                                    "Plot interactilvely",
                                    FALSE
                      ),
               ),

               column(6,
                      textInput(ns("xy_plot_xlabel"),
                                "X label",
                                value = NULL),
                      textInput(ns("xy_plot_ylabel"),
                                "Y label",
                                value = NULL)
               ))
           ),

           fluidRow(

             column(6 ),
             column(6,
                    actionButton(ns("button_apply_xy_selection"),
                                 icon = icon("paper-plane"),
                                 label = "Plot Scatterplot",
                                 style="float:right; color: #fff; background-color: #337ab7; border-color: #2e6da4"
                    )

             ),
             verbatimTextOutput(ns("console"), placeholder = FALSE)
           )


    ),
    column(8, uiOutput(ns("ui_plot")))

  )

}

#' ADVANCDED_SCATTER_SERVER
#'
#' @param id
#' @param data_frame
#' @param x_index
#' @param y_index
#' @param grouping_index
#'
#' @return
#' @export
#'
#' @examples
#'
ADVANCDED_SCATTER_SERVER <- function(id, data_frame,
                                     x_index =1,
                                     y_index =2,
                                     grouping_index =  NULL){

  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    #library(ggrepel)
    #colnames(data_frame)[1:2]c <-
    column_index <- 1: ncol(data_frame)
    names(column_index) <-colnames(data_frame)

    output$ui_x_index <- renderUI({
      selectInput(ns("x_index"),
                  "Column for x:",
                  column_index,
                  selected  = x_index
      )

    })
    output$ui_y_index <- renderUI({
      selectInput(ns("y_index"),
                  "Column for y:",
                  column_index,
                  selected  = y_index
      )
    })


    output$ui_labeling_index <- renderUI({
      selectizeInput(ns("labeling_index"),
                     "Column for labeling text",
                     column_index,
                     selected =  grouping_index,
                     multiple = TRUE,
                     options = list(maxItems = 1)
      )
    })

    # more options here
    output$ui_point_shape_index <- renderUI({
      selectInput(ns("point_shape_index"),
                  "Column as point shape on plot:",
                  column_index,
                  selected  = grouping_index
      )

    })
    output$ui_point_color_index <- renderUI({
      selectInput(ns("point_color_index"),
                  "column as point color on plot:",
                  column_index,
                  selected  = grouping_index
      )

    })
    output$ui_label_color_index <- renderUI({
      selectInput(ns("label_color_index"),
                  "Column as label color on plot:",
                  column_index,
                  selected  = grouping_index
      )

    })


    observeEvent(input$button_apply_xy_selection,{
      print (input$plot_interactive)

      if(input$if_label_points_on_plot){
        label_index_set <- as.numeric(input$labeling_index)
        print(label_index_set)
        label_color_index_set <- as.numeric(input$label_color_index)
        print(label_color_index_set)
      }else{
        label_index_set <- NULL
        label_color_index_set <- NULL
      }


      if(input$if_color_groups){
        point_color_index_set <- as.numeric(input$point_color_index)
      }else{
        point_color_index_set <- NULL
      }

      if(input$if_shape_groups){
        point_shape_index_set <- as.numeric(input$point_shape_index)
      }else{
        point_shape_index_set <- NULL
      }

      withConsoleRedirect("console",
                          try(
                            this_plot <- scatterplot_ggrepel(data_frame,
                                                             x_index = as.numeric(input$x_index),# column index/location
                                                             y_index = as.numeric(input$y_index),# column index/location
                                                             label_index = label_index_set,
                                                             point_shape_index = point_shape_index_set, # column index/location
                                                             point_color_index = point_color_index_set, # column index/location will overide the defined color
                                                             label_color_index = label_color_index_set,
                                                             point_size = input$point_size,
                                                             alpha = input$point_transparence,
                                                             maintitle = input$xy_plot_maintitle,
                                                             xlabel = input$xy_plot_xlabel,
                                                             ylabel = input$xy_plot_ylabel
                            )
                          )
      )


      callModule(PLOT_IN_BOX_SERVER, "scatter_plot",
                 plot_object = this_plot,
                 name_tag = "scatter_plot",
                 plot_type = "ggplot2",
                 interactive = input$plot_interactive)

      output$ui_plot <- renderUI({
        PLOT_IN_BOX_UI(ns("scatter_plot"),
                       boxwidth = 12
        )
      })
    })
  })
}


