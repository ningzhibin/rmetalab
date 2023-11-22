#' shiny module to plot in a box, ui end
#'
#' @param id
#' @param boxtitle
#' @param boxwidth
#'
#' @return
#' @export
#'
#' @examples

PLOT_IN_BOX_UI <- function(id, boxtitle = NULL,boxwidth = 6, plot_height = 800) {
  ns <- NS(id)
  box(
    title = boxtitle,
    width = boxwidth,
    solidHeader = TRUE,
    status = "primary",
    uiOutput(ns("plot_in_box")),
    # box for options
    box(
      width = 12,
      solidHeader = TRUE,
      checkboxInput(ns("Show_plot_options"),"Export Plot", FALSE),

      conditionalPanel(
        condition = paste0("input['", ns("Show_plot_options"), "']"),
        box(
          title = "Resize noninteractive plot",
          solidHeader = TRUE,
          status = "primary",
          width = 6,
          sliderInput(ns("plot_width"), "Plot Width", 0, 100, 100),
          sliderInput(ns("plot_height"), "Plot Height", 4, 2000, plot_height),
        ),
        box(
          title = "Export plot",
          solidHeader = TRUE,
          status = "primary",
          width = 6,
          textInput(inputId= ns("export_width"), label="width (inch)", value = 10),
          textInput(inputId= ns("export_height"), label="height (inch)", value = 8),
          textInput(inputId= ns("pointsize"), label="Text size", value = 12),
          textInput(inputId= ns("resolution"), label="PPI for PNG", value = 300),

          downloadButton(ns("download_SVG"), 'SVG'),
          downloadButton(ns("download_PDF"), 'PDF'),
          downloadButton(ns("download_PNG"), 'PNG')
        )

      )

    )

  )
}

#' shiny module to plot in a box, server end
#'
#' @param input
#' @param output
#' @param session
#' @param plot_object
#' @param plot_type
#' @param name_tag for the file name of downloading
#' @param interactive
#'
#' @return
#' @export
#'
#' @examples
PLOT_IN_BOX_SERVER <- function(input, output, session,
                               plot_object, plot_type, name_tag,
                               interactive = TRUE) {
  ns <- session$ns
  observe({
    if(plot_type == "ggplot2"){

      if(interactive){ # default is interactive plotting
        output$plot <- plotly::renderPlotly({
          plotly::ggplotly(plot_object)
        })

      }else{ # regular gglolt2 output when intarective set to FALSE
        output$plot <- renderPlot({
          plot_object
        })
      }

      # for SVG download
      output$download_SVG <- downloadHandler(
        filename <- paste0(name_tag,".svg"),
        content <- function(file) {
          svg(file,
              width = as.numeric(input$export_width),
              height = as.numeric(input$export_height),
              pointsize = as.numeric(input$pointsize),
              bg = "white")
          print(plot_object) # here it has to be "print", "plot" works in some rare cases
          dev.off()
        }
      )
      # for high resolution PDF download
      output$download_PDF <- downloadHandler(

        filename <- paste0(name_tag,".pdf"),
        content <- function(file) {
          pdf(file,
              width = as.numeric(input$export_width),
              height = as.numeric(input$export_height),
              pointsize = as.numeric(input$pointsize),
              bg = "white")

          #plot_object
          print(plot_object) # here it has to be "print", "plot" works in some rare cases
          dev.off()
        }
      )
      # for high resolution PNG download
      output$download_PNG <- downloadHandler(

        filename <- paste0(name_tag,".png"),
        content <- function(file) {
          png(file,res = as.numeric(input$resolution),
              width = as.numeric(input$export_width),
              height = as.numeric(input$export_height),
              pointsize = as.numeric(input$pointsize),
              units = "in", bg = "white")
          #plot_object
          print(plot_object) # here it has to be "print", "plot" works in some rare cases
          dev.off()
        }
      )

    }else if(plot_type == "plotly"){
      output$plot <- plotly::renderPlotly({
        plot_object
      })

    }else if(plot_type == "recordPlot"){
      output$plot <- renderPlot({
        replayPlot(plot_object)
      })

      # for SVG download
      output$download_SVG <- downloadHandler(
        filename <- paste0(name_tag,".svg"),
        content <- function(file) {
          svg(file,
              width = as.numeric(input$export_width),
              height = as.numeric(input$export_height),
              pointsize = as.numeric(input$pointsize),
              bg = "white")
          # this is important, it has to be print, not plot, or nothing, the object is already a plot
          print(plot_object)
          dev.off()
        }
      )
      # for high resolution PDF download
      output$download_PDF <- downloadHandler(

        filename <- paste0(name_tag,".pdf"),
        content <- function(file) {
          pdf(file,
              width = as.numeric(input$export_width),
              height = as.numeric(input$export_height),
              pointsize = as.numeric(input$pointsize),
              bg = "white")

          #plot_object
          print(plot_object) # here it has to be "print", "plot" works in some rare cases
          dev.off()
        }
      )
      # for high resolution PNG download
      output$download_PNG <- downloadHandler(

        filename <- paste0(name_tag,".png"),
        content <- function(file) {
          png(file,res = as.numeric(input$resolution),
              width = as.numeric(input$export_width),
              height = as.numeric(input$export_height),
              pointsize = as.numeric(input$pointsize),
              units = "in", bg = "white")
          # this is important, it has to be print, not plot, or nothing, cause, the object is already a plot
          print(plot_object)
          dev.off()
        }
      )
    }

  })


  # put the plot into UI
  output$plot_in_box <- renderUI({
    box(
      width = 12,
      solidHeader = TRUE,

      if(plot_type == "ggplot2"){
        if(interactive){
          plotly::plotlyOutput(ns("plot"),
                               width = paste0(input$plot_width, "%"),
                               height = paste0(input$plot_height, "px"))
        }else{ # only statically show ggplot2 object
          plotOutput(ns("plot"),
                     width = paste0(input$plot_width, "%"),
                     height = paste0(input$plot_height, "px"))
        }


      }else if(plot_type == "plotly"){
        plotly::plotlyOutput(ns("plot"),
                             width = paste0(input$plot_width, "%"),
                             height = paste0(input$plot_height, "px"))
      }else if(plot_type == "recordPlot"){
        plotOutput(ns("plot"),
                   width = paste0(input$plot_width, "%"),
                   height = paste0(input$plot_height, "px"))
      }else{
        print("wrong plot_type, continue using recordPlot")
        plotOutput(ns("plot"),
                   width = paste0(input$plot_width, "%"),
                   height = paste0(input$plot_height, "px"))
      }
    )
  })
}



