
#' a ggplot2 wrapper plot MS identification rate check
#'
#' @param data.frame a data from with first two columns to be used, first as name, second as MS id rate
#' @param plot_type options are one or more in a c("scatter", "bar", "density", "histogram", "freqpoly", "box", "violin"), defautl as bar
#' @param group a vector of grouping information, the same order as the rows
#' @param cutoff a cutoff indicator for threshold plot, indicating usually accepted bar
#' @param maintitle plot title
#' @param xlabel label for x
#' @param vertical if too many lines, vertical layout holds more information.
#' @param ...
#'
#' @return  list of selected plot
#' @export
#'
#' @examples
#'
#' # see example for input data format
#' my_test <- data.frame("samplename" = paste0("sample_", 1:20),
#'                       "msms_id" = c(abs(rnorm(10))*10+20, abs(rnorm(10))*10+30),
#'                       "treat_group" = c(paste0("group_", rep("A",10)), paste0("group_", rep("B",10)))
#' )
#'
#' tt <-  MQ_QC_plot(my_test, plot_type = c("scatter","bar","density", "histogram", "freqpoly", "box", "violin"), cutoff = 35, group = "treat_group", maintitle = "MSMS ID Rate", xlabel = "MS/MS ID %")
#' # now tt has all the required plot
#' tt$scatter
#' tt$density
#'
#'
#'
#'
MQ_QC_plot<- function(data.frame,
                      plot_type = c("scatter", "bar", "density", "histogram", "freqpoly", "box", "violin") ,
                      group = NULL, # needs to be column name
                      cutoff = 20,
                      maintitle = "",
                      xlabel = "",
                      vertical =  FALSE,
                      ...
){

  # in case some column names are not valid names (containing special symbol, like space, % etc)
  library(ggplot2)

  names(data.frame)[1] <- "names"
  names(data.frame)[2] <- "value"

  if(length(plot_type) == 0){ # if no plot type defined, plot barplot
    plot_type = "bar"
  }

  plot_out <- list()

  if(is.null(group)){ # for none grouped
    # this will ensure the default plotting order is the same as the order in the data.frame input
    data.frame$names <- factor(data.frame$names, levels = data.frame$names)

    #for violinplot and boxplot, a fake group is needed
    group <- "All"
    data.frame$All = "All"

    #plotting

    if("scatter" %in% plot_type){
      scatter_plot <- ggplot(data.frame) +
        geom_point(aes_string(x = "names", y = "value")) +
        geom_hline(yintercept = cutoff, linetype="dashed", color = "blue", size=1) +
        labs(title = maintitle, x = "", y = xlabel) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(angle = 90, hjust = 1),
              panel.grid = element_blank()) +
        coord_flip()



      plot_out <- c(plot_out, list("scatter_plot" = scatter_plot))
    }



    if("bar" %in% plot_type){
      bar_plot <- ggplot(data.frame) +
        geom_hline(yintercept = cutoff, linetype="dashed", color = "blue", size=1) +
        geom_col(aes_string(x = "names", y = "value"))+

        labs(title = maintitle, x = "",y = xlabel) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(angle = 90, hjust = 1),
              panel.grid = element_blank())+
        coord_flip()

      plot_out <- c(plot_out, list("bar_plot" = bar_plot))

    }


    if("freqpoly" %in% plot_type){
      # distritibution
      freqpoly_plot <- ggplot(data.frame) +
        annotate("rect", xmin=-Inf, xmax= cutoff, ymin=0, ymax=Inf, alpha=0.1, fill="red") +
        annotate("rect", xmin=cutoff, xmax=Inf, ymin=0, ymax=Inf, alpha=0.5, fill="lightblue") +
        geom_freqpoly(aes_string("value") )+
        geom_vline(xintercept = cutoff, linetype="dashed", color = "blue", size=1)+
        labs(title = maintitle,  x = "",y = xlabel) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(angle = 90, hjust = 1),
              panel.grid = element_blank())
      if(vertical){
        freqpoly_plot <- freqpoly_plot + coord_flip()
      }

      plot_out <- c(plot_out, list("freqpoly_plot" = freqpoly_plot))
    }

    if("histogram" %in% plot_type){
      histogram_plot <- ggplot(data.frame) +
        annotate("rect", xmin=-Inf, xmax= cutoff, ymin=0, ymax=Inf, alpha=0.1, fill="red") +
        annotate("rect", xmin=cutoff, xmax=Inf, ymin=0, ymax=Inf, alpha=0.5, fill="lightblue") +
        geom_histogram(aes_string("value"),position = "identity",alpha = 0.5)+
        geom_vline(xintercept = cutoff, linetype="dashed", color = "blue", size=1)+
        labs(title = maintitle,  x = "",y = xlabel) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(angle = 90, hjust = 1),
              panel.grid = element_blank())
      if(vertical){
        histogram_plot <- histogram_plot + coord_flip()
      }
      plot_out <- c(plot_out, list("histogram_plot" = histogram_plot))
    }



    if("density" %in% plot_type){
      density_plot <-ggplot(data.frame) +
        annotate("rect", xmin=-Inf, xmax= cutoff, ymin=0, ymax=Inf, alpha=0.1, fill="red") +
        annotate("rect", xmin=cutoff, xmax=Inf, ymin=0, ymax=Inf, alpha=0.5, fill="lightblue") +
        geom_density(aes_string("value"),position = "identity",alpha = 0.5) +
        geom_vline(xintercept = cutoff, linetype="dashed", color = "blue", size=1)+
        labs(title = maintitle,  x = "",y = xlabel) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(angle = 90, hjust = 1),
              panel.grid = element_blank())
      if(vertical){
        density_plot <- density_plot + coord_flip()
      }
      plot_out <- c(plot_out, list("density_plot" = density_plot))
    }


  }else{
    # if grouping provided, order by group
    data.frame$names <- factor(data.frame$names, levels = data.frame$names[order(data.frame[group])])

    if("scatter" %in% plot_type){
      scatter_plot <- ggplot(data.frame) +
        geom_point(aes_string(x = "names", y = "value", colour = group)) +
        geom_hline(yintercept = cutoff, linetype="dashed", color = "blue", size=1) +
        labs(title = maintitle, x = "", y = xlabel) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(angle = 90, hjust = 1),
              panel.grid = element_blank())+
        coord_flip()



      plot_out <- c(plot_out, list("scatter_plot" = scatter_plot))
    }
    if("bar" %in% plot_type){
      bar_plot <- ggplot(data.frame) +
        geom_hline(yintercept = cutoff, linetype="dashed", color = "blue", size=1) +
        geom_col(aes_string(x = "names", y = "value", fill = group))+
        labs(title = maintitle, x = "",y = xlabel) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(angle = 90, hjust = 1),
              panel.grid = element_blank())+
        coord_flip()


      plot_out <- c(plot_out, list("bar_plot" = bar_plot))

    }
    if("freqpoly" %in% plot_type){
      # distritibution
      freqpoly_plot <- ggplot(data.frame) +
        annotate("rect", xmin=-Inf, xmax= cutoff, ymin=0, ymax=Inf, alpha=0.1, fill="red") +
        annotate("rect", xmin=cutoff, xmax=Inf, ymin=0, ymax=Inf, alpha=0.5, fill="lightblue") +
        geom_freqpoly(aes_string("value",colour = group) )+
        geom_vline(xintercept = cutoff, linetype="dashed", color = "blue", size=1)+
        labs(title = maintitle,  x = "",y = xlabel) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(angle = 90, hjust = 1),
              panel.grid = element_blank())
      if(vertical){
        freqpoly_plot <- freqpoly_plot + coord_flip()
      }

      plot_out <- c(plot_out, list("freqpoly_plot" = freqpoly_plot))
    }

    if("histogram" %in% plot_type){
      histogram_plot <- ggplot(data.frame) +
        annotate("rect", xmin=-Inf, xmax= cutoff, ymin=0, ymax=Inf, alpha=0.1, fill="red") +
        annotate("rect", xmin=cutoff, xmax=Inf, ymin=0, ymax=Inf, alpha=0.5, fill="lightblue") +
        geom_histogram(aes_string("value", colour = group, fill = group),position = "identity",alpha = 0.5)+
        geom_vline(xintercept = cutoff, linetype="dashed", color = "blue", size=1)+
        labs(title = maintitle,  x = "",y = xlabel) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(angle = 90, hjust = 1),
              panel.grid = element_blank())
      if(vertical){
        histogram_plot <- histogram_plot + coord_flip()
      }
      plot_out <- c(plot_out, list("histogram_plot" = histogram_plot))
    }



    if("density" %in% plot_type){
      density_plot <-ggplot(data.frame) +
        annotate("rect", xmin=-Inf, xmax= cutoff, ymin=0, ymax=Inf, alpha=0.1, fill="red") +
        annotate("rect", xmin=cutoff, xmax=Inf, ymin=0, ymax=Inf, alpha=0.5, fill="lightblue") +
        geom_density(aes_string("value", colour = group, fill = group),position = "identity",alpha = 0.5) +
        geom_vline(xintercept = cutoff, linetype="dashed", color = "blue", size=1)+
        labs(title = maintitle,  x = "",y = xlabel) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(angle = 90, hjust = 1),
              panel.grid = element_blank())
      if(vertical){
        density_plot <- density_plot + coord_flip()
      }
      plot_out <- c(plot_out, list("density_plot" = density_plot))
    }

  }

  # the following 2 plots have to use grouping information for plotting, even with only 1 group
  if("violin" %in% plot_type){
    violin_plot <- ggplot(data.frame) +
      geom_violin(aes_string(x =group,  y = "value", colour = group, fill = group))+
      geom_jitter(aes_string(x =group,  y = "value",colour = group, fill = group),shape=21)  +
      geom_hline(yintercept = cutoff, linetype="dashed", color = "blue", size=1) +
      labs(title = maintitle,  x = "",y = xlabel) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 90, hjust = 1),
            panel.grid = element_blank())
    if(vertical){
      violin_plot <- violin_plot + coord_flip()
    }
    plot_out <- c(plot_out, list("violin_plot" = violin_plot))

  }


  if("box" %in% plot_type){
    box_plot <- ggplot(data.frame) +
      geom_boxplot(aes_string(x =group,  y = "value", colour = group, fill = group))+
      geom_jitter(aes_string(x =group,  y = "value",colour = group, fill = group),shape=21)  +
      geom_hline(yintercept = cutoff, linetype="dashed", color = "blue", size=1) +
      labs(title = maintitle,  x = "",y = xlabel) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 90, hjust = 1),
            panel.grid = element_blank())
    if(vertical){
      box_plot <- box_plot + coord_flip()
    }
    plot_out <- c(plot_out, list("box_plot" = box_plot))

  }

  return(plot_out)
}


# this function is borrowed from: http://michaeljw.com/blog/post/subchunkify/
# subchunkify <- function(g, fig_height=7, fig_width=5) { # g is a ggplot object
#   g_deparsed <- paste0(deparse(
#     function() {g}
#   ), collapse = '')
#
#   sub_chunk <- paste0("
#   `","``{r sub_chunk_", floor(runif(1) * 10000), ", fig.height=",
#    fig_height, ", fig.width=", fig_width, ", echo=FALSE}",
#   "\n(",
#     g_deparsed
#     , ")()",
#   "\n`","``
#   ")
#
#   cat(knitr::knit(text = knitr::knit_expand(text = sub_chunk), quiet = TRUE))
# }
