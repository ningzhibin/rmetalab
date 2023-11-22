
#' a ggplot2 wrapper plot or describe a column from a DF, grouping is supported
#'
#' It is generalization of MQ_QC_plot, for shiny in doc,  only return 1 plot at a time, instead of all plots for rmd
#' This function aims to describe one variable (X), using types of plot
#' This function is designed to be a low level function
#'
#' @param DF a data.frame with at least two columns, first as names, second as data for plotting, others (if any) are optional could be used as grouping information
#' @param plot_type options are one in a c("point", "bar", "density", "histogram", "freqpoly", "box", "violin"), default as bar plot, plot as it is
#' @param group  character, one of the column name for grouping in the DF
#' @param cutoff a maker line, default is 0
#' @param maintitle plot title
#' @param xlabel label for x
#' @param label label for x
#' @param vertical if too many lines, vertical layout holds more information.
#' @param reorder Logical, if need to reorder
#' @param ...
#'
#' @return  list of selected plots, if no selection, plot bar
#' @export
#'
#' @examples
#'
#' # see example for input data format
#' DF <- data.frame("samplename" = paste0("sample_", sample(20,20)),
#'                       "ID_rate" = c(abs(rnorm(10))*10+20, abs(rnorm(10))*10+30),
#'                       "treat_group" = c(paste0("group_", rep("A",10)), paste0("group_", rep("B",10))))
#'
#' df_plot <-  plot_DF(DF, plot_type = "point", cutoff = 35, group = "treat_group", maintitle = "MSMS ID Rate", xlabel = "MS/MS ID %")






plot_X<- function(DF, plot_type = "bar" ,
                      group = NULL, # needs to be column name
                      cutoff = 0,
                      maintitle = "",
                      xlabel = "",
                      ylabel = "",
                      vertical =  FALSE,
                      fix_order = TRUE,
                      plotly = FALSE,
                      ...
){

  library(ggplot2)
  # in case some column names are not valid names (containing special symbol, like space, % etc)
  names(DF)[1] <- "names"
  names(DF)[2] <- "value"

  if(!(plot_type %in% c("point", "bar", "density", "histogram", "freqpoly", "box", "violin") ) ){ # if no plot type defined, plot barplot
    plot_type = "bar"
  }

  # without grouping ---------------------------------------------------------
  #
  if(is.null(group) || nchar(group) == 0){ # for none group information provided  ! importatn, use || operator

    #for violinplot and boxplot, a fake group is needed if you really want to plot, though pointless
    DF$All = "All"
    group = "All"

    if(fix_order) {# this will ensure the default plotting order is the same as the order in the data.frame input
      DF$names <- factor(DF$names, levels = DF$names)
    }


    plot_out <- switch(plot_type,
           "point" = { ggplot(DF) +
               geom_point(aes_string(x = "names", y = "value")) +
               geom_hline(yintercept = cutoff, linetype="dashed", color = "blue", size=0.5) },
           "bar" = {ggplot(DF) +
               geom_col(aes_string(x = "names", y = "value")) +
               geom_hline(yintercept = cutoff, linetype="dashed", color = "blue", size=0.5)},
           "freqpoly" = {ggplot(DF) +
               annotate("rect", xmin=-Inf, xmax= cutoff, ymin=0, ymax=Inf, alpha=0.1, fill="red") +
               annotate("rect", xmin=cutoff, xmax=Inf, ymin=0, ymax=Inf, alpha=0.5, fill="lightblue") +
               geom_freqpoly(aes_string("value") )+
               geom_vline(xintercept = cutoff, linetype="dashed", color = "blue", size=1)},

           "histogram" = {ggplot(DF) +
               annotate("rect", xmin=-Inf, xmax= cutoff, ymin=0, ymax=Inf, alpha=0.1, fill="red") +
               annotate("rect", xmin=cutoff, xmax=Inf, ymin=0, ymax=Inf, alpha=0.5, fill="lightblue") +
               geom_histogram(aes_string("value"),position = "identity",alpha = 0.5)+
               geom_vline(xintercept = cutoff, linetype="dashed", color = "blue", size=1)},
           "density" = {ggplot(DF) +
               annotate("rect", xmin=-Inf, xmax= cutoff, ymin=0, ymax=Inf, alpha=0.1, fill="red") +
               annotate("rect", xmin=cutoff, xmax=Inf, ymin=0, ymax=Inf, alpha=0.5, fill="lightblue") +
               geom_density(aes_string("value"),position = "identity",alpha = 0.5) +
               geom_vline(xintercept = cutoff, linetype="dashed", color = "blue", size=1)},
           "violin" = {ggplot(DF) +
               geom_violin(aes_string(x =group,  y = "value", colour = group, fill = group))+
               geom_jitter(aes_string(x =group,  y = "value",colour = group, fill = group),shape=21)  +
               geom_hline(yintercept = cutoff, linetype="dashed", color = "blue", size=1) },
           "box" = {ggplot(DF) +
               geom_boxplot(aes_string(x =group,  y = "value", colour = group, fill = group))+
               geom_jitter(aes_string(x =group,  y = "value",colour = group, fill = group),shape=21)  +
               geom_hline(yintercept = cutoff, linetype="dashed", color = "blue", size=1)}

           )

# with grouping ---------------------------------------------------------

  }else{
    # if grouping information is provided in the same data.frame, order by group first for better comparison

    DF$names <- factor(DF$names, levels = DF$names[order(DF[,group, drop = TRUE])])
    plot_out <- switch(plot_type,
                       "point" = {ggplot(DF) +
                           geom_point(aes_string(x = "names", y = "value", colour = group)) +
                           geom_hline(yintercept = cutoff, linetype="dashed", color = "blue", size=1) },

                       "bar" = {ggplot(DF) +
                           geom_hline(yintercept = cutoff, linetype="dashed", color = "blue", size=1) +
                           geom_col(aes_string(x = "names", y = "value", fill = group))},
                       "freqpoly" = {
                         ggplot(DF) +
                           annotate("rect", xmin=-Inf, xmax= cutoff, ymin=0, ymax=Inf, alpha=0.1, fill="red") +
                           annotate("rect", xmin=cutoff, xmax=Inf, ymin=0, ymax=Inf, alpha=0.5, fill="lightblue") +
                           geom_freqpoly(aes_string("value",colour = group) )+
                           geom_vline(xintercept = cutoff, linetype="dashed", color = "blue", size=1)},
                       "histogram" = {ggplot(DF) +
                           annotate("rect", xmin=-Inf, xmax= cutoff, ymin=0, ymax=Inf, alpha=0.1, fill="red") +
                           annotate("rect", xmin=cutoff, xmax=Inf, ymin=0, ymax=Inf, alpha=0.5, fill="lightblue") +
                           geom_histogram(aes_string("value", colour = group, fill = group),position = "identity",alpha = 0.5)+
                           geom_vline(xintercept = cutoff, linetype="dashed", color = "blue", size=1)},
                       "density" = {ggplot(DF) +
                           annotate("rect", xmin=-Inf, xmax= cutoff, ymin=0, ymax=Inf, alpha=0.1, fill="red") +
                           annotate("rect", xmin=cutoff, xmax=Inf, ymin=0, ymax=Inf, alpha=0.5, fill="lightblue") +
                           geom_density(aes_string("value", colour = group, fill = group),position = "identity",alpha = 0.5) +
                           geom_vline(xintercept = cutoff, linetype="dashed", color = "blue", size=1)},
                       "violin" = {ggplot(DF) +
                           geom_violin(aes_string(x =group,  y = "value", colour = group, fill = group))+
                           geom_jitter(aes_string(x =group,  y = "value",colour = group, fill = group),shape=21)  +
                           geom_hline(yintercept = cutoff, linetype="dashed", color = "blue", size=1) },
                       "box" = {ggplot(DF) +
                           geom_boxplot(aes_string(x =group,  y = "value", colour = group, fill = group))+
                           geom_jitter(aes_string(x =group,  y = "value",colour = group, fill = group),shape=21)  +
                           geom_hline(yintercept = cutoff, linetype="dashed", color = "blue", size=1)}

    )
  }

  # basic pretty
  plot_out <- plot_out +
    labs(title = maintitle,  x = xlabel, y = xlabel) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 90, hjust = 1),
          panel.grid = element_blank())


  if(vertical){
    plot_out <- plot_out + coord_flip()
  }

  if(plotly){
    plot_out <- plotly::ggplotly(plot_out)
  }

  return(plot_out)
}

# for testing

# DF <- data.frame("samplename" = paste0("sample_", sample(20,20)),
#                       "ID_rate" = c(abs(rnorm(10))*10+20, abs(rnorm(10))*10+30),
#                       "treat_group" = paste0("group_", sample(c("A", "B"),20, replace = TRUE)))
#
# plot_X(DF, plot_type = "bar", cutoff = 0, group = "treat_group", maintitle = "MSMS ID Rate", xlabel = "MS/MS ID %", plotly = TRUE)
# plot_X(DF, plot_type = "bar", cutoff = 0, group = "treat_group", maintitle = "MSMS ID Rate", xlabel = "MS/MS ID %", plotly = FALSE)
# plot_X(DF, plot_type = "point", plotly = FALSE)
# plot_X(DF, plot_type = "box", plotly = FALSE)
# plot_X(DF, plot_type = "violin", plotly = FALSE)
# plot_X(DF)
#
#
