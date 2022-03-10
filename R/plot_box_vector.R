#' Boxplot from a vector along with grouping
#'
#' A wrapper function of geom_violin or geom_boxplo. Will do the data data,framn preaparation first
#' depricated name: simple_ggboxplot_vector
#'
#' @param vector a numeric vector
#' @param factor a factor vector, the same order as the vector
#' @param plot_type type of plot, default as "violin", alternative as "boxboxplot"
#' @param title a string of title name
#'
#' @return  a ggplot2 object of either boxplot or violin plot
#' @export
#'
#' @examples
#'
#' vector_test <- rnorm(200)
#' factor_test <- sample(LETTERS[1:3], 200, replace = TRUE)
#' plot_box_vector(vector_test, factor_test)
#' plot_box_vector(vector = vector_test, factor = factor_test, plot_type = "boxplot")
#'
#'
#'
#'
plot_box_vector <- function(vector = NULL, factor = 1, plot_type = "violin", title = "Distribution"){

  install.packages.auto(ggplot2)

  data_df <- data.frame(grouping = as.factor(factor), Values = vector)

  p <- ggplot(data_df,aes_string("grouping","Values",fill="grouping"))

  if (plot_type == "violin"){
    p1 <- p+geom_violin() + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))
  } else if (plot_type == "boxplot"){
    p1 <- p+geom_boxplot(alpha=0.3, width=0.9) + geom_jitter(shape=21) + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))
  }
  return(p1)

}


