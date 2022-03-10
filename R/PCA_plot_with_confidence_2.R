#' internal method for displaying pca result, with confidence eclipse
#'
#' @param prcomp_out output from prcomp
#' @param data_meta grouping information to label with groups, format is required as data.frame, with first column as sample name, second column as group
#'
#' @return
#'
#' @export
#'

PCA_plot_with_confidence_2 <- function(prcomp_out, data_meta){
  suppressMessages(install.packages.auto(ggplot2))

  loadings <- as.data.frame(prcomp_out$x)                       # scores for first three PC's

  # reorder the meta (grouping information)
  data_meta <- data_meta[match(rownames(loadings), data_meta[,1]),]

  loadings$groups <- data_meta[,2]

  p<- ggplot(loadings, aes(PC1, PC2, color = groups)) +
    geom_point() +
    stat_ellipse(type = "norm", linetype = 2, level = 0.95)
  # confidence level =0.95 for normal distribution
  # see for more details: https://ropensci.github.io/plotly/ggplot2/stat_ellipse.html
  p<-p+labs(title = "PCA Clustering")+ theme_bw()+theme(plot.title = element_text(hjust = 0.5))

  return(p)

}
