#' internal method for displaying pca result using gplotly
#'
#' @param prcomp_out output from prcomp
#' @param grouping grouping information to label with groups, format is required as data.frame, with first column as sample name, second column as group
#'
#' @return a plot_ly plot
#'
#' @export
#'
#' @examples
#' test_data <- generate_test_data()
#' PCA_result <- prcomp(t(test_data$matrix))
#' PCA_plot_3d_interactive_3(prcomp_out = PCA_result, grouping = test_data$meta)
#'

PCA_plot_3d_interactive_3<-function(prcomp_out, grouping){
  suppressMessages(install.packages.auto(plotly))

  loading <- as.data.frame(prcomp_out$x)
  # reorder the meta (grouping information)
  grouping <- grouping[match(rownames(loading), grouping[,1]),]

  loading$Sample.Name <- rownames(loading)
  loading$Groups <- grouping[,2]

  #loading_merge <- as.data.frame(merge(grouping, loading, by.y=0, by.x = "Sample.Name"))
  #row.names(loading_merge)<-loading_merge$Sample.Name

  p1 <- plot_ly(loading, x = ~PC1, y = ~PC2, z = ~PC3, color = grouping[,2], colors = c('#BF382A', '#0C4B8E', "#1ABC9C")) %>%
    add_markers() %>%
    #add_text(loading_merge, x = ~PC1, y = ~PC2, z = ~PC3,text = ~Sample.Name) %>%
    add_text(text = grouping[,1]) %>%
    layout(scene = list(xaxis = list(title = 'PC1'),
                        yaxis = list(title = 'PC2'),
                        zaxis = list(title = 'PC3')))

  return(p1)
}
