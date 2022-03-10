#' plot HCA from intensity matrix
#'
#' intensity matrix needs to be along table, with no extra columns, values are MS intensities
#' the default workflow will add 1 to all density values to make full usage of the whole table, then do log10 transformation, then scale the features, cacluate the distance and plot the dengrogam. will add a colorbar if meta grouping provided
#'
#' @param df_intensity data matrix with MS raw readings
#' @param meta_table data.frame for grouping
#' @param plus1 TRUE or FALSE, means if all values +1 for easy log transformation for intensitymatrix
#'
#' @return a HCA plot
#' @export
#'
#' @examples
plot_HCA <- function(df_intensity = NULL, meta_table = NULL, plus1 =  TRUE){

  library(dendextend)

  if(plus1){
    df_intensity <- df_intensity+1
  }

  par(mar = c(6, 2, 2, 8))
  dend <- t(log10(df_intensity)) %>%  scale %>%
    dist %>% hclust(method= "ward.D") %>% as.dendrogram

  dend %>% plot (horiz = TRUE, type = "triangle",xlab = "Height")

  if(!is.null(meta_table)){
    color_bar <- factor2colorDF(as.data.frame(meta_table))
    colored_bars(colors = color_bar, dend = dend, horiz = TRUE,cex.rowLabels = 0.8)
  }

  par(mar = c(5, 4, 4, 2)) # restore
}
