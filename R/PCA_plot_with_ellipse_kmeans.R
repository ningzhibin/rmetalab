#' internal method for displaying pca result, with kmeans eclipse
#'
#' @param prcomp_out output from prcomp
#' @param grouping grouping information to label with groups, format is required as data.frame, with first column as sample name, second column as group
#'
#' @return
#'
#' @export
#'

PCA_plot_with_ellipse_kmeans_2 <- function(prcomp_out, grouping){
  suppressMessages(install.packages.auto(ggplot2))

  scores <- prcomp_out$x                       # scores for first three PC's
  number_of_groups <- length(levels(as.factor(grouping[,2])))
  km  <- kmeans(scores, centers=number_of_groups, nstart=5)

  #ggdata <- data.frame(scores, Cluster=km$cluster, Groups=grouping)
  ggdata <- data.frame(scores, Cluster=km$cluster)

  p<- ggplot(ggdata) +
    geom_point(aes(x=PC1, y=PC2, colour=factor(Cluster)), size=3, shape=20) +
    geom_text(aes(x=PC1, y=PC2, color=factor(Cluster),label=rownames(ggdata)))+
    stat_ellipse(aes(x=PC1,y=PC2,fill=factor(Cluster)), geom="polygon", level=0.95, alpha=0.2) +
    guides(color=guide_legend("Cluster"),fill=guide_legend("Cluster"))+
    theme_bw()+theme(plot.title = element_text(hjust = 0.5))


  p<-p+labs(title = "PCA Clustering")+ theme_bw()+theme(plot.title = element_text(hjust = 0.5))

  return(p)

}

