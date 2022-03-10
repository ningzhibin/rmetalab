

#________________________________________________________________________________________

#     PCA_Screeplot
#     PCA_Screeplot_2
#
#________________________________________________________________________________________

# ___Description___:
# plot PCA screeplot, which shows how much of variance of each Principal Component explains
# a good pca needs the firt 2/3 components explains most of the varaiance, otherwise, the separtaion is not good
# the pca analysis was done by prcomp
# an alternative is to to use a function of recordPlot to record the last plot as an object, which can be replotted after, where it was invoked. A list of plots can be returned by this way
# a better alternative is to use ggplot2, where all plots are objects

# ___Arguments___:
# PCA_Screeplot: data matrix as input
# PCA_Screeplot_@: the output of prcomp as input. in case you have already finished the pca analysis

#____Usage____;
# PCA_Screeplot(data_matrix)
# PCA_Screeplot_2(prcomp.out)

# ___Values___:
# plot a scree plot
#

# 1<-p1+labs(x = "Princaple Component Number",y="Percent of Variance",title = "Screeplot of Variance")
#   p1<-p1+theme_bw()+theme(plot.title = element_text(hjust = 0.5))
#
#   return(list(Scree.plot = p1))
# }

# accept pca result from prcomp
#' Title
#'
#' @param prcomp_out
#'
#' @return
#' @export
#'

PCA_Screeplot_2<-function(prcomp_out){
  suppressMessages(install.packages.auto(ggplot2))
  # get the values
  sd <- prcomp_out$sdev
  scores <- prcomp_out$x

  var <- sd^2
  var.percent <- var/sum(var) * 100

  #barplot(var.percent, xlab="Principal Component", ylab="Percent of Variance", names.arg=1:length(var.percent), las=1, ylim=c(0,max(var.percent)), col="gray", main="Percent of Variance")
  #abline(h=1/nrow(pca.output$rotation)*100, col="red")
  #p1 <- recordPlot()

  p1<-ggplot()+geom_bar(aes(x=c(1:length(var.percent)),y=var.percent), stat="identity")
  p1<-p1+geom_hline(yintercept = 1/nrow(prcomp_out$rotation)*100, colour = "red")
  p1<-p1+labs(x = "Princaple Component Number",y="Percent of Variance",title = "Screeplot of Variance")
  p1<-p1+theme_bw()+theme(plot.title = element_text(hjust = 0.5))


  return(list(Scree.plot = p1))
}
