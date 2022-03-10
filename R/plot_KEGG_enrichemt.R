#' Plot classical KEGG enrichment graph using basic r command
#'
#' @param df.match data.fram imput, with 4 columns "FunctionName"    "Number_in_group" "Number_matched"  "pvalue"          "pvalue_adjusted"
#' @param title.tag A pre in the figure title
#' @param topN the number of top N of pvalue_adjusted to plot
#' @param margin_left if you have a long FunctionName, you might need to set the margin_left big enought to
#'
#' @return a plot
#' @export
#'
#' @examples
#'
#' # prepare a fake data
#' size <- sample(100:1000, 10)
#' p = exp(-abs(sample(1:20,10)))
#' data <- data.frame(FunctionName =  paste0("KEGG_",1:20 ),
#'                   Number_in_group = sample(100:1000, 10),
#'                   Number_matched = size * (sample(1:100, 10)/100),
#'                  pvalue = p,
#'                  pvalue_adjusted = p*(sample(1:100, 10)))
#' # plot
#' plot_KEGG_enrichemt(data, topN = 10)
#'
#'

#'
plot_KEGG_enrichemt <- function(df.match, title.tag = NULL, topN = 20, margin_left = 10){

  df.match[which(df.match$pvalue_adjusted == 0),5] <- NA
  df.match[which(is.na(df.match$pvalue_adjusted)),5] <- min(df.match$pvalue_adjusted, na.rm = TRUE)

  df.match<- df.match[order(df.match$pvalue_adjusted),]

  #postscript("temp")
  #dev.control('enable')

  if(nrow(df.match) > topN){
    df.match <- df.match[1:topN,]
  }

  par(mar = c(5,margin_left,8,5),mgp=c(2,0.3,0.5),tck=-.01,cex.axis = 0.8)
  m <- df.match$Number_matched
  names(m) <-df.match$FunctionName
  barplot(rev(m),cex.names = 0.8,font.axis = 3,axes=F,border = NA,las =1,
          horiz = T,xlim = c(0, 1.2*max(m)), main = paste(title.tag, " Enrichment Analysis", sep = "") )
  z<-seq(0, ceiling(max(m)), by = ceiling(max(m))/4)
  axis(side = 1,at = z,col="grey",line = -0.5)

  par(new = T)
  p_for_plot <- -log10(df.match$pvalue_adjusted)

  plot(rev(p_for_plot),1:length(p_for_plot),type="l",lwd = 2,col="red",axes=F,xlab=NA,ylab=NA,xlim = c(0, ceiling(max(p_for_plot))))
  abline(v = -log10(0.01),lwd = 1, col="red" ,lty = "dotted" )
  z<-seq(0, ceiling(max(p_for_plot)), by = ceiling(max(p_for_plot))/4)
  axis(side = 3,at = z,col="red",line = -0.5, col.axis = 'red')
  mtext(side = 3, line = 1.2,cex= 0.9, col="red",expression(-log[10](italic(p.adjusted))))

  #EA.plot <- recordPlot()
  #dev.off()
  #return(EA.plot)
}

