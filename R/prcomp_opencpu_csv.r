#' pca test for opencpu development
#'
#' a wrapper for pca test, starting from a csv imput, do the pca analysis, output some txt test.
#' the coordinate of the pca plot will be returned
#'
#' @param combo_target a csv file with row as observations, while column as experiment
#' @return  a list of sample name, and x,y coordinate
#'
#' @seealso \code{\link{prcomp}}
#'
#' @export
#'
#'

prcomp_opencpu_csv<-function(filename){
	# readin table
	taxon.table <-read.csv(filename, sep = "\t")

	# process
	rownames(taxon.table) <- taxon.table[,1]
	taxon.table <- taxon.table[,-1]
	taxon.table[taxon.table==0] <- NA
	taxon.table <-log10(taxon.table)
	cluster.result <-prcomp(t(na.omit(taxon.table)), scale = TRUE)$x

	# get values, and return, task finished!
	x <- cluster.result[,1]
	y <- cluster.result[,2]
	sample.name <- rownames(cluster.result)

	write.table(cluster.result,"value_cluster.txt",sep="\t",row.names = TRUE, col.names = NA)
	write.table(x,"value_x.txt",sep="\t",row.names = TRUE,col.names = NA)
	write.table(y,"value_y.txt",sep="\t",row.names = TRUE,col.names = NA)

	png("plot.png")
	myplot<-plot(x, y)
	myplot<-myplot+text(x,y, labels=sample.name, cex= 0.7, pos=3)
	print(myplot)
	dev.off()

	return(list(sample.name, x, y))
}
