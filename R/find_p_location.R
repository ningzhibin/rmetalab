#' locate the significant pairs from ANOVA test result
#' internal function
#'
#' @param p_matrix p value matrix from pairwise.t.test like functions (need to $p.value)
#' @param p_threshold pvalue threshold
#'
#' @return list of the group pairs
#' @export
#'
#' @examples
#' attach(airquality)
#' Month <- factor(Month, labels = month.abb[5:9])
#' result<-pairwise.t.test(Ozone, Month, p.adj = "fdr")
#' find_p_location(result$p.value)
#'
#'
#'
#'
find_p_location<-function(p_matrix, p_threshold=0.05){
  xx <- which(p_matrix < p_threshold, arr.ind=TRUE)
  if(length(xx) > 0){ # only perform the picking up if there is significant one
    ALL_significant_pairs <- NULL
    for(i in 1:nrow(xx)){
      #group1 name/dimention1 name:rownames(xx)[i]
      #group2 name/dimention2 name:colnames(p_matrix)[xx[i,2]]
      #p value: p_matrix[xx[i,1],xx[i,2]]
      significant_pair <- paste0(p_matrix[xx[i,1],xx[i,2]],"(",rownames(xx)[i],"~",colnames(p_matrix)[xx[i,2]],")")
      ALL_significant_pairs <- c(ALL_significant_pairs,significant_pair)
    }
      ALL_significant_pairs <- paste(ALL_significant_pairs, collapse  = "; ")
    return(ALL_significant_pairs)
  }else{
    return("No significantly changed pairs found")
  }
}


