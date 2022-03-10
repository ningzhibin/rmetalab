#' post hoc analysis on a vector (one dimension) with group information by pairwise.t.test
#'
#' do one way anova first, if p-value is significant, do the posthoc analysis by pairwise.t.test, and return significantly changed pairs, with p values
#'
#' @param vector a numeric vector
#' @param factor grouping information for the vector
#' @param p_anova  p value threshhold for anova
#' @param p_ttest p value threshold for ttest
#'
#' @return significantly different group pairs and corresponding p-values (FDR adjusted), return NA if no significance found
#'
#' @export
#'
#' @examples
#' vector <- c(rnorm(4),rnorm(4)+4)
#' factor <- as.factor(rep(LETTERS[1:2], each = 4))
#' Anova_1_PostHoc(vector, factor,p_anova = 0.05, p_ttest = 0.05)
#'
#' attach(airquality)
#' Month <- factor(Month, labels = month.abb[5:9])
#' result<-pairwise.t.test(Ozone, Month, p.adj = "fdr")
#' Anova_1_PostHoc(Ozone,Month,p_anova = 0.05, p_ttest = 0.05)
#'
#'
#'
Anova_1_PostHoc<-function(vector, factor, p_anova = 0.05, p_ttest = 0.05){

  vector <- as.numeric(vector)
  factor <- as.factor(factor)

  p.value_anova <- anova(lm(vector~factor))$Pr[1]
  if(p.value_anova < p_anova){

    if(nlevels(factor) >=3){
      p_PostHoc_matrix <- pairwise.t.test(vector,as.factor(factor),p.adj = "fdr")$p.value
      p_PostHoc_pairs <- find_p_location(p_PostHoc_matrix, p_threshold = p_ttest) # find_p_location is the self define functions
      return(paste0("ANOVA p=",p.value_anova,"; ",p_PostHoc_pairs))
    }else if(nlevels(factor) == 2){
      return(p.value_anova)
    }

  }else{
    return(NA)
  }
}




