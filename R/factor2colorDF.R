#' low level function convert a dataframe of factors to corresponding colors, useful for marking heatmap/HCA grouping
#' avaliable colorschemes from Rcolor Brewer qulity colors.
#'
#' @param DF a data.frame of factors (or elements which can be converted to factor)
#' @param RcolorBrewer_theme options from "Accent"  "Dark2"   "Paired"  "Pastel1" "Pastel2" "Set1"    "Set2"    "Set3"
#' @param Rcolorbrewer_schemes one of c("seq","qual","div")
#'
#' @return a data.frame of color codes
#' @export
#'
#' @examples
#' factor1 <- sample(LETTERS[1:3], 10, replace = TRUE)
#' factor2 <- sample(LETTERS[1:3], 10, replace = TRUE)
#' factor2colorDF(cbind(factor1, factor2))
#'
#'



factor2colorDF <- function(DF,RcolorBrewer_theme = "Spectral", Rcolorbrewer_schemes =  "div" ){

  for(i in 1:ncol(DF)){

    DF[,i] <-factor2color(DF[,i], RcolorBrewer_theme = RcolorBrewer_theme, Rcolorbrewer_schemes = Rcolorbrewer_schemes)
  }
  return(DF)

}


