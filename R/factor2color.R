#' low level function convert a vector to corresponding colors, useful for marking heatmap grouping
#' available colorschemes from Rcolor Brewer qulity colors.
#' see RColorBrewer::brewer.pal.info for detailed options and colors
#' @param factor_input a vector of factor
#' @param RcolorBrewer_theme options from "Accent"  "Dark2"   "Paired"  "Pastel1" "Pastel2" "Set1"    "Set2"    "Set3"
#' @param Rcolorbrewer_schemes one of c("seq","qual","div")
#'
#' @return a vector of colors, the the same levels of the input vector
#' @export
#'
#' @examples
#' f <- as.factor(sample(LETTERS[1:3], 10, replace = TRUE))
#' factor2color(f, "Spectral")
#'
#'
#'


factor2color <- function(factor_input, RcolorBrewer_theme = "Spectral", Rcolorbrewer_schemes =  "div" ){

  colors <- as.factor(factor_input)
  levels(colors) <- generate_colors(nlevels(colors),
                                    Rcolorbrewer_schemes = Rcolorbrewer_schemes,
                                    RcolorBrewer_theme = RcolorBrewer_theme)
  return(as.vector(colors))

}


# changle log:
# 20220218 change to the new version, using the generate_colors function get things easy


# this is the old version

# factor2color <- function(factor_input, Rcolorbrewerscheme = "Accent"){
#
#   library("RColorBrewer")
#
#   colors <- as.factor(factor_input)
#   max_color <- brewer.pal.info[which(rownames(brewer.pal.info) == Rcolorbrewerscheme),1]
#
#   if(nlevels(colors) <= max_color){
#     levels(colors) <- brewer.pal(nlevels(colors),Rcolorbrewerscheme)
#   }else{
#     levels(colors) <- rainbow(nlevels(colors))
#   }
#
#   return(as.vector(colors))
# }
