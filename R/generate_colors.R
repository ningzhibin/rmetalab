#'Wrapper of RColorBrewer to produce any number of colors by colorRampPalette
#'
#'For scheme "seq",  check the available options by: rownames(brewer.pal.info[brewer.pal.info$category == "seq",])
#'For scheme "qual", check by: rownames(brewer.pal.info[brewer.pal.info$category == "qual",])
#'For scheme "div", check by: rownames(brewer.pal.info[brewer.pal.info$category == "div",])
#'
#' @param ncolors nubmer of colors to generate
#' @param Rcolorbrewer_schemes one of c("seq","qual","div") div means diverging, qual means qualititive, both with high contract, while seq means sequential, from the same color tone, good for quantative
#' @param RcolorBrewer_theme color theme, check all names by brewer.pal.info or displayed by display.brewer.all()
#'
#' @return a vector of color codes
#' @export
#'
#' @examples
#' generate_colors(100)
#' col <- generate_colors(100, "seq","Spectral" )
#' pie(rep(1,100), col = col,border = FALSE,labels = "")
#'
generate_colors <- function(ncolors = 10,
                           Rcolorbrewer_schemes =  "div",
                           RcolorBrewer_theme = "Spectral" )
  {
  library("RColorBrewer")

  # get the max number of colors in the spectra
  max_color <- RColorBrewer::brewer.pal.info[which(rownames(RColorBrewer::brewer.pal.info) == RcolorBrewer_theme),1]

  # get the full series of the defined colorbrewer_colors

  my_colbrew_colors <- RColorBrewer::brewer.pal(max_color,RcolorBrewer_theme)
  # return the color generated
  colorRampPalette(my_colbrew_colors)(ncolors)
}

