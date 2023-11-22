#' ggplot2_prettier
#'
#' @param ggplot2_object
#' @param maintitle
#' @param xlab
#' @param ylab
#' @param axis.text.angle.x
#' @param axis.text.angle.y
#' @param vertical
#'
#' @return
#' @export
#'
#' @examples
ggplot2_prettier <- function(ggplot2_object,
                             maintitle = NULL,
                             xlab = NULL,
                             ylab = NULL,
                             axis.text.angle.x = 0,
                             axis.text.angle.y = 0,
                             vertical =  FALSE){

  p <- ggplot2_object


  # title and labeling

  if(!is.null(maintitle)){
    if(maintitle != ""){
      p <- p + ggtitle(maintitle)
    }
  }

  if(!is.null(xlab)){
    if(xlab != ""){
      p <- p +xlab(xlab)
    }

  }

  if(!is.null(ylab)){
    if(ylab != ""){
      p <- p +ylab(ylab)
    }

  }


  p <- p+ theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = axis.text.angle.x, hjust = 1),
          axis.text.y = element_text(angle = axis.text.angle.y, hjust = 1),
          panel.grid = element_blank())

  if(vertical){
    p <- p+ coord_flip()
  }


  return(p)

}

