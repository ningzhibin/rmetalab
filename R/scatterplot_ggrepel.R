
#' scatterplot_ggrepel, a wrapper of scatter plot with ggplot2, and repel function, easy to map color shape etc
#'
#' @param data_frame # df, with at least three columns, 1 as labeling, 2 as x y mapping
#' @param x_index
#' @param y_index
#' @param label_index
#' @param label_color_index
#' @param point_shape_index
#' @param point_color_index
#' @param point_size
#' @param alpha
#' @param maintitle
#' @param xlabel
#' @param ylabel
#' @param ...
#'
#' @return a ggplot2 object
#' @export
#'
#' @examples
scatterplot_ggrepel <- function(data_frame,
                                x_index = NULL,# column index/location
                                y_index = NULL,# column index/location
                                label_index = NULL,
                                label_color_index = NULL,
                                point_shape_index = NULL, # column index/location
                                point_color_index = NULL, # column index/location will overide the defined color
                                point_size = 4,
                                alpha = 0.5,
                                maintitle = NULL,
                                xlabel = NULL,
                                ylabel = NULL,
                                ...){
  library(ggplot2)

  col_names <- colnames(data_frame)

  #if(map_shape && ncol(data_frame) >2 && !is.null(data_frame[,3])){

  if(!is.null(label_index)){
    # check if shape_map set correctly
    if(label_index > ncol(data_frame)){
      message("lable index is not set correctly")
    }else{
      # in case this column is continuous numerics
      data_frame[[label_index]] <- as.character(data_frame[[label_index]])
      label_map <- colnames(data_frame)[label_index]
    }
  }else{
    label_map <- NULL
  }

  if(!is.null(label_color_index)){
    if(label_color_index > ncol(data_frame)){
      message("color index is not set correctly")
      label_color_map <- NULL
    }else{
      # in case this column is continuous numerics
      data_frame[[label_color_index]] <- as.character(data_frame[[label_color_index]])
      label_color_map <- colnames(data_frame)[label_color_index]
    }
  }else{
    label_color_map <- NULL
  }

  if(!is.null(point_shape_index)){
    if(point_shape_index > ncol(data_frame)){
      message("shape index is not set correctly")
      point_shape_map <- NULL
    }else{
      # in case this column is continuous numerics
      data_frame[[point_shape_index]] <- as.character(data_frame[[point_shape_index]])
      point_shape_map <- colnames(data_frame)[point_shape_index]
    }
  }else{
    point_shape_map <- NULL
  }

  if(!is.null(point_color_index)){
    if(point_color_index > ncol(data_frame)){
      message("color index is not set correctly")
      point_color_map <- NULL
    }else{
      # in case this column is continuous numerics
      data_frame[[point_color_index]] <- as.character(data_frame[[point_color_index]])
      point_color_map <- colnames(data_frame)[point_color_index]
    }
  }else{
    point_color_map <- NULL
  }

  # plotting
  this_plot <-  ggplot2::ggplot(data_frame) +
      geom_point(aes_string(col_names[x_index],
                            col_names[y_index],
                            shape = point_shape_map,
                            color = point_color_map
      ),
      size = point_size,
      alpha =  alpha
    )

  # label with ggreple
  if(!is.null(label_index)){
    this_plot <- this_plot +
      ggrepel::geom_text_repel(aes_string(col_names[x_index],
                                 col_names[y_index],
                                 label = label_map,
                                 color = label_color_map
      )
      )
  }
  # title and xy
  this_plot <- ggplot2_prettier(this_plot,
                                maintitle = maintitle,
                                xlab = xlabel,
                                ylab = ylabel,
                                axis.text.angle.x = 0,
                                axis.text.angle.y = 0,
                                vertical =  FALSE
  )
  return(this_plot)
}


