library(ggplot2)

plots_to_pdf <-
  function(plot_objects,
           filename,
           paper,
           height,
           width) {
    #' Creates a pdf with a list of plotted objects
    #' @param all_graph_plots A list of plots
    #' @param filename A string
    #' @param paper A string
    #' @param height A integer
    #' @param width A integer
    #' @return NULL
    
    pdf(filename,
        paper = paper,
        height = height,
        width = width)
    
    for (i in seq_along(plot_objects)) {
      p_object <- plot_objects[[i]]
      if (('ggplot' %in% class(p_object)) |
          ('igraph' %in% class(p_object))) {
        print(p_object)
      }
      else {
        draw(p_object)
      }
    }
    
    dev.off()
  }

try_until_done <- function(tries, func) {
  loops = tries
  success = FALSE
  while (loops > 0 & !success) {
    try (ret <- myf())
  }
}

library(ComplexHeatmap)

prepare_heatmap_data <-
  function(data,
           count_var,
           matrix_row_name,
           matrix_col_name,
           by_percentage = TRUE,
           min_row_sum = 0,
           is_bynary = FALSE) {
    #' @param by_percentage a boolean flag to indicate if metrics should be calculated
    #' in %
    
    df <- data [, c(matrix_row_name, matrix_col_name, count_var)]
    df <-
      df %>%
      pivot_wider(names_from = all_of(matrix_col_name),
                  values_from =  count_var)
    df[sapply(df, is.na)] <- 0
    
    
    # (1) Calculate % based on columns
    if (by_percentage == TRUE) {
      df <- df %>%
        mutate_at(vars(-!!matrix_row_name), funs((. / sum(.)) * 100)) %>%
        mutate_at(vars(-!!matrix_row_name), funs(round(., 2)))
    }
    # else {
    #   df <- df %>%
    #     mutate_at(vars(-!!matrix_row_name), funs(as.integer(.)))
    # }
    # Ensure we get only relevant rows
    # Ensure matrices are ready for heatmap's htclust function
    df[sapply(df, is.nan)] <- 0
    df[sapply(df, is.infinite)] <- 0
    if (min_row_sum != 0) {
      df <- df %>% filter(rowSums(.[-1]) >= min_row_sum)
    }
    heatmap_matrix <- as.matrix(df[,-1])
    if (is_bynary) {
      heatmap_matrix [heatmap_matrix > 0] <- 1
    }
    rownames(heatmap_matrix) <- df[[matrix_row_name]]
    return (heatmap_matrix)
  }



draw_interval_heatmap <- function(heat_map_matrix,
                                  main_title,
                                  legend_title,
                                  by_col_rows_order = FALSE,
                                  font_size = 10,
                                  col = NULL,
                                  title_font_size = 12,
                                  is_binary = FALSE) {
  #' @param heat_map_matrix a matrix of data in a format ready for heatmap
  #' representation
  #' @param main_title the main title of the heatmap
  #' @param legend_title the title of the legend plotted alongside the heatmap
  #'  @description Return a heatmp made by the library ComplexHeatMap for a single metric
  
  
  if (by_col_rows_order) {
    heat_map <- Heatmap(
      heat_map_matrix,
      name = legend_title,
      column_title = main_title,
      column_title_side = "top",
      column_title_gp = gpar(fontsize = title_font_size, fontface = "bold"),
      col = col,
      row_title_side = "right",
      show_column_dend = FALSE,
      show_row_dend = FALSE,
      rect_gp = gpar(col = "white", lwd = 1),
      row_order = order(rownames(heat_map_matrix)),
      column_order = 1:length(colnames(heat_map_matrix)),
      cell_fun = function(j, i, x, y, width, height, fill) {
        if (!is_binary) {
          grid.text(sprintf("%.1f", heat_map_matrix[i, j]),
                    x,
                    y,
                    gp = gpar(fontsize = font_size))
        }
      }
    )
  }
  else {
    heat_map <- Heatmap(
      heat_map_matrix,
      name = legend_title,
      column_title = main_title,
      column_title_side = "top",
      column_title_gp = gpar(fontsize = 12, fontface = "bold"),
      row_title_side = "right",
      show_column_dend = FALSE,
      show_row_dend = FALSE,
      rect_gp = gpar(col = "white", lwd = 1),
      col = col,
      cell_fun = function(j, i, x, y, width, height, fill) {
        grid.text(sprintf("%.1f", heat_map_matrix[i, j]),
                  x,
                  y,
                  gp = gpar(fontsize = font_size))
      }
    )
  }
  return (heat_map)
}
