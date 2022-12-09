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
