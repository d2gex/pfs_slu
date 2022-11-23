library(ggplot2)

plot_vbf <-
  function (data,
            title,
            x_label,
            y_label,
            linf,
            k,
            t0,
            c = NULL,
            s = NULL) {
    return (
      ggplot(data = data, aes(x = t, y = vbf_length)) +
        geom_point() +
        geom_line() +
        ggtitle(title) +
        xlab(x_label) +
        ylab(y_label) +
        theme_bw() +
        theme(plot.title = element_text(
          size = 9,
          hjust = 0.5,
          face = "bold"
        )) +
        annotate(
          "text",
          size = 3,
          x = 5,
          y = 175,
          label = paste("K:", round(k, 4)),
          hjust = 0
        ) +
        annotate(
          "text",
          size = 3,
          x = 5,
          y = 165,
          label = paste("Linf:", round(linf, 4)),
          hjust = 0
        ) +
        annotate(
          "text",
          size = 3,
          x = 5,
          y = 155,
          label = paste("t0:", round(t0, 4)),
          hjust = 0
        )
    )
    
  }


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
  while (loops >0 & !success) {
    try (ret <- myf())
  }
}
