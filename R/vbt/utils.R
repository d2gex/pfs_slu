library(ggplot2)

plot_vbf <-
  function (data,
            title,
            x_label,
            y_label,
            k,
            linf,
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
          size = 12,
          hjust = 0.5,
          face = "bold"
        )) +
        annotate(
          "text",
          x = 1,
          y = 200,
          label = paste("K:", round(k, 4))
        ) +
        annotate(
          "text",
          x = 1,
          y = 190,
          label = paste("Linf:", round(linf, 4))
        ) +
        annotate(
          "text",
          x = 1,
          y = 180,
          label = paste("t0:", round(t0, 4))
        )
    )
    
  }