library(ggplot2)

create_distribution_plot <-
  function(prefix, data_sample, sample_percentage) {
    # Plot distribution
    mean <- mean(data_sample$age)
    sd <- sd(data_sample$age)
    length_interval_100 <- paste0("Length interval: [",
                                  min(data_sample$length),
                                  ",",
                                  max(data_sample$length),
                                  "]cm")
    median <- median(data_sample$length)
    graph <- ggplot(data = data_sample) +
      geom_histogram(
        mapping = aes(x = age, y = after_stat(density)),
        fill = "steelblue",
        colour = "black",
        binwidth = 1
      ) +
      ggtitle(paste(
        prefix,
        "Frequency of ages with",
        sample_percentage,
        "of the sample"
      )) +
      stat_function(fun = dnorm, args = list(mean = mean, sd = sd)) +
      xlab("Age (years)") +
      ylab('Density') +
      annotate(
        "text",
        size = 3,
        x = -0.5,
        y = 0.3,
        label = paste("Median:", round(median, 3), 'cm'),
        hjust = 0
      ) +
      annotate(
        "text",
        size = 3,
        x = -0.5,
        y = 0.27,
        label = length_interval_100,
        hjust = 0
      ) +
      annotate(
        "text",
        size = 3,
        x = -0.5,
        y = 0.24,
        label = paste("Mean (age):", round(mean, 3)),
        hjust = 0
      ) +
      annotate(
        "text",
        size = 3,
        x = -0.5,
        y = 0.21,
        label = paste("Sd (age):", round(sd, 3)),
        hjust = 0
      ) +
      theme_bw() +
      theme(plot.title = element_text(
        size = 9,
        hjust = 0.5,
        face = "bold"
      ))
    
    
    return (graph)
    
  }

create_lt_lt1_plot <- function(data_sample) {
  # --> Plot lt vs lt+1 slope
  return (
    ggplot(data = data_sample, aes(x = x, y = y)) +
      geom_point() +
      stat_smooth(method = "lm", col = "red") +
      xlab("Lt (cms)") +
      ylab("Lt +1 (cms)") +
      ggtitle("Slope generated from representing Lt vs Lt+1") +
      theme_bw() +
      theme(plot.title = element_text(
        size = 9,
        hjust = 0.5,
        face = "bold"
      ))  +
      stat_regline_equation(label.y = 240, aes(label = ..eq.label..)) +
      stat_regline_equation(label.y = 230, aes(label = ..rr.label..))
  )
  
}

plot_vbf <-
  function (data,
            title,
            x_label,
            y_label,
            linf,
            k,
            t0,
            tmax = NULL,
            vline_at_x = NULL,
            c = NULL,
            s = NULL) {
    # do we need to replot to show the curve up to linf?
    if (!is.null(tmax)) {
      t <- 1:tmax
      vbf_length <- linf * (1 - exp(-k * (t - t0)))
      data <- data.frame(t = t, y = vbf_length)
    }
    
    g <- ggplot(data = data, aes(x = t, y = vbf_length)) +
      geom_point() +
      geom_line() +
      ggtitle(title) +
      xlab(x_label) +
      ylab(y_label) +
      ylim(NA, linf) +
      theme_bw() +
      theme(plot.title = element_text(
        size = 9,
        hjust = 0.5,
        face = "bold"
      )) +
      annotate(
        "text",
        size = 3,
        x = 12,
        y = 175,
        label = paste("K:", round(k, 4)),
        hjust = 0
      ) +
      annotate(
        "text",
        size = 3,
        x = 12,
        y = 165,
        label = paste("Linf:", round(linf, 4)),
        hjust = 0
      ) +
      annotate(
        "text",
        size = 3,
        x = 12,
        y = 155,
        label = paste("t0:", round(t0, 4)),
        hjust = 0
      )
    
    if (!is.null(vline_at_x)) {
      g <- g + geom_vline(xintercept = vline_at_x,
                          color = "blue",
                          size = 0.75)
    }
    
  }



create_vbf_plot <-
  function(vbf,
           linf,
           k,
           t0,
           title,
           tmax = NULL,
           vline_at_x =  NULL) {
    # --> Plot naive Von Bertlanffy result
    
    return(plot_vbf (
      vbf,
      title,
      "Age (years)",
      "Length (cms)",
      linf,
      k,
      t0,
      tmax,
      vline_at_x
    ))
  }
