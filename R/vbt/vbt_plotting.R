create_distribution_plot <- function(data_sample) {
  # Plot distribution
  mean <- mean(data_sample$age)
  sd <- sd(data_sample$age)
  length_interval_100 <- paste0("Length interval: [",
                                min(data_sample$length),
                                ",",
                                max(data_sample$length),
                                "]cm")
  graph <- ggplot(data = data_sample) +
    geom_histogram(
      mapping = aes(x = age, y = after_stat(density)),
      fill = "steelblue",
      colour = "black",
      binwidth = 1
    ) +
    ggtitle(paste("Frequency of ages with", sample_size, "% of the sample")) +
    stat_function(fun = dnorm, args = list(mean = mean, sd = sd)) +
    xlab("Age (years)") +
    ylab('Density') +
    theme_bw() +
    theme(plot.title = element_text(
      size = 12,
      hjust = 0.5,
      face = "bold"
    )) +
    annotate(
      "text",
      x = -0.5,
      y = 0.3,
      label = length_interval_100,
      hjust = 0
    ) +
    annotate(
      "text",
      x = -0.5,
      y = 0.27,
      label = paste("Mean (age):", round(mean, 2)),
      hjust = 0
    ) +
    annotate(
      "text",
      x = -0.5,
      y = 0.24,
      label = paste("Sd (age):", round(sd, 4)),
      hjust = 0
    )
  
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
        size = 12,
        hjust = 0.5,
        face = "bold"
      ))  +
      stat_regline_equation(label.y = 240, aes(label = ..eq.label..)) +
      stat_regline_equation(label.y = 230, aes(label = ..rr.label..))
  )
  
}


create_vbf_plot <- function(vbf, linf, k, t0, title) {
  # --> Plot naive Von Bertlanffy result
  
  return(plot_vbf (vbf,
                   title,
                   "Age (years)",
                   "Length (cms)",
                   linf,
                   k,
                   t0))
}
