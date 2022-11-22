source("R/vbt/config.R")
#---------------------------------------------------------------
#  Von Bertalanffy with 100% of sampling
#---------------------------------------------------------------

herring_sample <- herring_data
source ("R/vbt/vbt.R")

# Plot distribution
mean_100 <- mean(herring_sample$age)
sd_100 <- sd(herring_sample$age)
length_interval_100 <- paste0(
  "Length interval: [",
  min(herring_sample$length),
  ",",
  max(herring_sample$length),
  "]cm"
)
dis_graph_100 <- ggplot(data = herring_sample) +
  geom_histogram(
    mapping = aes(x = age, y = ..density..),
    fill = "steelblue",
    colour = "black",
    binwidth = 1
  ) +
  ggtitle("Frequency of ages with the 100% of the sample") +
  stat_function(fun = dnorm, args = list(mean = mean_100, sd = sd_100)) +
  xlab("Age (years)") +
  ylab('Density') +
  theme_bw() +
  theme(plot.title = element_text(size = 12,
                                  hjust = 0.5,
                                  face = "bold")) +
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
    label = paste("Mean:", round(mean_100, 2)),
    hjust = 0
  ) +
  annotate(
    "text",
    x = -0.5,
    y = 0.24,
    label = paste("Sd:", round(sd_100, 4)),
    hjust = 0
  )


# --> Plot lt vs lt+1 slope
lt_lt1_graph_100 <-
  ggplot(data = h_df_initial_slope, aes(x = x, y = y)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  xlab("Lt (cms)") +
  ylab("Lt +1 (cms)") +
  ggtitle("Slope generated from representing Lt vs Lt+1") +
  theme_bw() +
  theme(plot.title = element_text(size = 12,
                                  hjust = 0.5,
                                  face = "bold"))  +
  stat_regline_equation(label.y = 240, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 230, aes(label = ..rr.label..))


# --> Plot naive Von Bertlanffy result

vbf_naive_graph_100 <-
  plot_vbf (
    vbf_naive,
    "Initial Von Bertalanffy curve after solving the equation",
    "Age (years)",
    "Length (cms)",
    k_n1,
    linf_n1,
    t0_n1,
  )

# --> Plot estimated Von Bertlanffy result with no quarters

vbf_nq_graph_100 <-
  plot_vbf (
    vbf_nq,
    "Estimated Von Bertalanffy curve after parameter estimations (no quarters)",
    "Age (years)",
    "Length (cms)",
    k_nq,
    linf_nq,
    t0_nq,
  )

# --> Plot estimated Von Bertlanffy result with no quarters

vbf_q_graph_100 <-
  plot_vbf (
    vbf_q,
    "Estimated Von Bertalanffy curve after parameter estimations (with quarters)",
    "Age (years)",
    "Length (cms)",
    k_q,
    linf_q,
    t0_q,
  )


first_row_grid <- ggarrange(
  plotlist = list(dis_graph_100,
                  lt_lt1_graph_100,
                  vbf_naive_graph_100),
  ncol = 3,
  nrow = 1
)
second_row_grid <- ggarrange(
  plotlist = list(vbf_nq_graph_100,
                  vbf_q_graph_100),
  ncol = 2,
  nrow = 1
)
outer_grid_100 <-
  ggarrange(
    plotlist = list(first_row_grid, second_row_grid),
    ncol = 1,
    nrow = 2
  )

outer_grid_100

#---------------------------------------------------------------
#  Von Bertalanffy with 75% of sampling
#---------------------------------------------------------------
