source("R/vbt/config.R")
#---------------------------------------------------------------
#  Von Bertalanffy with 100% of sampling
#---------------------------------------------------------------

herring_sample <- herring_data
source ("R/vbt/vbt.R")

# --> Plot lt vs lt+1 slope
lt_lt1_graph <-
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

vbf_naive_graph <-
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

vbf_nq_graph <-
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

vbf_q_graph <-
  plot_vbf (
    vbf_q,
    "Estimated Von Bertalanffy curve after parameter estimations (with quarters)",
    "Age (years)",
    "Length (cms)",
    k_q,
    linf_q,
    t0_q,
  )


outer_grid <-
  ggarrange(
    plotlist = list(lt_lt1_graph,
                    vbf_naive_graph,
                    vbf_nq_graph,
                    vbf_q_graph),
    ncol = 2,
    nrow = 3
  )

outer_grid
