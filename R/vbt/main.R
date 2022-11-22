source("R/vbt/config.R")
source ("R/vbt/vbt.R")
source("R/vbt/vbt_plotting.R")
#---------------------------------------------------------------
#  Von Bertalanffy with 100% of sampling
#---------------------------------------------------------------

herring_sample <- herring_data
sample_size = 100

vbf_100 <- VonBertalanffy$new(herring_data)
mean_ages <- vbf_100$get_lt_lt1_means()
vbf_initial_params <- vbf_100$find_vbf_initial_params (mean_ages)
result_naive <- vbf_100$naive(vbf_initial_params)
result_nq <- vbf_100$estimated()
result_q <- vbf_100$estimated(is_quarter = TRUE)

title <- "Initial Von Bertalanffy curve after solving the equation"
# generate sampling distribution
dist_plot <- create_distribution_plot (herring_sample)
lt_lt1_plot <- create_lt_lt1_plot(mean_ages)
vbf_naive_plot <- create_vbf_plot(result_naive$data,
                                  result_naive$linf,
                                  result_naive$linf,
                                  result_naive$t0,
                                  title)

title <-
  "Estimated Von Bertalanffy curve after parameter estimations (no quarters)"
vbf_nq_plot <- create_vbf_plot(result_nq$data,
                               result_nq$linf,
                               result_nq$linf,
                               result_nq$t0,
                               title)

title <-
  "Estimated Von Bertalanffy curve after parameter estimations (with quarters)"
vbf_q_plot <- create_vbf_plot(result_q$data,
                              result_q$linf,
                              result_q$linf,
                              result_q$t0,
                              title)


first_row_grid <- ggarrange(
  plotlist = list(dist_plot,
                  lt_lt1_plot,
                  vbf_naive_plot),
  ncol = 3,
  nrow = 1
)
second_row_grid <- ggarrange(
  plotlist = list(vbf_nq_plot,
                  vbf_q_plot),
  ncol = 2,
  nrow = 1
)

outer_grid_100 <-
  ggarrange(
    plotlist = list(first_row_grid, second_row_grid),
    ncol = 1,
    nrow = 2
  )

# #---------------------------------------------------------------
# #  Von Bertalanffy with 50% of sampling
# #---------------------------------------------------------------
# sample_size <- round(0.5 * nrow(herring_data))
# herring_sample <- sample_n(herring_data, sample_size)
# source ("R/vbt/vbt.R")
# source("R/vbt/vbt_plotting.R")
#
# outer_grid_50 <-
#   ggarrange(
#     plotlist = list(first_row_grid, second_row_grid),
#     ncol = 1,
#     nrow = 2
#   )


#---------------------------------------------------------------
#  Von Bertalanffy with 25% of sampling
#---------------------------------------------------------------
sample_size <- round(0.25 * nrow(herring_data))
herring_sample <- sample_n(herring_data, sample_size)
source ("R/vbt/vbt.R")
source("R/vbt/vbt_plotting.R")

outer_grid_25 <-
  ggarrange(
    plotlist = list(first_row_grid, second_row_grid),
    ncol = 1,
    nrow = 2
  )

to_path <-
  paste(OUTPUT,
        "/",
        "vbt_herring_different_sampling.pdf",
        sep = "")

plots_to_pdf(
  list(outer_grid_100, outer_grid_50, outer_grid_25),
  to_path,
  paper_type,
  paper_height,
  paper_width
)