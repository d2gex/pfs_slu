source("R/vbt/config.R")
source ("R/vbt/vbt.R")
source("R/vbt/vbt_plotting.R")

run_vbf <- function (data_sample, sample_percentage) {
  vbf_obj <- VonBertalanffy$new(data_sample)
  mean_ages <- vbf_obj$get_lt_lt1_means()
  vbf_initial_params <- vbf_obj$find_vbf_initial_params (mean_ages)
  result_naive <- vbf_obj$naive(vbf_initial_params)
  result_nq <- vbf_obj$estimated()
  result_q <- vbf_obj$estimated(is_quarter = TRUE)
  
  title <-
    "Initial Von Bertalanffy curve after solving the equation"
  # generate sampling distribution
  dist_plot <- create_distribution_plot (data_sample,
                                         paste0(sample_percentage, '%'))
  lt_lt1_plot <- create_lt_lt1_plot(mean_ages)
  vbf_naive_plot <- create_vbf_plot(result_naive$data,
                                    result_naive$linf,
                                    result_naive$k,
                                    result_naive$t0,
                                    title)
  
  title <-
    "Estimated Von Bertalanffy curve after parameter estimations (no quarters)"
  vbf_nq_plot <- create_vbf_plot(result_nq$data,
                                 result_nq$linf,
                                 result_nq$k,
                                 result_nq$t0,
                                 title)
  
  title <-
    "Estimated Von Bertalanffy curve after parameter estimations (with quarters)"
  vbf_q_plot <- create_vbf_plot(result_q$data,
                                result_q$linf,
                                result_q$k,
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
  
  return(ggarrange(
    plotlist = list(first_row_grid, second_row_grid),
    ncol = 1,
    nrow = 2
  ))
}


#  Von Bertalanffy with 100% of sampling
grid_100 <- run_vbf(herring_data, '100')

#  Von Bertalanffy with 50% of sampling
sample_size <- round(0.5 * nrow(herring_data))
herring_sample <- sample_n(herring_data, sample_size)
grid_50 <- run_vbf(herring_sample, '50')

#  Von Bertalanffy with 25% of sampling
sample_size <- round(0.25 * nrow(herring_data))
herring_sample <- sample_n(herring_data, sample_size)
grid_25 <- run_vbf(herring_sample, '25')

to_path <-
  paste("outputs/vbt_herring_different_sampling.pdf")

plots_to_pdf(list(grid_100, grid_50, grid_25),
             to_path,
             paper_type,
             paper_height,
             paper_width)
