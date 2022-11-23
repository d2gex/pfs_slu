source("R/vbt/config.R")
source ("R/vbt/vbt.R")
source("R/vbt/vbt_plotting.R")

generate_vbf_plot <- function (data_sample, sample_percentage) {
  vbf_obj <- VonBertalanffy$new(data_sample)
  mean_ages <- vbf_obj$get_lt_lt1_means()
  vbf_initial_params <- vbf_obj$find_vbf_initial_params (mean_ages)
  result_naive <- vbf_obj$naive(vbf_initial_params)
  result_nq <- vbf_obj$estimated()
  result_q <- vbf_obj$estimated(is_quarter = TRUE)
  
  
  # generate sampling distribution
  dist_plot <- create_distribution_plot (data_sample,
                                         paste0(sample_percentage, '%'))
  title <-
    "Initial Von Bertalanffy curve after solving the equation"
  lt_lt1_plot <- create_lt_lt1_plot(mean_ages)
  vbf_naive_plot <- create_vbf_plot(result_naive$data,
                                    result_naive$linf,
                                    result_naive$k,
                                    result_naive$t0,
                                    title)
  
  title <-
    paste("Growth curve with no quarters and  ", sample_percentage, "% of sample")
  vbf_nq_plot <- create_vbf_plot(result_nq$data,
                                 result_nq$linf,
                                 result_nq$k,
                                 result_nq$t0,
                                 title)
  
  title <-
    paste("Growth curve with quarters and  ", sample_percentage, "% of sample")
  vbf_q_plot <- create_vbf_plot(result_q$data,
                                result_q$linf,
                                result_q$k,
                                result_q$t0,
                                title)
  return (list(
    dist_plot,
    lt_lt1_plot,
    vbf_naive_plot,
    vbf_nq_plot,
    vbf_q_plot
  ))
}

generate_sample_plots <- function (data_sample) {
  #  Von Bertalanffy with 100% of sampling
  plots_100 <- generate_vbf_plot(data_sample, '100')
  
  #  Von Bertalanffy with 50% of sampling
  sample_size <- round(0.5 * nrow(data_sample))
  herring_sample <- sample_n(data_sample, sample_size)
  plots_50 <- generate_vbf_plot(herring_sample, '50')
  
  #  Von Bertalanffy with 25% of sampling
  sample_size <- round(0.25 * nrow(data_sample))
  herring_sample <- sample_n(data_sample, sample_size)
  plots_25 <- generate_vbf_plot(herring_sample, '25')
  
  return (list(
    plots_100 = plots_100,
    plots_50 = plots_50,
    plots_25 = plots_25
  ))
  
}

generate_all_sample_plots <- function(num_tries) {
  tries = num_tries
  plots <- NULL
  while (tries > 0 & is.null(plots)) {
    tries = tries - 1
    try (plots <-
           generate_sample_plots(herring_data), silent = TRUE)
  }
  return (plots)
}

plots <- generate_all_sample_plots(100)

if (!is.null(plots)) {
  # Show all sample distributions on the first row
  first_row_grid <-
    ggarrange(
      plotlist =  list(plots$plots_25[[1]], plots$plots_50[[1]], plots$plots_100[[1]]),
      ncol = 3,
      nrow = 1
    )
  # Show the quarter-vbf plots on the second row
  second_row_grid <-
    ggarrange(
      plotlist =  list(plots$plots_25[[5]], plots$plots_50[[5]], plots$plots_100[[5]]),
      ncol = 3,
      nrow = 1
    )
  outer_grid <- ggarrange(
    plotlist = list(first_row_grid, second_row_grid),
    ncol = 1,
    nrow = 2
  )
  
  to_path <-
    paste("outputs/vbt_herring_different_sampling.pdf")
  
  plots_to_pdf(list(outer_grid),
               to_path,
               paper_type,
               paper_height,
               paper_width)
}
