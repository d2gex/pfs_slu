source("R/vbt/config.R")
source ("R/vbt/vbt.R")
source("R/vbt/plot_utils.R")

generate_vbf_plot <-
  function (data_sample,
            sample_percentage,
            title_prefixes,
            tmax = NULL,
            vline_at_x = NULL) {
    vbf_obj <- VonBertalanffy$new(data_sample)
    mean_ages <- vbf_obj$get_lt_lt1_means()
    vbf_initial_params <-
      vbf_obj$find_vbf_initial_params (mean_ages)
    result_naive <- vbf_obj$naive(vbf_initial_params)
    result_nq <- vbf_obj$estimated()
    result_q <- vbf_obj$estimated(is_quarter = TRUE)
    
    
    # generate sampling distribution
    dist_plot <-
      create_distribution_plot (title_prefixes[1],
                                data_sample,
                                paste0(sample_percentage, '%'))
    title <-
      paste("Initial Von Bertalanffy curve after solving the equation")
    lt_lt1_plot <- create_lt_lt1_plot(mean_ages)
    vbf_naive_plot <- create_vbf_plot(result_naive$data,
                                      result_naive$linf,
                                      result_naive$k,
                                      result_naive$t0,
                                      title)
    
    title <-
      paste(title_prefixes[2],
            "Growth curve with no quarters and",
            sample_percentage,
            "% of sample")
    vbf_nq_plot <- create_vbf_plot(
      result_nq$data,
      result_nq$linf,
      result_nq$k,
      result_nq$t0,
      title,
      tmax = tmax,
      vline_at_x
    )
    
    title <-
      paste(title_prefixes[3],
            "Growth curve with quarters and",
            sample_percentage,
            "% of sample")
    vbf_q_plot <- create_vbf_plot(result_q$data,
                                  result_q$linf,
                                  result_q$k,
                                  result_q$t0,
                                  title,
                                  tmax = tmax,
                                  vline_at_x)
    return (list(
      dist_plot,
      lt_lt1_plot,
      vbf_naive_plot,
      vbf_nq_plot,
      vbf_q_plot
    ))
  }

generate_sample_plots <-
  function (data_sample,
            tmax = NULL,
            vline_at_x = NULL) {
    #  Von Bertalanffy with 100% of sampling
    plots_100 <-
      generate_vbf_plot(data_sample,
                        '100',
                        c('C)', 'F)', 'I)'),
                        tmax,
                        vline_at_x)
    
    #  Von Bertalanffy with 50% of sampling
    sample_size <- round(0.5 * nrow(data_sample))
    herring_sample <- sample_n(data_sample, sample_size)
    plots_50 <-
      generate_vbf_plot(herring_sample,
                        '50',
                        c('B', 'E)', 'H)'),
                        tmax,
                        vline_at_x)
    
    #  Von Bertalanffy with 25% of sampling
    sample_size <- round(0.25 * nrow(data_sample))
    herring_sample <-
      sample_n(data_sample,
               sample_size)
    plots_25 <-
      generate_vbf_plot(herring_sample,
                        '25',
                        c('A)', 'D)', 'G)'),
                        tmax,
                        vline_at_x)
    
    return (list(
      plots_100 = plots_100,
      plots_50 = plots_50,
      plots_25 = plots_25
    ))
    
  }

generate_all_sample_plots <-
  function(data_sample,
           num_tries,
           tmax = NULL,
           vline_at_x = NULL) {
    tries = num_tries
    plots <- NULL
    while (tries > 0 & is.null(plots)) {
      tries = tries - 1
      try (plots <-
             generate_sample_plots(data_sample, tmax, vline_at_x))
    }
    return (plots)
  }
