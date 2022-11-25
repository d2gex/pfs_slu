source ("R/vbt/vbt_herring.R")

plots <-
  generate_all_sample_plots(herring_data, 100, tmax = 20, vline_at_x = 10)

if (!is.null(plots)) {
  # Show all sample distributions on the first row
  first_row_grid <-
    ggarrange(
      plotlist =  list(plots$plots_25[[1]],
                       plots$plots_50[[1]], plots$plots_100[[1]]),
      ncol = 3,
      nrow = 1
    )
  # Show the quarter-vbf plots on the second row
  second_row_grid <-
    ggarrange(
      plotlist =  list(plots$plots_25[[4]],
                       plots$plots_50[[4]], plots$plots_100[[4]]),
      ncol = 3,
      nrow = 1
    )
  
  third_row_grid <-
    ggarrange(
      plotlist =  list(plots$plots_25[[5]],
                       plots$plots_50[[5]], plots$plots_100[[5]]),
      ncol = 3,
      nrow = 1
    )
  outer_grid <- ggarrange(
    plotlist = list(first_row_grid, second_row_grid, third_row_grid),
    ncol = 1,
    nrow = 3
  )
  
  to_path <-
    paste("outputs/vbt_herring_different_sampling.pdf")
  
  plots_to_pdf(list(outer_grid),
               to_path,
               paper_type,
               paper_height,
               paper_width)
}
