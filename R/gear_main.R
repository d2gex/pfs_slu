source("R/config.R")
source ("R/catchability.R")
library(ggVennDiagram)
library(ggplot2)

sprat_data[sapply(sprat_data, is.na)] <- 0
selected_cols <-
  c (
    'haul.id',
    'HaulLat',
    'HaulLong',
    'HaulDur',
    'StatRec',
    'Depth',
    'SweepLngt',
    'Netopening',
    'Distance',
    'DoorSpread'
  )

selected_years <- c(2003, 2011)
wing_spread <- 22

by_year_hauls <-
  select_working_data(sprat_data, selected_cols, selected_years)

year_abundances_by_hauls <-
  lapply(by_year_hauls, function (year_haul_data) {
    calculate_abundance_by_haul_km2 (year_haul_data, wing_spread)
  })

year_abundances_by_ices <-
  lapply(year_abundances_by_hauls, function (year_ices_data) {
    calculate_abundance_and_num_hauls_by_ices_km2 (year_ices_data)
  })

# Venn diagram showing ices intersection in 2003 and 2011

ices_survey_square_by_year <-
  list(
    y_2003 = year_abundances_by_ices[[1]]$StatRec,
    y_2011 = year_abundances_by_ices[[2]]$StatRec
  )

ices_hauling_by_year_plot <- ggVennDiagram(
  ices_survey_square_by_year,
  category.names = c("2003 Survey", "2011 Survey"),
  label_size = 3,
  set_size = 3
) +
  ggtitle("Ices squared surveyed in 2003 and 2011") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 10, hjust = 0.5, face = "bold")
  )
