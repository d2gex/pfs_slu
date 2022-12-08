source("R/config.R")
source ("R/catchability.R")
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

year_abundances <- lapply(by_year_hauls, function (year_data) {
  calculate_abundance_by_haul_km2 (year_data, wing_spread) %>%
    arrange(-abundance_km2)
})
