source("R/config.R")
source ("R/catchability.R")

sprat_data[sapply(sprat_data, is.nan)] <- 0
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