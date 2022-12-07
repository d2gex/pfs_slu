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

by_year_hauls <-
  select_working_data(sprat_data, selected_cols, selected_years)
