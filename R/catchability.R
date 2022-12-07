library(R6)
library(dplyr)


calculate_100_area <- function(haul_distance,
                               door_distance,
                               wing_distance) {
  total_area <- door_distance * haul_distance # outer rectangle
  wing_area <- wing_distance * haul_distance # inner rectangle
  flank_areat_a_100 <-
    (total_area - wing_area) / 4 # 1/4 of outer - inner
  total_100_area <- wing_area + flank_areat_a_100
  return (round (total_100_area, 3))
  
}

calculate_abundance <- function(area, num_fish, to_km = FALSE) {
  m2_in_km2 <- 10 ** 6
  if (to_km) {
    abundance <- (num_fish / area) * m2_in_km2
  }
  else {
    abundance <- (num_fish / area)
  }
  return (round(abundance, 4))
}

select_working_data <-
  function(data,
           keep_cols,
           years,
           num_fish) {
    dfs <- lapply(years, function (year) {
      df_common_cols <-
        data %>%
        filter(Year == year) %>%
        select(all_of(keep_cols))
      
      
      df_num_fish <-
        data %>%
        filter(Year == year) %>%
        group_by(haul.id) %>%
        summarise(num_fish = sum(HLNoAtLngt), .groups = 'drop')
      
      return (unique(merge(
        df_common_cols,
        df_num_fish,
        by = c('haul.id')
      )))
    })
    return (dfs)
  }

# add_100_area_and_abundance <- function (data,
#                                         keep_cols,
#                                         years,
#                                         haul_distance,
#                                         door_distance,
#                                         wing_distance,
#                                         num_fish) {
#
# }