library(R6)
library(dplyr)

Catchability <- R6Class(
  'Catchability',
  public = list(
    h = NULL,
    d_distance = NULL,
    w_distance = NULL,
    total_area = NULL,
    catch_100_area = NULL,
    catch_50_area = NULL,
    swept_fish_num = NULL,
    wing_catchability = NULL,
    sweep_catchability = NULL,
    catchability_ratio = NULL,
    area_ratio = NULL,
    initialize = function(swept_fish_num,
                          haul_distance,
                          door_distance,
                          wing_distance,
                          wing_catchability,
                          sweep_catchability) {
      self$h  <- haul_distance
      self$d_distance <- door_distance
      self$w_distance <- wing_distance
      self$swept_fish_num <- swept_fish_num
      self$wing_catchability <- wing_catchability
      self$sweep_catchability <- sweep_catchability
      self$catchability_ratio <- self$calc_catchability_ratio ()
      self$area_ratio <- self$calc_area_ratio ()
      self$calculate_area_km2()
      
    },
    calculate_abundance_km2 = function() {
      # spread the caught fish evenly across catch_100 and catch_50 area
      n_cash_100 <- self$swept_fish_num * self$area_ratio
      n_cash_50 <- self$swept_fish_num - n_cash_100
      n_cash_100_km2 <- n_cash_100 / self$catch_100_area
      n_cash_50_km2 <-
        n_cash_50 * self$catchability_ratio / self$catch_100_area
      return(n_cash_100_km2 + n_cash_50_km2)
      
    }
  ),
  
  private = list (
    calculate_area_km2 = function() {
      km = 1000
      self$total_area <- round(self$d_distance * self$h / km, 2)
      self$catch_100_area <-
        round(self$w_distance * self$h / km, 2)
      # split by 2 as we only get half of the rectangle on the flanks
      self$catch_50_area <-
        round(((self$total_area - self$catch_100_area) / 2) / km, 2)
    },
    
    calc_catchability_ratio = function() {
      # order that catchability is higher on the wing zone vs sweep zone
      return(round(self$wing_catchability / self$sweep_catchability, 2))
    },
    
    calc_area_ratio = function() {
      # Order that wing area is smaller than sweep area
      return(round(self$catch_100_area / self$catch_50_area, 2))
    }
    
  )
)
