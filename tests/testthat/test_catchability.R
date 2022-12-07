num_fish <- 500
wing_distance <- 20 # m
door_distance <- 100 # m
haul_distnace <- 5000 # m


test_that("100% catchability area calculation", {
  #  wing area: 20 * 5000 =  100,000 m2 = 0.1 km2
  #  door area: 100 * 5000 =  500,000 m2 = 0.5 km2
  # 1/4 * (wing_area - door_area):  = 100,000 m2 = 0.1 km2
  # total 100% area: 100,000 + 100,000 = 200,000 m2 = 0.2 km2
  
  expected_area = 200000 # m2
  expect_equal(calculate_100_area(haul_distnace, door_distance, wing_distance),
               expected_area)
  
})

test_that("Abundance Calculation", {
  m2_in_km2 <- 10 ** 6
  actual_area_meters <-
    calculate_100_area(haul_distnace, door_distance, wing_distance)
  expected_area_meters = 200000 # m2
  
  actual_abundance_m2 <-
    calculate_abundance (actual_area_meters, num_fish, to_km = FALSE)
  actual_abundance_km2 <-
    calculate_abundance (actual_area_meters, num_fish, to_km = TRUE)
  expected_abundance_m2 <- 0.0025 # fish/m2
  expected_abundance_km2 <- 0.0025 * m2_in_km2 # fish/km2
  
  
  expect_equal(actual_abundance_m2,
               expected_abundance_m2)
  expect_equal(actual_abundance_km2,
               expected_abundance_km2)
  
  
  
})