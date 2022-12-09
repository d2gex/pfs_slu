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
  list(y_2003 = year_abundances_by_ices[[1]]$StatRec,
       y_2011 = year_abundances_by_ices[[2]]$StatRec)

ices_hauling_by_year_plot <- ggVennDiagram(
  ices_survey_square_by_year,
  category.names = c("2003 Survey", "2011 Survey"),
  label_size = 3,
  set_size = 3
) +
  ggtitle("ICES squared surveyed in 2003 and 2011") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 10, hjust = 0.5, face = "bold")
  )

# Comparing abundances through ices squares
y_ab <-
  year_abundances_by_ices[[1]] [, c('StatRec',
                                    'num_hauls',
                                    'total_abundance')] %>%
  mutate(year = 2003)

y_ab_2011 <-
  year_abundances_by_ices[[2]] [, c('StatRec',
                                    'num_hauls',
                                    'total_abundance')] %>%
  mutate(year = 2011)

y_ab <-
  rbind(y_ab, y_ab_2011) %>%
  mutate(total_abundance = round(total_abundance / 1000, 3))

haul_year_barplot <- ggplot(y_ab,
                            aes(
                              x = StatRec,
                              y = total_abundance,
                              group = year,
                              fill = factor(year)
                            )) +
  geom_bar(position = "dodge",
           stat = "identity",) +
  ylab(expression("Abundance in thousands/km" ^ "2")) +
  xlab("ICES statistic squares") +
  scale_fill_discrete(name = "Years") +
  scale_y_continuous(breaks = seq(min(y_ab$total_abundance),
                                  max(y_ab$total_abundance), by = 1000)) +
  ggtitle("A) Aboundance for 2003 and 2011 across different ICES statistic squares") +
  theme_bw() +
  theme(plot.title = element_text(size = 11,
                                  hjust = 0.5,
                                  face = "bold"))

# Comparing hauls through ices squares
ices_year_barplot <- ggplot(y_ab,
                            aes(
                              x = StatRec,
                              y = num_hauls,
                              group = year,
                              fill = factor(year)
                            )) +
  geom_bar(position = "dodge",
           stat = "identity",) +
  ylab("Number of hauls") +
  xlab("ICES statistic squares") +
  scale_fill_discrete(name = "Years") +
  scale_y_continuous(breaks = seq(min(y_ab$num_hauls),
                                  max(y_ab$num_hauls), by = 1)) +
  ggtitle("B) Number of hauls for 2003 and 2011 across different ICES statistic squares") +
  theme_bw() +
  theme(plot.title = element_text(size = 11,
                                  hjust = 0.5,
                                  face = "bold"))


# Export abundance by hauls and year to csv
write.csv(year_abundances_by_hauls[[1]],
          file.path(OUTPUTS_PATH, 'hauls_2003.csv'))
write.csv(year_abundances_by_hauls[[2]],
          file.path(OUTPUTS_PATH, 'hauls_2011.csv'))
write.csv(year_abundances_by_ices[[1]],
          file.path(OUTPUTS_PATH, 'ices_2003.csv'))
write.csv(year_abundances_by_ices[[2]],
          file.path(OUTPUTS_PATH, 'ices_2011.csv'))
