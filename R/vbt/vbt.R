source("R/vbt/utils.R")
library(dplyr)
library(minpack.lm)
library(ggpubr)
library(FSA)

vbT <- vbFuns("typical")
vbTSomers <- vbFuns("Somers")

#---------------------------------------------------------------
#  Naive vbT
#---------------------------------------------------------------

# (2) Calculate average length per age class
h_df_mean_ages <- herring_sample %>%
  arrange(age) %>%
  group_by(age) %>%
  summarise(x = mean(length))

# (3) Prepare sequence of lengths to draw an slope between Lt and Lt+1
h_df_initial_slope <- h_df_mean_ages %>%
  mutate(y = lead(x, 1)) %>%
  slice(1:n() - 1)

# (4) find the slope between Lt and Lt+1
age_linear_model <- lm (data = h_df_initial_slope, y ~ x)
summary(age_linear_model)
age_length_cof <- coef(age_linear_model)
m <- age_linear_model$coefficients[['x']]
b <- age_linear_model$coefficients[['(Intercept)']]


# (5)  Calculate growth and asyntotic value
# Lt+1 = Linf * (1 - e^-k) + Lt*e-k
# When fitting a linear model  y = mx + b, then
# Linf * (1 - e^-k) + Lt*e^-k = mx + b iff:
# --> x = e^-k --> k = -ln(x)
# --> b = Linf * (1 - e^-k) = Linf * (1 - x) --> Linf = b/(1-x)
k_n1 <- -log(m)
linf_n1 <- b / (1 - m)

# (6) calculate t0 = t + 1/k (ln (linf-lt) / linf)
# -> we get the most populated class
age <- (herring_data %>%
          group_by(age) %>%
          summarise(num_individuals = n()) %>%
          arrange(desc(num_individuals)))[1,]$age
l_age = h_df_mean_ages[h_df_mean_ages$age == 0,]$x
t0_n1 = 0 + (1 / k_n1) * (log ((linf_n1 - l_age) / linf_n1))

# (7) plot naive von bertalanffy function

h_t_length_naive <- data.frame(t = herring_data$age,
                               f_length = herring_data$length)

lt_naive <-
  linf_n1 * (1 - exp(-k_n1 * (h_t_length_naive$t - t0_n1)))

vbf_naive <-
  data.frame(t = h_t_length_naive$t, vbf_length = lt_naive) %>%
  arrange(t)

#---------------------------------------------------------------
#  vbT with no quarters
#---------------------------------------------------------------

# (8) Redraw the vbf curve with a more accurate method by estimating
# the parameters without yet using the quarters

h_t_length_nq <-
  data.frame(t = herring_data$age, f_length = herring_data$length) %>%
  mutate_all(function(x)
    as.numeric(x))

# --> Estimate the new parameters
formula <- f_length ~ vbT(t, linf, k, t0)
adjusted_vbf_nq <- nls(
  formula = formula,
  data = h_t_length_nq,
  start = list(linf = linf_n1, k = k_n1, t0 = t0_n1)
)
vbf_nq_coefs <- coef(summary(adjusted_vbf_nq))
linf_nq <- vbf_nq_coefs[1]
k_nq <- vbf_nq_coefs[2]
t0_nq <- vbf_nq_coefs[3]


lt_non_quarter <-
  linf_nq * (1 - exp(-k_nq * (h_t_length_nq$t - t0_nq)))

vbf_nq <-
  data.frame(t = herring_data$age, vbf_length = lt_non_quarter) %>%
  arrange(t)

#---------------------------------------------------------------
#  vbT with quarters
#---------------------------------------------------------------
# (9) Redraw the vbf curve with a more accurate method by estimating
# the parameters by using the quarters

# --> Add age 0.25 to 0.75 to each age
h_t_length_q <- data.frame(
  t = herring_data$age,
  quarter = herring_data$quarter,
  f_length = herring_data$length
)  %>%
  mutate(t = case_when(
    quarter == 1 ~ t + 0,
    quarter == 2 ~ t + 0.50,
    quarter == 3 ~ t + 0.75,
    quarter == 4 ~ t + 0.85
  ))

# --> find again naive coefficients
vbf_q_coefs <-
  vbStarts(f_length ~ t, data = h_t_length_q, type = "typical")
linf_n2 <- vbf_q_coefs[1]
k_n2 <- vbf_q_coefs[2]
t0_n2 <- vbf_q_coefs[3]

adjusted_vbf_q <- nls(
  formula = f_length ~ vbT(t, linf, k, t0),
  data = h_t_length_q,
  start = list(linf = linf_n2, k = k_n2, t0 = t0_n2)
)
vbf_nq_coefs <- coef(summary(adjusted_vbf_q))
linf_q <- vbf_nq_coefs[1]
k_q <- vbf_nq_coefs[2]
t0_q <- vbf_nq_coefs[3]

lt_quarter <-
  linf_q * (1 - exp(-k_q * (h_t_length_q$t - t0_q)))

vbf_q <-
  data.frame(t = h_t_length_q$t, vbf_length = lt_quarter) %>%
  arrange(t)
