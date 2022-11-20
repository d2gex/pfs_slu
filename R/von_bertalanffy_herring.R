library(ggplot2)
library(dplyr)


# (1) read the datafarame 
herring_data <- read.csv("data/herring_data_221116.csv") 

# (2) Calculate average length per age class
h_df_mean_ages <- herring_data %>% 
  arrange(age) %>% 
  group_by(age) %>% 
  summarise(x = mean(length))

# (3) Prepare sequence of lengths to draw an slope between Lt and Lt+1 
h_df <- h_df_mean_ages %>%
  mutate(y = lead(x, 1)) %>% 
  slice(1: n() -1)
  
# (4) find the slope between Lt and Lt+1
age_linear_model <- lm (data = h_df, y ~ x)
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
k <- -log(m)
linf <- b / (1 - m)

# (6) calculate t0 = t + 1/k (ln (linf-lt) / linf)
# -> we get the most populated class
age <- (herring_data %>% 
  group_by(age) %>% 
  summarise(num_individuals = n()) %>% 
  arrange(desc(num_individuals)))[1, ]$age
l_age = h_df_mean_ages[h_df_mean_ages$age == 3, ]$x
t0 = age + (1/k) * (log ((linf - l_age) / linf))

# (7) plot naive von bertalanffy function

vbf_naive <- data.frame(age = herring_data$age, 
                        vbf = linf * (1 - exp(-k * (herring_data$age - t0)))) %>%
  arrange(age)


# ggplot(data = h_df, aes(x = x, y = y)) + 
#   geom_point() +
#   stat_smooth(method = "lm", col = "red")

ggplot(data = vbf_naive, aes(x = age, y = vbf)) + 
  geom_point() +
  geom_line()


