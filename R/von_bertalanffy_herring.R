library(ggplot2)
library(dplyr)
library(minpack.lm)
library(ggpubr)

plot_vbf <-
  function (title,
            x_label,
            y_label,
            k,
            linf,
            t0,
            c = NULL,
            s = NULL) {
    return (
      ggplot(data = vbf_naive, aes(x = t, y = vbf_length)) +
        geom_point() +
        geom_line() +
        ggtitle(title) +
        xlab(x_label) +
        ylab(y_label) +
        theme_bw() +
        theme(plot.title = element_text(
          size = 12,
          hjust = 0.5,
          face = "bold"
        )) +
        annotate(
          "text",
          x = 1,
          y = 200,
          label = paste("K:", round(k, 4))
        ) +
        annotate(
          "text",
          x = 1,
          y = 190,
          label = paste("Linf:", round(linf, 4))
        ) +
        annotate(
          "text",
          x = 1,
          y = 180,
          label = paste("t0:", round(t0, 4))
        )
    )
    
  }

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
  slice(1:n() - 1)

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
          arrange(desc(num_individuals)))[1,]$age
l_age = h_df_mean_ages[h_df_mean_ages$age == 0,]$x
t0 = 0 + (1 / k) * (log ((linf - l_age) / linf))

# (7) plot naive von bertalanffy function

vbf_naive <- data.frame(t = herring_data$age,
                        vbf_length = linf * (1 - exp(-k * (
                          herring_data$age - t0
                        )))) %>%
  arrange(t)



# (8) Redraw the vbf curve with a more accurate method by estimating
# the parameters an dusing the

h_t_length <-
  data.frame(t = herring_data$age, f_length = herring_data$length) %>%
  mutate_all(function(x)
    as.numeric(x))

# --> Estimate the new parameters
lt <- linf * (1 - exp(-k * (t - t0)))
formula <- f_length ~ lt
adjusted_vbf <- nls(
  f_length ~ vbT(t, linf, k, t0),
  data = h_t_length,
  start = list(linf = linf, k = k, t0 = t0)
)
vbf_nq_coefs <- coef(summary(adjusted_vbf))
linf_nq <- vbf_nq_coefs[1]
k_nq <- vbf_nq_coefs[2]
t0_nq <- vbf_nq_coefs[3]

lt_non_quarter <- linf_nq * (1 - exp(-k_nq * (t - t0_nq)))

vbf_nq <-
  data.frame(t = herring_data$age, vbf_length = lt_non_quarter) %>%
  arrange(t)

# (10) draw all graphs

# --> Plot lt vs lt+1 slope
lt_lt1_graph <- ggplot(data = h_df, aes(x = x, y = y)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  xlab("Lt (cms)") +
  ylab("Lt +1 (cms)") +
  ggtitle("Slope generated from representing Lt vs Lt+1") +
  theme_bw() +
  theme(plot.title = element_text(size = 12,
                                  hjust = 0.5,
                                  face = "bold"))  +
  stat_regline_equation(label.y = 240, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 230, aes(label = ..rr.label..))


# --> Plot naive Von Bertlanffy result

vbf_naive_graph <-
  plot_vbf (
    "Initial Von Bertalanffy curve after solving the equation",
    "Age (years)",
    "Length (cms)",
    k,
    linf,
    t0,
  )

# --> Plot estimated Von Bertlanffy result

vbf_naive_graph <-
  plot_vbf (
    "Estimated Von Bertalanffy curve after parameter estimations (no quarters)",
    "Age (years)",
    "Length (cms)",
    k_nq,
    linf_nq,
    t0_nq,
  )

