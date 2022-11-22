source("R/vbt/utils.R")
library(dplyr)
library(minpack.lm)
library(ggpubr)
library(FSA)
library(R6)

VonBertalanffy <- R6Class(
  "VonBertalanffy",
  public = list(
    data_sample = NULL,
    vbT = NULL,
    
    initialize = function(data_sample) {
      self$data_sample = data_sample
      self$vbT =  vbFuns("typical")
    },
    
    get_lt_lt1_means = function() {
      mean_ages <- self$data_sample %>%
        arrange(age) %>%
        group_by(age) %>%
        summarise(x = mean(length))
      
      # Prepare sequence of lengths to draw an slope between Lt and Lt+1
      lt_lt_1_data <- mean_ages %>%
        mutate(y = lead(x, 1)) %>%
        slice(1:n() - 1)
      
      return(lt_lt_1_data)
    },
    
    find_vbf_initial_params = function(lt_lt1_means) {
      # (1) find the slope between Lt and Lt+1
      age_linear_model <- lm (data = lt_lt1_means, y ~ x)
      age_length_cof <- coef(age_linear_model)
      m <- age_linear_model$coefficients[['x']]
      b <- age_linear_model$coefficients[['(Intercept)']]
      
      
      # (2)  Calculate growth and asyntotic value
      # Lt+1 = Linf * (1 - e^-k) + Lt*e-k
      # When fitting a linear model  y = mx + b, then
      # Linf * (1 - e^-k) + Lt*e^-k = mx + b iff:
      # --> x = e^-k --> k = -ln(x)
      # --> b = Linf * (1 - e^-k) = Linf * (1 - x) --> Linf = b/(1-x)
      k <- -log(m)
      linf <- b / (1 - m)
      
      # (3) calculate t0 = t + 1/k (ln (linf-lt) / linf)
      # -> we get the most populated year class to calculate t0
      age <- (
        self$data_sample %>%
          group_by(age) %>%
          summarise(num_individuals = n()) %>%
          arrange(desc(num_individuals))
      )[1,]$age
      l_age = lt_lt1_means[lt_lt1_means$age == age,]$x
      t0 = age + (1 / k) * (log ((linf - l_age) / linf))
      return(list(linf = linf, k = k, t0 = t0))
      
    },
    
    naive = function(params) {
      t_length_naive <- data.frame(t = self$data_sample$age,
                                   f_length = self$data_sample$length)
      
      linf = params$linf
      k = params$k
      t0 = params$t0
      
      vbf_formula <-
        linf * (1 - exp(-k * (t_length_naive$t - t0)))
      
      vbf <-
        data.frame(t = t_length_naive$t, vbf_length = vbf_formula) %>%
        arrange(t)
      
      return(c(list(data = vbf), params))
      
    },
    
    estimated = function(is_quarter = FALSE) {
      if (is_quarter) {
        t_length_data <- data.frame(
          t = self$data_sample$age,
          quarter = self$data_sample$quarter,
          f_length = self$data_sample$length
        )  %>%
          mutate(t = case_when(quarter == 1 ~ t + 0.125,
                               quarter == 4 ~ t + 0.875))
      }
      else {
        t_length_data <-
          data.frame(t = self$data_sample$age,
                     f_length = self$data_sample$length) %>%
          mutate_all(function(x)
            as.numeric(x))
      }
      
      t_length_data <- t_length_data %>% mutate_all(function(x)
        as.numeric(x))
      
      # --> find again naive coefficients
      vbf_coefs <-
        vbStarts(f_length ~ t, data = t_length_data, type = "typical")
      linf <- vbf_coefs[1]
      k <- vbf_coefs[2]
      t0 <- vbf_coefs[3]
      
      adjusted_vbf <- nls(
        formula = f_length ~ self$vbT(t, linf, k, t0),
        data = t_length_data,
        start = list(linf = linf, k = k, t0 = t0)
      )
      vbf_coefs <- coef(summary(adjusted_vbf))
      linf <- vbf_coefs[1]
      k <- vbf_coefs[2]
      t0 <- vbf_coefs[3]
      
      vbf_formula <-
        linf * (1 - exp(-k * (t_length_data$t - t0)))
      
      vbf <-
        data.frame(t = t_length_data$t, vbf_length = vbf_formula) %>%
        arrange(t)
      
      return (list(
        data = vbf,
        linf = linf,
        k = k,
        t0 = t0
      ))
    }
  )
)
