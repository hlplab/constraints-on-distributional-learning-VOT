# load required libraries
library(tidyverse)
library(brms)
library(magrittr)

d.for_IA_predictions <- 
  read_csv("~/Desktop/d.for_IA_prediction_model.csv", show_col_types = FALSE)

d.for_IA_predictions %<>%
  mutate(Block = as.numeric(
    case_when(
      Block == "1" ~ 1,
      Block == "3" ~ 2,
      Block == "5" ~ 3,
      Block == "7" ~ 4))) 


my_priors <- c(
  prior(student_t(3, 0, 2.5), class = "b", dpar = "mu2"))
  #prior(cauchy(0, 2.5), class = "sd", dpar = "mu2"),
  #prior(lkj(1), class = "cor"))

fit_test.IA_predicted <-
  brm(
    bf(
      Response.Voiceless ~ 1,
      mu1 ~ 0 + offset(0),
      mu2 ~ 1 + IA.logodds_block1 + 
        (IA.logodds_delta_condition_and_block_mean + IA.logodds_delta_condition_and_block_delta) * Block,
      theta1 ~ 1),
    data = d.for_IA_predictions %>%
      group_by(Condition.Exposure, Block) %>%
      mutate(
        IA.logodds_delta_condition_and_block_mean = mean(IA.logodds_delta),
        IA.logodds_delta_condition_and_block_delta = IA.logodds_delta - IA.logodds_delta_condition_and_block_mean),
    prior = my_priors,
    cores = 4,
    chains = 4,
    init = 0,
    iter = 4000,
    warmup = 2000,
    family = mixture(bernoulli("logit"), bernoulli("logit"), order = F),
    control = list(adapt_delta = .995), 
    file ="~/Desktop/constraints-on-distributional-learning-VOT/models/test-IA-logodds-predict.rds",
    file_refit = "on_change")
