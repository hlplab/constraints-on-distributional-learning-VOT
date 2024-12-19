# load required libraries
library(tidyverse)
library(brms)
library(magrittr)

d.IA_predicted_logodds <- 
  read_csv("/home/mtan12/AEDLVOT/data/d.IA_predicted_logodds.csv", show_col_types = FALSE)

VOT.mean_test <- mean(d.IA_predicted_logodds$Item.VOT)
VOT.sd_test <- sd(d.IA_predicted_logodds$Item.VOT)

# code Block as factor
d.IA_predicted_logodds %<>%
  ungroup() %>% 
  droplevels() %>%
  mutate(Block = factor(Block),
         VOT_gs = Item.VOT - VOT.mean_test / (2 * VOT.sd_test))

contrasts(d.IA_predicted_logodds$Block) <- MASS::fractions(MASS::contr.sdif(4))
dimnames(contrasts(d.IA_predicted_logodds$Block))[[2]] <- c("_Block2 vs. Block1", "_Block3 vs. Block2", "_Block4 vs. Block3")

my_priors <-  c(
  prior(student_t(3, 0, 2.5), class = "b", dpar = "mu2"),
  prior(student_t(3, 0, 15), class = "b", coef = "VOT_gs", dpar = "mu2"),
  prior(cauchy(0, 2.5), class = "sd", dpar = "mu2"),
  prior(lkj(1), class = "cor"))

fit_test.upto.block4 <- 
  brm(
    formula = bf(Response.Voiceless ~ 1,
                 mu1 ~ 0 + offset(0),
                 mu2 ~ 1 + VOT_gs * Condition.Exposure * Block + 
                   (1 + VOT_gs * Block | ParticipantID) + (1 + VOT_gs * Condition.Exposure * Block | Item.MinimalPair),
                 theta1 ~ 1),
    data = d.IA_predicted_logodds,
    prior = my_priors,
    cores = 4,
    chains = 4,
    init = 0,
    iter = 4000,
    warmup = 2000,
    save_pars = save_pars(all = TRUE),
    family = mixture(bernoulli("logit"), bernoulli("logit"), order = F),
    control = list(adapt_delta = 0.99),
    file = "/home/mtan12/AEDLVOT/models/test-standard-SD15-upto-block4.rds",
    file_refit = "on_change"
  )
