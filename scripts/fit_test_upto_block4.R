# load required libraries
library(tidyverse)
library(brms)
library(magrittr)

d.IA_predicted_logodds <- 
  read_csv("/home/mtan12/AEDLVOT/data/d.IA_predicted_logodds.csv", show_col_types = FALSE)

VOT.mean_test_upto_block4 <- mean(d.IA_predicted_logodds$Item.VOT)
VOT.sd_test_upto_block4 <- sd(d.IA_predicted_logodds$Item.VOT)

d.IA_predicted_logodds %<>%
  ungroup() %>% 
  droplevels() %>%
  mutate(across(c(Block, Condition.Exposure), factor),
         VOT_gs = Item.VOT - VOT.mean_test_upto_block4 / (2 * VOT.sd_test_upto_block4))

contrasts(d.IA_predicted_logodds$Block) <- MASS::fractions(MASS::contr.sdif(4))
dimnames(contrasts(d.IA_predicted_logodds$Block))[[2]] <- c("_Test2 vs. Test1", "_Test3 vs. Test2", "_Test4 vs. Test3")
contrasts(d.IA_predicted_logodds$Condition.Exposure) <- cbind("_Shift10 vs. Shift0" = c(-2/3, 1/3, 1/3),
                                                              "_Shift40 vs. Shift10" = c(-1/3,-1/3, 2/3))

my_priors <-  c(
  prior(student_t(3, 0, 2.5), class = "b", dpar = "mu2"),
  prior(student_t(3, 0, 15), class = "b", coef = "VOT_gs", dpar = "mu2"),
  prior(cauchy(0, 2.5), class = "sd", dpar = "mu2"),
  prior(lkj(1), class = "cor"))

fit_test.upto.block4 <- 
  brm(
    bf(Response.Voiceless ~ 1,
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
