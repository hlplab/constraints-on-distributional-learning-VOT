# load required libraries
library(tidyverse)
library(brms)
library(magrittr)
library(MASS)

prepVars <- function(d, levels.Condition = NULL) {
  d %<>%
    drop_na(Condition.Exposure, Phase, Block, Item.MinimalPair, ParticipantID, Item.VOT, Response)
  
  print(paste("VOT mean:", signif(mean(d$Item.VOT, na.rm = T))))
  print(paste("VOT sd:", signif(sd(d$Item.VOT, na.rm = T))))
  
  d %<>% 
    ungroup() %>% 
    mutate(
      Block_n = as.numeric(as.character(Block)),  
      across(c(Condition.Exposure, Block, Item.MinimalPair), factor),

      Condition.Exposure = factor(Condition.Exposure, levels = levels.Condition)) %>% 

    drop_na(Block, Response, Item.VOT) %>% 
    mutate(VOT_gs = (Item.VOT - mean(Item.VOT, na.rm = TRUE)) / (2 * sd(Item.VOT, na.rm = TRUE))) %>% 
    droplevels()

  print(paste("mean VOT is", mean(d$Item.VOT), "and SD is", sd(d$Item.VOT)))
  
  contrasts(d$Condition.Exposure) = cbind("_Shift0 vs. Shift10" = c(-2/3, 1/3, 1/3),
                                           "_Shift10 vs. Shift40" = c(-1/3,-1/3, 2/3))
  require(MASS)
  if (all(d$Phase == "test") & n_distinct(d$Block) > 1) {
   contrasts(d$Block) <- contr.sdif(6)  
   dimnames(contrasts(d$Block))[[2]] <- c("_Block1 vs. Block3", "_Block3 vs. Block5", "_Block5 vs. Block7", "_Block7 vs. Block8", "_Block8 vs. Block9")
 
  print(contrasts(d$Condition.Exposure))
  print(contrasts(d$Block)) } else {
    print(contrasts(d$Condition.Exposure))
  }
  
  return(d)
}

# read in data file
d.test_exposure_for_analysis <- read_csv(file = "/home/mtan12/AEDLVOT/data/d.test_exposure_exp_2.csv", show_col_types = F) 

d.test_exposure_for_analysis %<>% mutate(Response.Voiceless = ifelse(Response.Voicing == "voiceless", 1, 0))
levels_Condition.Exposure <- c("Shift0", "Shift10", "Shift40")

# set priors for psychometric model
my_priors <- c(
  prior(student_t(3, 0, 2.5), class = "b", dpar = "mu2"), # prior 
  prior(student_t(3, 0, 2.5), class = "b", dpar = "theta1"), 
  prior(cauchy(0, 2.5), class = "sd"),
  prior(lkj(1), class = "cor")
)

# fitting a nested model to facilitate easier extraction of slopes and intercepts of each condition and block
fit_mix_nested <- brm(
  bf(
    Response.Voiceless ~ 1,
    mu1 ~ 0 + offset(0),
    mu2 ~  0 + I(paste(Condition.Exposure, Block, sep = "x")) / VOT_gs + 
      (0 + Block / VOT_gs | ParticipantID) + 
      (0 + I(paste(Condition.Exposure, Block, sep = "x")) / VOT_gs | Item.MinimalPair),
    theta1 ~ 1),
  data = d.test_exposure_for_analysis %>% 
    filter(Phase == "test") %>% 
    prepVars(levels.Condition = levels_Condition.Exposure),
  cores = 4,
  chains = 4,
  init = 0,
  iter = 4000, # iterations to run
  warmup = 2000, # samples used to fit
  family = mixture(bernoulli("logit"), bernoulli("logit"), order = F),
  control = list(adapt_delta = .99),
  file = "/home/mtan12/AEDLVOT/models/Exp-AE-DLVOT-labelled-lapsing-GLMM-nested.rds")
  
