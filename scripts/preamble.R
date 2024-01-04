# Libraries ---------------------------------------------------------------------

# Make sure the following are installed. This includes packages that are not on CRAN
# and packages that are not loaded below but instead directly references in the code
# (to avoid having to load packages into memory of which we only use a few functions).
list.of.packages <- c("remotes", "papaja", "MVBeliefUpdatr", "supunsup", "MASS", "terra", "lme4")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) {
  if ("remotes" %in% new.packages) install.packages("remotes")
  if ("papaja" %in% new.packages) remotes::install_github("crsh/papaja")
  if ("MVBeliefUpdatr" %in% new.packages) remotes::install_github("hlplab/MVBeliefUpdatr")
  if ("supunsup" %in% new.packages) remotes::install_github("kleinschmidt/phonetic-sup-unsup")
  new.packages <- setdiff(new.packages, c("remotes", "papaja", "MVBeliefUpdatr", "supunsup"))

  install.packages(new.packages)
}

library(papaja)             # APA formatted ms

library(tidyverse)          # keeping things tidy
library(magrittr)           # pipes
library(rlang)              # quosures (in functions)
library(assertthat)         # asserts (in functions)

library(patchwork)          # plot layouts
library(magick)
library(webshot)
library(ggstance)
library(ggforce)            # facet_matrix for pairwaise correlation plots
library(ggtext)             # make geom textboxes
library(ggnewscale)         # extra colour scale in ggplots
library(kableExtra)         # for formatting tables

library(linguisticsdown)    # IPA symbols
library(latexdiffr)         # track changes

library(brms)               # fit Bayesian regression models
library(tidybayes)          # posterior samples and plots in tidy format
library(broom.mixed)        # extracting effects from lmer models
library(posterior)

library(phonR)              # normalization of f0
library(supunsup)           # Kleinschmidt & Jaeger 2016 data
library(MVBeliefUpdatr)     # generating Ideal Observers

library(furrr)              # future_map for parallelization

# Functions ---------------------------------------------------------------------
source("functions.R")


# Constants ---------------------------------------------------------------------
RESET_FIGURES = F   # switch on/off whether figures that have been stored in files are regenerated
RESET_MODELS = F    # switch on/off whether models that have been stored in files are rerun

# For Stan/rstan
chains <- 4
options(
  width = 1000,
  mc.cores = min(chains, parallel::detectCores()))

# Get citation information
r_refs(file = "latex-stuff/r-references.bib")

# plot formatting
myGplot.defaults("paper")
colours.condition <- c("Shift0" = "#cc0000", "Shift10" = "#12D432", "Shift40" = "#0481F3")
colours.category_greyscale <- c("/d/" = "#afafaf", "/t/" = "#d2d4dc")
colours.category <- c("/d/" = "#eb6223", "/t/" = "#522888")

set.seed(42007)

# Load data ---------------------------------------------------------------------

n_min_per_talker <- 15

# Get production data for the prior.
#

# We are reimporting the data because here we are subsetting the Mixer 6 data to
# talkers that have at least 10 instances each of the relevant stops (we're being
# conservative here since we're creating ideal observers based on this data that
# are averaging across talkers, and so we'd like to make sure that each talker-
# specific IO is based on enough data).
d.chodroff_wilson <-
  get_ChodroffWilson_data(
    database_filename = "../data/chodroff_wilson_speech_db.csv",
    categories = c("/d/", "/t/"),
    min.n_per_talker_and_category = n_min_per_talker,
    limits.VOT = c(-Inf, Inf),
    limits.f0 = c(-Inf, Inf),
    # Setting multi-modality exclusion to 0 disables it
    max.p_for_multimodality = 0)

d.chodroff_wilson %<>% 
  mutate(across(c(speechstyle, Talker, category, gender), factor)) %>%
  # Subset to female talkers and exclude cases with NAs and distributional outliers
  group_by(Talker) %>% 
  filter(
    gender == "female",
    if_all(c("VOT", "f0_Mel", "vowel_duration"), ~ !is.na(.x)),
    if_all(c("VOT", "f0_Mel", "vowel_duration"), ~ abs(scale(.x)) < 3.5))

# We selected an f0_Mel cutoff point based on visual inspection to remove pitch-halving tokens
# (this truncates the f0 distribution).
d.chodroff_wilson %>%
  ggplot(aes(x = f0_Mel)) +
  geom_density() +
  geom_vline(xintercept = 200, linetype = "dashed") +
  facet_wrap(~ Talker, ncol = 5)

# Filter out tokens with pitch-halving
  d.chodroff_wilson %<>% 
  filter(case_when(
    Talker %in% c("113093", "120367", "120426", "120488", "120521", "120562", "120563", "120577", "120585", "120597", "120666", "120685") ~ f0_Mel > 100,
    Talker %in% c("120330", "120425", "120480", "120506", "120565", "120611", "120660", "120684", "120701", "120751", "120826") ~ f0_Mel > 150,
    Talker %in% c("120544", "120882") ~ f0_Mel > 175,
    Talker %in% c("120306", "CVC16", "CVC19", "120503") ~ f0_Mel > 225,
    Talker %in% c("120651", "120707", "120763") ~ f0_Mel > 250,
    TRUE ~ f0_Mel > 200
  )) 

# This dataframe is used whenever we reference overall stats from Chodroff & Wilson,
# prior to excluding any talkers, or subsetting the data. Not all of the quantities
# were reported in the paper.
d.chodroff_wilson.talker_stats <-
  d.chodroff_wilson %>%
  group_by(speechstyle, Talker, category) %>%
  summarise(
    across(
      c("VOT", "f0_Mel", "vowel_duration"),
      list(mean = mean, sd = sd, var = var))) %>%
  group_by(speechstyle, category) %>%
  summarise(
    across(
      ends_with(c("_mean", "_sd", "_var")),
      list(mean = mean, sd = sd, var = var)))

# SUBSET TO DATA THAT WE USE TO CREATE PRIORS FOR THE PRESENT PROJECT
d.chodroff_wilson %<>%
  group_by(speechstyle, Talker, category) %>%
  # Subset the data to be balanced, so that each talker provides an equal number of
  # /d/ and /t/ tokens, the maximal number of tokens possible for that talker. This
  # is done in order to make sure that normalization (if applied) doesn't introduce
  # indirect information about category identity.
  mutate(n.for_category_and_talker = n()) %>%
  group_by(speechstyle, Talker) %>%
  # Select talkers that have both /d/ and /t/ observations
  filter(all(c("/d/", "/t/") %in% unique(category))) %>%
  # Get the smaller number of counts for the two categories for each talker and sample
  # that many (and thus equally many) tokens from each category for that talker.
  mutate(n_min.for_category_and_talker = min(n.for_category_and_talker)) %>%
  group_by(speechstyle, Talker, category) %>%
  sample_n(size = unique(n_min.for_category_and_talker)) %>%
  ungroup()

d.chodroff_wilson.talker_stats <-
  d.chodroff_wilson %>%
  group_by(speechstyle, Talker, category) %>%
  summarise(
    across(
      c("VOT", "f0_Mel", "vowel_duration"),
      list(mean = mean, sd = sd, var = var))) %>%
  group_by(speechstyle, category) %>%
  summarise(
    across(
      ends_with(c("_mean", "_sd", "_var")),
      list(mean = mean, sd = sd, var = var)))


d.chodroff_wilson.connected <-
  d.chodroff_wilson %>%
  filter(speechstyle == "connected") %>%
  # mutate(
  #   across(
  #     c("VOT", "f0_Mel", "vowel_duration"),
  #     function(x) apply_ccure(data = ., cue = substitute(x)))) %>%
  ungroup()

d.chodroff_wilson.isolated <-
  d.chodroff_wilson %>%
  filter(speechstyle == "isolated") %>%
  # mutate(
  #   across(
  #     c("VOT", "f0_Mel", "vowel_duration"),
  #     function(x) apply_ccure(data = ., cue = substitute(x)))) %>%
  ungroup()





PREAMBLE_LOADED <- TRUE

