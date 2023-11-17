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
library(ggtext)            # make geom textboxes
library(ggnewscale)        # extra colour scale in ggplots
library(kableExtra)        # for formatting tables

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

set.seed(42007)

# Load data ---------------------------------------------------------------------

# Get production data for the prior.
#
# We are reimporting the data because here we are subsetting the Mixer 6 data to
# talkers that have at least 10 instances each of the relevant stops (we're being
# conservative here since we're creating ideal observers based on this data that
# are averaging across talkers, and so we'd like to make sure that each talker-
# specific IO is based on enough data).
d.chodroff_wilson.connected <-
  get_ChodroffWilson_data(
    database_filename = "../data/chodroff_wilson_mixer6_non-missing_vot_cog_f0.csv",
    categories = c("/d/", "/t/"),
    min.n_per_talker_and_category = 10,
    limits.VOT = c(-Inf, Inf),
    limits.f0 = c(-Inf, Inf),
    max.p_for_multimodality = .1) %>%
  # Only use female talkers
  # (TO DO: revisit whether this makes a difference)
  filter(gender == "female") %>%
  # Subsample talkers to talkers that have both categories, and subsample
  # tokens, so as to make the data of each talker balanced with regard to
  # the number of tokens per categories)
  group_by(Talker, category) %>%
  mutate(n.for_category_and_talker = n()) %>%
  group_by(Talker) %>%
  mutate(
    n_min.for_category_and_talker = min(n.for_category_and_talker),
    n_category.for_category_and_talker = n_distinct(category)) %>%
  # Select talkers that have both /d/ and /t/ observations
  filter(n_category.for_category_and_talker == 2) %>%
  group_by(Talker, category) %>%
  sample_n(size = first(n_min.for_category_and_talker)) %>%
  ungroup()

d.chodroff_wilson.connected %<>%
  mutate(
    across(c(Talker, category, gender), factor),
    across(
      c("VOT", "f0", "f0_Mel", "f0_semitones", "vowel_duration"),
      function(x) apply_ccure(data = ., cue = substitute(x))))

message("Imported phonetic data from Chodroff & Wilson (2018). After applying exclusion criteria, this data contains ",
        n_distinct(d.chodroff_wilson.connected$Talker), " talkers")


# Get production data for the prior -- this is from the isolated speech corpus
# this may need tidying up later
d.chodroff_wilson.isolated <-
  read_tsv(
    "../data/cueAnalysis_engCVC.txt",
    col_names = c("file", "category","segment_start","segment_end","interval_number_TextGrid","VOT","following_sonorant", "vowel_duration","Word","Word_duration","f0_1","f0_2","f0_3","f0_4","f0_5","f0_6","f0_7","f0_8","f0_9","f0_10")) %>%
  filter(
    following_sonorant != "L",
    !Word %in% c("AGAIN", "xxxGOOT", "POAT0"),
    category %in% c("D", "T")) %>%
  mutate(
    across(c(11:20), function (x) na_if(x, "--undefined--")),
    f0 = pmap(.l = list(f0_1, f0_2, f0_3, f0_4, f0_5, f0_6, f0_7, f0_8, f0_9, f0_10),
              ~ as.numeric(c(..1, ..2, ..3, ..4, ..5, ..6, ..7, ..8, ..9, ..10))),
    f0 = map_dbl(f0, ~ (.x[!is.na(.x)])[1]),
    f0_Mel = normMel(f0),
    across(c(Word_duration, VOT, vowel_duration), ~ .x * 1000),
    Talker = gsub("(\\.*)_edited$", "\\1", file),
    category = paste0("/", tolower(category), "/")) %>%
  left_join(
    # read in the talker by gender data
    read_delim(
      file = "../data/engCVC_gender.csv") %>%
      rename(Talker = subj) %>%
      filter(!is.na(gender))) %>%
  rename(Vowel = following_sonorant) %>%
  select(Talker, gender, segment_start, segment_end, Word, category, Word_duration, VOT, vowel_duration, f0, f0_Mel) %>%
  na.omit() %>%
  filter(gender == "female", vowel_duration <= 450) %>%
  group_by(Talker, category) %>%
  mutate(n.for_category_and_talker = n()) %>%
  group_by(Talker) %>%
  mutate(
    n_min.for_category_and_talker = min(n.for_category_and_talker),
    n_category.for_category_and_talker = n_distinct(category)) %>%
  # Select talkers that have both /d/ and /t/ observations
  filter(n_category.for_category_and_talker == 2) %>%
  group_by(Talker, category) %>%
  sample_n(size = first(n_min.for_category_and_talker)) %>%
  ungroup()

d.chodroff_wilson.isolated %<>%
  mutate(
    across(c(Talker, category, gender), factor),
    across(
      c("VOT", "f0", "f0_Mel", "vowel_duration"),
      function(x) apply_ccure(data = ., cue = substitute(x))))

d.chodroff_wilson <-
  bind_rows(
    d.chodroff_wilson.connected %>% mutate(speechstyle = "connected"),
    d.chodroff_wilson.isolated %>% mutate(speechstyle = "isolated"))

PREAMBLE_LOADED <- TRUE

