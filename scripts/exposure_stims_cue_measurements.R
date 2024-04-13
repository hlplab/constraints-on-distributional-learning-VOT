# This code collates and tidies stimuli measurements from the various annotation files into one dataframe
# make column names for targetted cue values and measured cue values.
# save as a .csv for joining with the experiment data

#------------------------------------------------
library(tidyverse)          # keeping things tidy
library(magrittr)           # pipes

files <- list.files(path = "../materials/stimuli_AE/annotation_files/", pattern = "*_measured.csv")
exposure_stimuli_cue_measurements <-
  bind_rows(map(1:4, ~ read_csv(paste0("../materials/stimuli_AE/annotation_files/", files[.x]), show_col_types = F))) %>%
  mutate(
    Item.Filename = filename,
    Item.Filename = paste0(filename, ".wav"),
    Item.MinimalPair = str_replace(filename, "(.*)_.*_.*$", "\\1"),
    # mark cue columns as targetted cue values and measured cue values
    # both types of columns for VOT will have the same values
    Item.VOT.measured = as.numeric(str_replace(filename, ".*_VOT(.*)_.*$", "\\1")),
    Item.VOT = as.numeric(Item.VOT.measured),
    # mark the f0 values that were targetted during stimuli generation
    Item.f0 = as.numeric(str_replace(filename, ".*_VOT.*_F0(.*)$", "\\1")),
    # take the f0 measured during the first 5ms into vowel
    Item.f0.measured = f0_5ms_into_vowel,
    # mark the actual vowel duration measurements
    Item.VowelDuration.measured = vowel * 1000) %>%
  # filter all minimal pairs to common VOT values and exclude unused minimal pair
  filter(Item.VOT.measured %in% c(-20:125),
         Item.MinimalPair != "dimtim") %>%
  group_by(Item.MinimalPair) %>%
  arrange(Item.VOT.measured, by_group = T) %>%
  summarise(across(everything(), list)) %T>%
  # temporarily store vector of vowel duration
  { .$Item.VowelDuration.measured[[3]] ->> replacement } %>%
  # standardise vowel durations to that of diptip measurements
  # VC segments with sonorant codas are difficult to objectively measure because of coarticulation
  # since the stimuli were created in the same way we assume the vowel lengths to be the same across minimal pairs
  mutate(Item.VowelDuration = map(Item.VowelDuration.measured, ~ c(replacement))) %>%
  unnest_longer(everything()) %>%
  mutate(across(c(Item.VOT.measured, Item.f0), as.numeric)) %>% 
  select(c(1, 10:16))

write_csv(exposure_stimuli_cue_measurements, "../data/exposure_stimuli_cue_measurements.csv")
