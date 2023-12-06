# This script tidies the Chodroff & Wilson 2008 isolated speech database and combines it with the Mixer 6 connected speech corpus
# Both databases can be downloaded from https://osf.io/k6djr/ 
# For the isolated speech it combines two files: cueAnalysis_engCVC.txt which has the phonetic cue measurements and the list of subjects by sex, engCVC_gender.csv
# The columns f0_1 to f0_10 are f0 measurements taken at intervals of 5ms from vowel onset. 
#------------------------------------------------
library(tidyverse)          # keeping things tidy
library(magrittr)           # pipes


chodroff_wilson_speech_db <- read_tsv(
  "../data/production_raw/cueAnalysis_engCVC.txt",
  col_names = c("file", "stop","start","end","interval_number_TextGrid","vot","following_sonorant", "vdur","syllable","syldur","f0_1","f0_2","f0_3","f0_4","f0_5","f0_6","f0_7","f0_8","f0_9","f0_10")) %>%
  filter(
    # exclude syllables that contain "l" between stop and vowel;
    # exclude anomalous syllables due to reading error
    following_sonorant != "L",
    !syllable %in% c("AGAIN", "xxxGOOT", "POAT0")) %>%
  # mutating the following cryptic column names so that it can be harmonised with
  # the columns of the Mixer6 corpus. This makes it easier to apply the get_ChodroffWilson_data fn
  # to the combined database later.
  mutate(
    # mark undefined f0 values as NA and capture first available f0 measure of vowel
    across(starts_with("f0"), ~ na_if(.x, "--undefined--") %>% as.numeric(.x)), 
    usef0 = pmap(.l = list(f0_1, f0_2, f0_3, f0_4, f0_5, f0_6, f0_7, f0_8, f0_9, f0_10),
                 ~ as.numeric(c(..1, ..2, ..3, ..4, ..5, ..6, ..7, ..8, ..9, ..10))),
    usef0 = map_dbl(usef0, ~ (.x[!is.na(.x)])[1]),
    # convert duration measure to ms
    across(c(syldur, vot, vdur), ~ .x * 1000),
    subj = gsub("(\\.*)_edited$", "\\1", file),
    speechstyle = "isolated") %>%
  full_join(
    # Read in the talker by gender data and mutate the values to match Mixer6
    read_delim(
      file = "../data/production_raw/engCVC_gender.csv")) %>%
  mutate(gender = ifelse(gender == "female", "F", "M")) %>% 
  # rename columns to columns to match Mixer6 columns. 
  # Now that non-vowel sonorants have been excluded, rename following_sonorant column
  rename(word = syllable,
         wdur = syldur,
         vowel = following_sonorant,
         stop_start = start,
         stop_end = end) %>% 
  bind_rows(
    read_csv("../data/production_raw/chodroff_wilson_mixer6_non-missing_vot_cog_f0.csv") %>% 
      mutate(subj = as.character(subj),
             speechstyle = "connected"))

write_csv(chodroff_wilson_speech_db, "../data/chodroff_wilson_speech_db.csv")