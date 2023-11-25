library(tidyverse)          # keeping things tidy
library(magrittr)           # pipes


chodroff_wilson_isolated_speech <- read_tsv(
  "../data/cueAnalysis_engCVC.txt",
  col_names = c("file", "category","segment_start","segment_end","interval_number_TextGrid","VOT","following_sonorant", "vowel_duration","Word","word_duration","f0_1","f0_2","f0_3","f0_4","f0_5","f0_6","f0_7","f0_8","f0_9","f0_10")) %>%
  filter(
    # exclude words that contain "l" between stop and vowel;
    # exclude anomalous words due to reading error
    following_sonorant != "L",
    !Word %in% c("AGAIN", "xxxGOOT", "POAT0")) %>%
  mutate(
    # mark undefined f0 values as NA and capture first available f0 measure of vowel
    across(c(11:20), function (x) na_if(x, "--undefined--")),
    f0 = pmap(.l = list(f0_1, f0_2, f0_3, f0_4, f0_5, f0_6, f0_7, f0_8, f0_9, f0_10),
              ~ as.numeric(c(..1, ..2, ..3, ..4, ..5, ..6, ..7, ..8, ..9, ..10))),
    f0 = map_dbl(f0, ~ (.x[!is.na(.x)])[1]),
    # convert duration measure to ms
    across(c(word_duration, VOT, vowel_duration), ~ .x * 1000),
    Talker = gsub("(\\.*)_edited$", "\\1", file),
    category = paste0("/", tolower(category), "/")) %>%
  filter(!is.na(f0)) %>% 
  full_join(
    # Read in the talker by gender data
    read_delim(
      file = "../data/engCVC_gender.csv") %>%
      rename(Talker = subj)) %>%
  rename(Vowel = following_sonorant) %>%
  select(Talker, gender, Word, Vowel,  segment_start, segment_end, category, word_duration, VOT, vowel_duration, f0) 

write_csv(chodroff_wilson_isolated_speech, "../data/chodroff_wilson_isolated_speech.csv")