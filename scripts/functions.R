# General functionality ----------------------------------------------
logit_to_prob <- function(model, term, index = 1) {
  paste0(round(plogis(as.numeric(summary(model)$fixed[term, index])) * 100, 1), "%")
}

# function to transform Gelman-scaled values back
descale <- function(x, mean, sd) {
  (x * 2 * sd) + mean
}


# Knitr output formatting --------------------------------------------
percent <- function(x) paste0(round(x * 100, 1), "%")


# Plotting -----------------------------------------------------------
myGplot.defaults = function(
  type = c("paper","poster","slides")[1],
  base_size = if (type == "paper") { 10 } else if (type == "slides") { 32 } else if (type == "poster") { 36 } else { 10 },
  margin=c("t" = 0.6, "r" = 0.5, "b" = 0.5, "l" = 0.3),
  set_theme = T
)
{
  require(ggplot2)

  if (set_theme) {
    theme_set(theme_bw(base_size=base_size))
    theme_update(
      axis.text.x = element_text(size=base_size, vjust=1),
      axis.text.y = element_text(size=base_size, hjust=1, vjust=.5),
      axis.title.x = element_text(size=base_size , vjust=0, hjust=0.5, face = "bold"),
      axis.title.y = element_text(size=base_size, hjust= 0.5, vjust=0.5, face = "bold"),
      strip.text = element_text(size=base_size, color = "white"),
      strip.background = element_rect(fill = "black", color = "black"),
      legend.title = element_text(size=base_size, face = "bold", hjust= 0),
      legend.text = element_text(size=base_size),
      plot.margin = unit(margin, "lines")
    )
  } else {
    return(
      theme(
        axis.text.x = element_text(size=base_size, vjust=1),
        axis.text.y = element_text(size=base_size, hjust=1, vjust=.5),
        axis.title.x = element_text(size=base_size , vjust=0, hjust=0.5, face = "bold"),
        axis.title.y = element_text(size=base_size, hjust= 0.5, vjust=0.5, face = "bold"),
        strip.text = element_text(size=base_size, color = "white"),
        strip.background = element_rect(fill = "black", color = "black"),
        legend.title = element_text(size=base_size, face = "bold", hjust= 0),
        legend.text = element_text(size=base_size),
        plot.margin = unit(margin, "lines")))
  }
}

remove_all_axes <-
  theme(axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank())

remove_axes_titles <-
  theme(axis.title.x = element_blank(),
      axis.title.y = element_blank())

remove_y_title <-
  theme(axis.title.y = element_blank())

remove_x_guides <-
  theme(axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank())

# from https://stackoverflow.com/questions/44283852/merging-expressions-for-plotting
comb_plotmath <- function(...) {
  Reduce(function(x, y) substitute(x ~ y, env = list(x = x, y = y)),
         list(...))
}

plot_exposure_stim_cues <- function(
    data,
    cues = c("VOT", "f0", "VowelDuration"),
    measurement)
{
  data %>%
    filter(measurement == {{ measurement }}) %>%
    ggplot(aes(x = .panel_x, y = .panel_y, group = Item.MinimalPair, color = Item.MinimalPair, fill = Item.MinimalPair)) +
    geom_autopoint(alpha = .1) +
    geom_autodensity(position = "identity", alpha = .4) +
    stat_ellipse(alpha = .3) +
    scale_color_brewer(
      "Minimal Pair", palette = "Set2",
      breaks = c("dilltill", "dintin", "diptip"),
      labels = c("dill-till", "din-tin", "dip-tip"),
      aesthetics = c("color", "fill")) +
    facet_matrix(
      vars(cues),
      layer.lower = c(1, 3), layer.diag = 2, layer.upper = c(1, 3),
      labeller = labeller(
        .rows = c(VOT = "VOT (ms)", f0 = str_c(measurement, "\n f0 (Hz)"), VowelDuration = str_c(measurement, "\n Vowel duration (ms)")),
        .cols = c(VOT = "VOT (ms)", f0 = str_c(measurement, "\n f0 (Hz)"), VowelDuration = str_c(measurement, "\n Vowel duration (ms)")))) +
    theme(legend.position = "top")
}

plot_phoneticdb_cues <- function(
    data
) {
  data %>%
    ggplot(aes(x = .panel_x, y = .panel_y, color = category, fill = category)) +
    geom_autopoint(alpha = .1) +
    geom_autodensity(position = "identity", alpha = .4) +
    stat_ellipse(
      aes(group = interaction(Talker, category)),
      alpha = .1) +
    stat_ellipse() +
    scale_color_manual(
      "Category",
      labels = c("/d/", "/t/"),
      values = colours.category_greyscale,
      aesthetics = c("color", "fill")) +
    facet_matrix(
      vars(c(VOT, f0_Mel, vowel_duration)),
      layer.lower = c(3, 4:5), layer.diag = 2, layer.upper = c(1, 4:5),
      labeller = labeller(
        .rows = c(VOT = "VOT", f0_Mel = "f0 (Mel)", vowel_duration = "Vowel duration"),
        .cols = c(VOT = "VOT (ms)", f0_Mel = "f0 (Mel)", vowel_duration = "Vowel duration (ms)"))) +
    theme(legend.position = "top")
}

# Load data --------------------------------------------------------------
get_ChodroffWilson_data <- function(
    database_filename = "chodroff_wilson_mixer6_non-missing_vot_cog_f0.csv",
    categories = c("/b/", "/d/", "/g/", "/p/", "/t/", "/k/"),
    min.n_per_talker_and_category = 0,
    limits.VOT = c(-Inf, Inf),
    limits.f0 = c(0, Inf),
    max.p_for_multimodality = 1
) {
  require(tidyverse)
  require(magrittr)
  require(diptest)

  # Standardizing variable names and values to conform to what we usually use.
  d <-
    read_csv(database_filename, show_col_types = FALSE) %>%
    rename(
      category = stop, VOT = vot, f0 = usef0, Talker = subj, Word = word, Trial = trial,
      Vowel = vowel, vowel_duration = vdur, word_duration = wdur, speech_rate = spk_rate,
      spectral_M1 = cog, spectral_M2 = spectral.var, spectral_M3 = skew, spectral_M4 = kurtosis) %>%
    mutate(
      category =
        plyr::mapvalues(
          category,
          c("B", "D", "G", "P", "T", "K"),
          c("/b/", "/d/", "/g/", "/p/", "/t/", "/k/")),
      gender = factor(
        plyr::mapvalues(
          gender,
          c("F", "M"),
          c("female", "male")),
        levels = c("male", "female")),
      poa = factor(
        plyr::mapvalues(
          poa,
          c("lab", "cor", "dor"),
          c("/b/-/p/", "/d/-/t/", "/g/-/k/")),
        levels = c("/b/-/p/", "/d/-/t/", "/g/-/k/")),
      voicing = factor(
        ifelse(category %in% c("/b/", "/d/", "/g/"), "yes", "no"),
        levels = c("yes", "no"))) %>%
    mutate(across(c(Talker, Word, gender, category), factor)) %>%
    dplyr::select(
      Talker, Word, Trial, Vowel, gender, category, poa, voicing, VOT, f0,
      spectral_M1, spectral_M2, spectral_M3, spectral_M4, vowel_duration,
      word_duration, speech_rate, speechstyle)

  d %<>%
    # Filter VOT and f0 for absolute values to deal with outliers
    filter(
      between(VOT, min(limits.VOT), max(limits.VOT)),
      between(f0, min(limits.f0), max(limits.f0))) %>%
    # Filter to requested categories
    filter(category %in% categories) %>%
    droplevels()

  # Identify and remove talkers with bimodal f0 distributions
  # (indicating pitch halving/doubling)
  suppressWarnings(
    d %<>%
      group_by(Talker) %>%
      mutate(f0_Mel = phonR::normMel(f0)) %>%
      group_by(Talker, category) %>%
      mutate(
        f0_Mel.multimodal = dip.test(f0_Mel)$p.value < max.p_for_multimodality) %>%
      filter(!f0_Mel.multimodal) %>%
      droplevels())

  # Keep only talkers with at least n.min observations for each stop
  d %<>%
    group_by(Talker, category) %>%
    mutate(n_per_category = length(category)) %>%
    group_by(Talker) %>%
    mutate(n_per_category = ifelse(any(is.na(n_per_category)), 0, min(n_per_category))) %>%
    ungroup() %>%
    filter(n_per_category > min.n_per_talker_and_category)

  # Get Mel and Semitones
  d %<>%
    group_by(Talker) %>%
    mutate(f0_semitones = 12 * log(f0 / mean(f0)) / log(2)) %>%
    ungroup()

  return(d)
}

# Prepare variables for regression modelling ---------------------------------------------------

prepVars <- function(
    d,
    test_mean = NULL,
    levels.Condition = NULL
) {
  d %<>%
    drop_na(Condition.Exposure, Phase, Block, Item.MinimalPair, ParticipantID, Item.VOT, Response)

  message("VOT mean:", signif(mean(d$Item.VOT, na.rm = T)))
  message("VOT sd:", signif(sd(d$Item.VOT, na.rm = T)))
  message(paste("VOT test mean:", test_mean))

  d %<>%
    ungroup() %>%
    mutate(
      Block_n = as.numeric(as.character(Block)),
      across(c(Condition.Exposure, Block, Item.MinimalPair), factor),
      Condition.Exposure = factor(Condition.Exposure, levels = levels.Condition)) %>%
    drop_na(Block, Response, Item.VOT) %>%
    mutate(VOT_gs = (Item.VOT - test_mean) / (2 * sd(Item.VOT, na.rm = TRUE))) %>%
    droplevels()

  contrasts(d$Condition.Exposure) <- cbind("_Shift10 vs. Shift0" = c(-2/3, 1/3, 1/3),
                                           "_Shift40 vs. Shift10" = c(-1/3,-1/3, 2/3))
  message(contrasts(d$Condition.Exposure))

  if (all(d$Phase == "test") & n_distinct(d$Block) > 1) {
    contrasts(d$Block) <- MASS::fractions(MASS::contr.sdif(6))
    dimnames(contrasts(d$Block))[[2]] <- c("_Test2 vs. Test1", "_Test3 vs. Test2", "_Test4 vs. Test3", "_Test5 vs. Test4", "_Test6 vs. Test5")
    message("Condition contrast is:", contrasts(d$Condition.Exposure))
    message("Block contrast is:", contrasts(d$Block))
  } else if (all(d$Phase == "exposure") & n_distinct(d$Block) > 1) {
    contrasts(d$Block) <- cbind("_Exposure2 vs. Exposure1" = c(-2/3, 1/3, 1/3),
                                "_Exposure3 vs. Exposure2" = c(-1/3,-1/3, 2/3))
    message("Condition contrast is:", MASS::fractions(contrasts(d$Condition.Exposure)))
    message("Block contrast is:", MASS::fractions(contrasts(d$Block)))
  } else if (n_distinct(d$Block) > 1) {
    contrasts(d$Block) <- MASS::fractions(MASS::contr.sdif(9))
    dimnames(contrasts(d$Block))[[2]] <- c("_Exp1 vs. Test1", "_Test2 vs. Exp1", "_Exp2 vs. Test2", "_Test3 vs. Exp2", "_Exp3 vs. Test3", "_Test4 vs. Exp3", "_Test5 vs. Test4", "_Test6 vs. Test5")
    message("Condition contrast is:", MASS::fractions(contrasts(d$Condition.Exposure)))
    message("Block contrast is:", MASS::fractions(contrasts(d$Block)))
  }
  return(d)
}


# Fit Bayesian model in standard and nested slope formulations---------------------------------------------------
# priorSD argument refers to the SD for the VOT estimate
fit_model <- function(
    data,
    phase,
    formulation = "standard",
    priorSD = 2.5,
    iter = 4000,
    warmup = 2000,
    adapt_delta = .99
) {
  require(tidyverse)
  require(magrittr)
  require(brms)

  # get the mean VOT at test for centering
  # in both test and exposure fitting we center to the mean during test phase
  VOT.mean_test <-
    data %>%
    filter(Phase == "test") %>%
    ungroup() %>%
    summarise(mean = mean(Item.VOT, na.rm = T)) %>%
    pull(mean)

  levels_Condition.Exposure <- c("Shift0", "Shift10", "Shift40")

  if (phase == "all") {
    data %<>%
      filter(Item.Labeled == F) %>%
      prepVars(test_mean = VOT.mean_test, levels.Condition = levels_Condition.Exposure)
  } else {
    data %<>%
      filter(Phase == phase & Item.Labeled == F) %>%
      prepVars(test_mean = VOT.mean_test, levels.Condition = levels_Condition.Exposure)
  }

  # specify the prior for beta parameters here if different from the general one
  prior_overwrite <- if (phase == "exposure" & formulation == "nested_within_condition_and_block") {
    c(set_prior(paste0("student_t(3, 0, ", priorSD, ")"), coef = "IpasteCondition.ExposureBlocksepEQxShift0x2:VOT_gs", dpar = "mu2"),
      set_prior(paste0("student_t(3, 0, ", priorSD, ")"), coef = "IpasteCondition.ExposureBlocksepEQxShift0x4:VOT_gs", dpar = "mu2"),
      set_prior(paste0("student_t(3, 0, ", priorSD, ")"), coef = "IpasteCondition.ExposureBlocksepEQxShift0x6:VOT_gs", dpar = "mu2"),
      set_prior(paste0("student_t(3, 0, ", priorSD, ")"), coef = "IpasteCondition.ExposureBlocksepEQxShift10x2:VOT_gs", dpar = "mu2"),
      set_prior(paste0("student_t(3, 0, ", priorSD, ")"), coef = "IpasteCondition.ExposureBlocksepEQxShift10x4:VOT_gs", dpar = "mu2"),
      set_prior(paste0("student_t(3, 0, ", priorSD, ")"), coef = "IpasteCondition.ExposureBlocksepEQxShift10x6:VOT_gs", dpar = "mu2"),
      set_prior(paste0("student_t(3, 0, ", priorSD, ")"), coef = "IpasteCondition.ExposureBlocksepEQxShift40x2:VOT_gs", dpar = "mu2"),
      set_prior(paste0("student_t(3, 0, ", priorSD, ")"), coef = "IpasteCondition.ExposureBlocksepEQxShift40x4:VOT_gs", dpar = "mu2"),
      set_prior(paste0("student_t(3, 0, ", priorSD, ")"), coef = "IpasteCondition.ExposureBlocksepEQxShift40x6:VOT_gs", dpar = "mu2"))
  } else if (phase == "test" & formulation == "nested_within_condition_and_block") {
    c(set_prior(paste0("student_t(3, 0, ", priorSD, ")"), coef = "IpasteCondition.ExposureBlocksepEQxShift0x1:VOT_gs", dpar = "mu2"),
      set_prior(paste0("student_t(3, 0, ", priorSD, ")"), coef = "IpasteCondition.ExposureBlocksepEQxShift0x3:VOT_gs", dpar = "mu2"),
      set_prior(paste0("student_t(3, 0, ", priorSD, ")"), coef = "IpasteCondition.ExposureBlocksepEQxShift0x5:VOT_gs", dpar = "mu2"),
      set_prior(paste0("student_t(3, 0, ", priorSD, ")"), coef = "IpasteCondition.ExposureBlocksepEQxShift0x7:VOT_gs", dpar = "mu2"),
      set_prior(paste0("student_t(3, 0, ", priorSD, ")"), coef = "IpasteCondition.ExposureBlocksepEQxShift0x8:VOT_gs", dpar = "mu2"),
      set_prior(paste0("student_t(3, 0, ", priorSD, ")"), coef = "IpasteCondition.ExposureBlocksepEQxShift0x9:VOT_gs", dpar = "mu2"),
      set_prior(paste0("student_t(3, 0, ", priorSD, ")"), coef = "IpasteCondition.ExposureBlocksepEQxShift10x1:VOT_gs", dpar = "mu2"),
      set_prior(paste0("student_t(3, 0, ", priorSD, ")"), coef = "IpasteCondition.ExposureBlocksepEQxShift10x3:VOT_gs", dpar = "mu2"),
      set_prior(paste0("student_t(3, 0, ", priorSD, ")"), coef = "IpasteCondition.ExposureBlocksepEQxShift10x5:VOT_gs", dpar = "mu2"),
      set_prior(paste0("student_t(3, 0, ", priorSD, ")"), coef = "IpasteCondition.ExposureBlocksepEQxShift10x7:VOT_gs", dpar = "mu2"),
      set_prior(paste0("student_t(3, 0, ", priorSD, ")"), coef = "IpasteCondition.ExposureBlocksepEQxShift10x8:VOT_gs", dpar = "mu2"),
      set_prior(paste0("student_t(3, 0, ", priorSD, ")"), coef = "IpasteCondition.ExposureBlocksepEQxShift10x9:VOT_gs", dpar = "mu2"),
      set_prior(paste0("student_t(3, 0, ", priorSD, ")"), coef = "IpasteCondition.ExposureBlocksepEQxShift40x1:VOT_gs", dpar = "mu2"),
      set_prior(paste0("student_t(3, 0, ", priorSD, ")"), coef = "IpasteCondition.ExposureBlocksepEQxShift40x3:VOT_gs", dpar = "mu2"),
      set_prior(paste0("student_t(3, 0, ", priorSD, ")"), coef = "IpasteCondition.ExposureBlocksepEQxShift40x5:VOT_gs", dpar = "mu2"),
      set_prior(paste0("student_t(3, 0, ", priorSD, ")"), coef = "IpasteCondition.ExposureBlocksepEQxShift40x7:VOT_gs", dpar = "mu2"),
      set_prior(paste0("student_t(3, 0, ", priorSD, ")"), coef = "IpasteCondition.ExposureBlocksepEQxShift40x8:VOT_gs", dpar = "mu2"),
      set_prior(paste0("student_t(3, 0, ", priorSD, ")"), coef = "IpasteCondition.ExposureBlocksepEQxShift40x9:VOT_gs", dpar = "mu2"))
  } else {
    c(set_prior(paste0("student_t(3, 0, ", priorSD, ")"), coef = "VOT_gs", dpar = "mu2"))
  }

  my_priors <-
    c(
      prior(student_t(3, 0, 2.5), class = "b", dpar = "mu2"),
      prior_overwrite,
      prior(cauchy(0, 2.5), class = "sd", dpar = "mu2"),
      prior(lkj(1), class = "cor"))

  brm(
    formula = if (formulation == "nested_within_condition_and_block") {
      bf(Response.Voiceless ~ 1,
         mu1 ~ 0 + offset(0),
         mu2 ~ 0 + I(paste(Condition.Exposure, Block, sep = "x")) / VOT_gs +
           (0 + Block / VOT_gs | ParticipantID) +
           (0 + I(paste(Condition.Exposure, Block, sep = "x")) / VOT_gs | Item.MinimalPair),
         theta1 ~ 1)
    } else if (formulation == "lapse_block") {
      bf(Response.Voiceless ~ 1,
         mu1 ~ 0 + offset(0),
         mu2 ~ 1 + VOT_gs * Condition.Exposure * Block + (1 + VOT_gs * Block | ParticipantID) + (1 + VOT_gs * Condition.Exposure * Block | Item.MinimalPair),
         theta1 ~ 1 + Block)}
      else {
      bf(Response.Voiceless ~ 1,
         mu1 ~ 0 + offset(0),
         mu2 ~ 1 + VOT_gs * Condition.Exposure * Block + (1 + VOT_gs * Block | ParticipantID) + (1 + VOT_gs * Condition.Exposure * Block | Item.MinimalPair),
         theta1 ~ 1)},
    data = data,
    prior = my_priors,
    cores = 4,
    chains = 4,
    init = 0,
    iter = iter,
    warmup = warmup,
    family = mixture(bernoulli("logit"), bernoulli("logit"), order = F),
    control = list(adapt_delta = adapt_delta),
    file = paste0("../models/", phase, "-", formulation, "-priorSD", priorSD, "-", adapt_delta, ".rds")
  )
}


# Get info from psychometric model -----------------------------------------------------------
get_intercepts_and_slopes <-
  . %>%
  # We are *not* including group-level variability here since this pipe is meant to
  # capture variability in the population-level parameter estimates.
  gather_draws(`b_mu2_IpasteCondition.ExposureBlocksepEQ.*`, regex = TRUE, ndraws = 8000) %>%
  mutate(
    .variable = gsub("b_mu2_IpasteCondition.ExposureBlocksepEQxShift(\\d{1,2})x(\\d{1}.*$)", "Shift\\1.\\2", .variable),
    term = ifelse(str_detect(.variable, "VOT_gs"), "slope", "Intercept")) %>%
  separate(col = .variable, into = c("Condition.Exposure", "Block"), sep = "\\.") %>%
  mutate(Block = ifelse(str_detect(Block, "VOT"), str_replace(Block, "(\\d{1}):VOT_gs", "\\1"), Block)) %>%
  pivot_wider(names_from = term, values_from = ".value") %>%
  relocate(c(Condition.Exposure, Block, Intercept, slope, .chain, .iteration, .draw))

# function to calculate proportion of change in PSE by Condition and draw
get_prop_shift_by_draw <- function(data) {
  # store the PSE for block 1 of the draw
  PSE_scaled_block1 <- data %>% filter(Block == 1) %>% pull(PSE_scaled)
  PSE_unscaled_block1 <- data %>% filter(Block == 1) %>% pull(PSE_unscaled)
  # the true shift is the distance between the ideal PSE for that condition and the actual PSE before exposure
  true_shift_scaled <- (data %>% filter(Block == 1) %>% pull(predictedPSE_scaled))- PSE_scaled_block1
  true_shift_unscaled <- (data %>% filter(Block == 1) %>% pull(predictedPSE_unscaled))- PSE_unscaled_block1
  
  data %>% 
    mutate(PSE_scaled.change = PSE_scaled - PSE_scaled_block1,
           prop.shift_scaled = PSE_scaled.change/true_shift_scaled,
           PSE_unscaled.change = PSE_unscaled - PSE_unscaled_block1,
           prop.shift_unscaled = PSE_unscaled.change/true_shift_unscaled)
}  


get_conditional_effects <- function(model, data, phase) {
  conditional_effects(
    x = model,
    effects = "VOT_gs:Condition.Exposure",
    conditions = make_conditions(
      data %>%
        filter(Phase == .env$phase & Item.Labeled == FALSE) %>%
        prepVars(test_mean = VOT.mean_test, levels.Condition = levels_Condition.Exposure),
      vars = c("Block")),
    method = "posterior_epred",
    ndraws = 500,
    re_formula = NA)
}

get_lapse_hypothesis <- function(model, contrast_row = 1) {
  paste(
    "plogis(theta1_Intercept +",
    paste(
      paste(
        unname(attr(model$data$Block, "contrasts")[contrast_row, 1:7]),
        "*",
        rownames(fixef(model))[56:62], "+", collapse = " "),
      paste(
        unname(attr(model$data$Block, "contrasts")[contrast_row, 8]),
        "*",
        rownames(fixef(model))[63])),
    ") > 0")
}

get_nsamples <- function(model) {
  n.posterior_samples <-
    ((map(model$fit@stan_args, ~ .x$iter) %>% reduce(`+`)) -
       (map(model$fit@stan_args, ~ .x$warmup) %>% reduce(`+`))) /
    (first(map(model$fit@stan_args, ~ .x$thin)))

  return(n.posterior_samples)
}


get_bf <- function(model, hypothesis, est = F, bf = F) {
  h <- hypothesis(model, hypothesis)[[1]]
  BF <- if (is.infinite(h$Evid.Ratio)) paste("\\geq", get_nsamples(model)) else paste("=", round(h$Evid.Ratio, 1))
  if (est == T & bf == T) {  str_c("Est. = ", round(h[[2]], 2), "; BF ", BF) }
  else if (est) { h[[2]] }
  else if (bf) { round(hypothesis(model, hypothesis)[[1]][[6]], 1) }
  else {
    paste0(
    "\\(\\hat{\\beta} = ", round(h$Estimate, 2),
    "\\), 90\\%-CI = \\([", round(h$CI.Lower, 3), ", ", round(h$CI.Upper, 3),
    "]\\), \\(BF ", BF,
    "\\), \\(p_{posterior} = \\) \\(", signif(h$Post.Prob, 3), "\\)") }
}

# Function to get identity CI of a model summary
get_CI <- function(model, term, hypothesis) {

  paste0(round(as.numeric(summary(model)$fixed[term, 1]), 1), " 95%-CI: ",
         paste(round(as.numeric(summary(model)$fixed[term, 3:4]), 1), collapse = " to "),
         "; ",
         get_bf(model = model, hypothesis = hypothesis))
}

print_CI <- function(model, term) {

  paste0(round(plogis(as.numeric(summary(model)$fixed[term, 1])) * 100, 1),
         "%, 95%-CI: ",
         paste0(round(plogis(as.numeric(summary(model)$fixed[term, 3:4])) * 100, 1), collapse = " to "), "%")
}

# Pipes and functions for plot and table aesthetics -----------------------------------------------------------
add_block_labels <-
  . %>%
  mutate(
    Block.plot_label = factor(case_when(
      Block == 1 ~ "Test 1",
      Block == 3 ~ "Test 2",
      Block == 5 ~ "Test 3",
      Block == 7 ~ "Test 4",
      Block == 8 ~ "Test 5",
      Block == 9 ~ "Test 6",
      Block == 2 ~ "Exposure 1",
      Block == 4 ~ "Exposure 2",
      Block == 6 ~ "Exposure 3")),
    Block.plot_label = fct_relevel(
      Block.plot_label,
      c("Test 1", "Exposure 1", "Test 2", "Exposure 2", "Test 3", "Exposure 3",  "Test 4", "Test 5", "Test 6")))


geom_linefit <- function(
    data,
    x,
    y,
    fill,
    legend.position,
    legend.justification = NULL
    ) {
  list(
    geom_ribbon(aes(x = {{ x }}, y = {{ y }}, group = Condition.Exposure,
                    ymin = lower__, ymax = upper__, fill = Condition.Exposure), alpha = .1),
    geom_line(aes(
      x = {{ x }}, y = {{ y }}, group = Condition.Exposure, colour = Condition.Exposure), linewidth = .7, alpha = 0.6),
    geom_rug(data = data %>% distinct(Item.VOT), aes(x = {{ x }}), alpha = 0.5, colour = "grey", inherit.aes = F),
    stat_summary(
      data = data, fun.data = mean_cl_boot, mapping = aes(x = {{ x }}, y = Response.Voiceless, colour = Condition.Exposure),
      geom = "pointrange", size = 0.1, alpha = 0.7, position = position_dodge2(width = 2), inherit.aes = F),
    labs(x = "VOT (ms)", y = "Proportion \"t\"-responses"),
    scale_color_manual(
      "Condition",
      labels = c("baseline", "+10ms", "+40ms"),
      values = colours.condition,
      aesthetics = c("color", "fill")),
    theme(
      legend.position = legend.position,
      legend.justification = legend.justification,
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.background = element_rect(fill = fill),
      strip.text.x = element_text(colour = "black")),
    facet_grid(~ Block.plot_label, scales = "free_x", space = "free_x")
  )
}

### function for Formatting hypothesis tables
align_tab <- function(hyp) {

  map_chr(hyp, ~ ifelse(class(.x) == "numeric", "r","l"))
}

make_hyp_table <- function(model = NULL, hypothesis, hypothesis_names, caption, col1_width = "15em", digits = 2) {

  bind_cols(tibble(Hypothesis = hypothesis_names), hypothesis) %>%
    dplyr::select(-2) %>%
    mutate(
      across(
        c(Estimate, Est.Error, CI.Lower, CI.Upper),
        ~ round(., digits = digits)),
      across(
        c(Post.Prob),
        ~ round(., digits = 3)),
      Evid.Ratio = ifelse((is.infinite(Evid.Ratio)), paste("$\\geq", get_nsamples(model), "$"), round(Evid.Ratio, digits = 1)),
      CI = paste0("[", CI.Lower, ", ", CI.Upper, "]")) %>%
    dplyr::select(-c(CI.Upper, CI.Lower)) %>%
    relocate(CI, .before = "Evid.Ratio") %>%
    kbl(caption = caption, align = align_tab(hypothesis),
        format = "latex",
        booktabs = TRUE,
        escape = FALSE,
        col.names = c("Hypothesis", "Est.", "SE", "90\\%-CI", "BF", "$p_{post}$")) %>%
    # HOLD_position for latex table placement H and hold_position for latex h!, neither if placement is left to latex
    kable_styling(latex_options = "hold_position", full_width = FALSE) %>%
    column_spec(1, width = col1_width)
}


# NORMALIZATION -----------------------------------------------------------

# if there is newdata return the newdata scaled by the old data
# if there is no newdata return data scaled by itself
prep_predictors_for_CCuRE <- function(data, newdata = NULL){
  # if(!is.null(newdata)) {
  #   mean.vowel_duration = mean(data$vowel_duration)
  #   sd.vowel_duration = sd(data$vowel_duration)
  #
  #   newdata %>%
  #     mutate(vowel_duration = (vowel_duration - mean.vowel_duration) / sd.vowel_duration)
  # } else {
  #   data %>%
  #     ungroup() %>%
  #     mutate(across(vowel_duration, ~ (.x - mean(.x)) / sd(.x)))
  # }
  if(!is.null(newdata)) {
    newdata
  } else {
    data
  }
}


get_CCuRE_model <- function(data, cue = "VOT") {
  if (length(unique(data$Talker)) > 1)
    lme4::lmer(formula(paste(cue, "~ 1 + (1 | Talker)")), data = data) else
      lm(formula(paste(cue, "~ 1")), data = data)
}

# Normalize cue in newdata based on cue in data
apply_ccure <- function(data, newdata = NULL, cue) {
  m.to_normalize_from <-
    get_CCuRE_model(
      data = data,
      cue = cue)

  allow_new_levels <- !is.null(newdata)
  # If no newdata was provided, use the data as newdata
  if (is.null(newdata)) newdata <- data else if (!("Talker" %in% names(newdata))) newdata$Talker <- "CONSTANT"
  m.to_normalize <-
      get_CCuRE_model(
        data = newdata,
        cue = cue)

  # Remove expected cue value of newdata and then add intercept of data
  newdata[[cue]] -
    predict(m.to_normalize, newdata = newdata, allow.new.levels = allow_new_levels) +
    fixef(m.to_normalize_from)[1]
}

# if there is newdata CCuRE the newdata by the old data and return newdata
# if there is no newdata CCuRE data based on its own statistics and return data
remove_speechrate_effect_from_cue <- function(data, newdata = NULL, cue = "VOT"){
  m <- get_CCuRE_model(
    data = data %>% mutate(current_outcome = !! sym(cue)),
    cue = "current_outcome",
    tidy_result = F)

  # If newdata isn't null, convert predictors of C-CuRE model into the space used
  # in the C-CuRE model fitted above.
  prep_predictors_for_CCuRE(data = data, newdata = newdata) %>%
    mutate(
      # Get the residual value for the cue given the C-CuRE model (while, for now
      # ignoring that the new data comes from different talker; that's what is made
      # possible by allow.new.levels = T)
      residual_current_cue = !! sym(cue) - predict(m, newdata = ., allow.new.levels = TRUE),
      # Now add the predicted mean of the original data back to the cue. Note that
      # this speech rate corrected cue has *not* yet been corrected for the mean of
      # the new talker. (for that one would have to also subtract out the difference
      # between the overall cue mean in the original data and the cue mean in the new
      # data)
      ccure_current_cue = residual_current_cue + fixef(m)[1]) %>%
    pull(ccure_current_cue)
}

center_stimuli <- function(d, database) {
  d %>% mutate(
    across(
      c("VOT", "f0_Mel", "vowel_duration"),
      function(x) apply_ccure(
        data = database,
        newdata = .,
        cue = substitute(x)),
      .names = "{.col}.CCuRE"))
}

point_overlay <- function(
    d,
    database,
    center = F
) {
  geom_autopoint(
    data =  if (center) { d %>%
        center_stimuli(database = database) %>%
        select(!c(VOT, f0_Mel, vowel_duration)) %>%
        rename(VOT = VOT.CCuRE, f0_Mel = f0_Mel.CCuRE, vowel_duration = vowel_duration.CCuRE) %>%
        filter(Phase == "test") %>% distinct(VOT, f0_Mel, vowel_duration) %>%
        mutate(category = "test") } else {
          d %>%
            filter(Phase == "test") %>%
            distinct(VOT, f0_Mel, vowel_duration) %>%
            mutate(category = "test") },
    color = "black", alpha = .5, size = 1, inherit.aes = F)
}

# MAKE IDEAL OBSERVERS ----------------------------------------------------

## Make IO out of MVG
make_stop_VOTf0_ideal_observer <- function(
    m,
    prior = rep(1 / nrow(m), nrow(m)),
    lapse_rate = 0,
    lapse_bias = rep(1 / nrow(m), nrow(m)),
    Sigma_noise = matrix(c(80, 0, 0, 878), ncol = 2, dimnames = list(names(first(m$mu)), names(first(m$mu))))
) {
  message("By default, using noise estimates from Kronrod et al. (2016). Mel noise estimates are taken from their vowel studies.")
  m %>%
    lift_MVG_to_MVG_ideal_observer(
      Sigma_noise = Sigma_noise,
      prior = prior,
      lapse_rate = lapse_rate,
      lapse_bias = lapse_bias)
}

## Capturing prior beliefs of perceiver
make_stop_VOTf0_ideal_adaptor <- function(m, kappa = 3, nu = 3) {
  assert_that(all(nu >= 3))

  m %>%
    rename(
      m = mu,
      S = Sigma) %>%
    mutate(
      kappa = kappa,
      nu = nu,
      S = get_S_from_expected_Sigma(S, nu))
}


get_bivariate_normal_ellipse <- function(
    mu = c(0, 0),
    Sigma = diag(2),
    level = .95,
    segments = 51,
    varnames = c("VOT", "F0")
) {
  # Ths function is based on calculate_ellipse from ggplot2, with modification to
  # remove uncertainty about Sigma (since we're plotting the theoretical distribution,
  # for which Sigma is known)
  require(tidyverse)

  chol_decomp <- chol(Sigma)
  # Adapted from https://stats.stackexchange.com/questions/64680/how-to-determine-quantiles-isolines-of-a-multivariate-normal-distribution
  # (tested)
  radius <- sqrt(-2 * log(1 - level))

  # Make n + 1 point over unit circle
  angles <- (0:segments) * 2 * pi/segments
  unit.circle <- cbind(cos(angles), sin(angles))

  # Shape unit circle by covariance, scale by radius, and move it to mu
  # (the t() calls are necessary since we allow mu to be a vector, so we
  # need to transform the 2-col x segements-rows matrix into a segments-col
  # x 2-row matrix, and then---after adding mu---transforming the whole
  # thing back into a 2-col x segements-rows matrix)
  ellipse <- as.data.frame(t(mu + radius * t(unit.circle %*% chol_decomp)))
  names(ellipse) <- varnames

  return(ellipse)
}

# LINK IDEAL OBSERVERS TO PERCEPTION EXPERIMENT ---------------------------

# Estimate the intercept, slope, and PSE that these ideal observers would have
# were they analyzed the same way the human data is analyzed.

fit_logistic_regression_to_model_categorization <- function(.data, resolution = 10^12, groups = NULL) {
  if ("io" %in% names(.data)) {
    # Prepare IO model predictions data frame
    .data %<>%
    pivot_wider(names_from = category, values_from = response, names_prefix = "response_") %>%
      mutate(n_d = round(`response_/d/` * .env$resolution), n_t = .env$resolution - n_d) %>%
      group_by(!!! syms(groups)) %>%
      nest()
  } else {
    .data %<>%
      # Prepare IA model predictions data frame
      mutate(
        n_d = round((1 - Predicted_posterior) * .env$resolution),
        n_t = .env$resolution - n_d) %>%
      group_by(group, .chain, .iteration, .draw) %>%
      nest()
  }
  .data %>%
    mutate(
    model_unscaled = map(data, ~ glm(
      cbind(n_t, n_d) ~ 1 + VOT,
      family = binomial,
      data = .x)),
    intercept_unscaled = map_dbl(model_unscaled, ~ tidy(.x)[1, 2] %>% pull()),
    slope_unscaled = map_dbl(model_unscaled, ~ tidy(.x)[2, 2] %>% pull()),
    model_scaled = map(data, ~ glm(
      cbind(n_t, n_d) ~ 1 + I((VOT - VOT.mean_test) / (2* VOT.sd_test)),
      family = binomial,
      data = .x)),
    intercept_scaled = map_dbl(model_scaled, ~ tidy(.x)[1, 2] %>% pull()),
    slope_scaled = map_dbl(model_scaled, ~ tidy(.x)[2, 2] %>% pull()),
    PSE = -intercept_unscaled/slope_unscaled) %>%
    # for IA model predictions: collapse over all Latin-square designed lists
    # (this still keeps all individual predictions but only has one unique combination
    # of exposure condition and test
    { if ("group" %in% names(.)) mutate(., group = gsub("[ABC]A", "", group)) else (.) }
}


get_logistic_parameters_from_model <- function(
    model,
    model_col = "model",
    groups = NULL
) {
  f <-
    if (any(map_lgl(model[[model_col]], is.MVG_ideal_observer)))
      get_categorization_from_MVG_ideal_observer else
        if (any(map_lgl(model[[model_col]], is.NIW_ideal_adaptor)))
          get_categorization_from_NIW_ideal_adaptor else stop("Model type not recognized.")

  model %>%
    mutate(
      categorization =
        map2(x, !! sym(model_col),
             ~ f(
               x = .x$x, model = .y, decision_rule = "proportional") %>%
               mutate(VOT = map_dbl(x, ~ .x[1])))) %>%
    unnest(cols = categorization, names_repair = "unique") %>%
    # Fit logistic regression and extract relevant information
    # (the regression only uses VOT regardless of what cues are used for the categorization
    # so that this matches the analysis of the human responses)
    fit_logistic_regression_to_model_categorization(groups = groups)
}

prep_data_for_IBBU_prediction <- function(
    model,
    data = NULL,
    untransform_cues = T,
    prep_test = T
) {
  cue.labels <- get_cue_levels_from_stanfit(model)

  if (prep_test) {
  get_test_data_from_stanfit(model) %>%
    distinct(!!! syms(cue.labels)) %>%
    { if (untransform_cues) get_untransform_function_from_stanfit(model)(.) else . } %>%
    make_vector_column(cols = cue.labels, vector_col = "x", .keep = "all") %>%
    nest(cues_joint = x, cues_separate = .env$cue.labels) %>%
    expand_grid(group = get_group_levels_from_stanfit(model))
  } else {
    # Prepare exposure_data
    data %>%
      filter(Phase == "exposure") %>%
      group_by(Condition.Exposure) %>%
      # get 1 set of exposure trials per condition, per list
      filter(ParticipantID == first(ParticipantID)) %>%
      reframe(Item.VOT, Item.f0_Mel, vowel_duration, category) %>%
      rename(VOT = Item.VOT, f0_Mel = Item.f0_Mel) %>%
      group_by(Condition.Exposure) %>%
      expand_grid(TestBlock = 1:4, LSD = c("A", "B", "C")) %>%
      mutate(group = factor(ifelse(TestBlock == 1, "no exposure", paste0("Cond ", Condition.Exposure, LSD, "A", "_Up to test", (TestBlock - 1) * 2 + 1)))) %>%
      make_vector_column(cols = cue.labels, vector_col = "x", .keep = "all") %>%
      nest(cues_joint = x, cues_separate = c(.env$cue.labels, category))
  }
}

get_IBBU_predicted_response <- function(
    model,
    data,
    groups = NULL,
    untransform_cues = T,
    target_category = 2, # target category "/d/" = 1, "/t/" = 2
    predict_test = T,
    seed = 583,
    ndraws = 1000
) {
  # Get and summarize posterior draws from fitted model
  d.pars <-
    add_ibbu_stanfit_draws(
      model,
      groups = groups,
      summarize = F,
      wide = F,
      seed = seed,
      ndraws = ndraws,
      untransform_cues = untransform_cues) %>%
    filter(group %in% .env$groups)


  # Categorize data
  d.pars %<>%
    group_by(group, .chain, .iteration, .draw) %>%
    do(f = get_categorization_function_from_grouped_ibbu_stanfit_draws(., logit = F)) %>%
    right_join(data, by = "group") %>%
    group_by(group, .chain, .iteration, .draw) %>%
    mutate(
      Predicted_posterior =
        pmap(
          .l = list(f, cues_joint, target_category),
          .f = ~ exec(..1, x = ..2$x, target_category = target_category))) %>%
    select(-f) %>%
    unnest(c(cues_joint, cues_separate, Predicted_posterior)) %>%
    # Repair estimates that yield infinite posteriors
    mutate(
      Predicted_posterior =
        case_when(
          is.infinite(Predicted_posterior) & sign(Predicted_posterior) == 1 ~ 1,
          is.infinite(Predicted_posterior) & sign(Predicted_posterior) == -1 ~ 0,
          T ~ Predicted_posterior)) %>%
    # for posterior predictions of exposure stimuli, get categorisations based on proportion and criterion decision rules
    { if (predict_test) . else mutate(
      .,
      Response.Proportion = ifelse(category == "/t/", Predicted_posterior, 1 - Predicted_posterior),
      Response.Criterion = ifelse(Predicted_posterior >= .5, "/t/","/d/")
      ) }
}

get_IO_predicted_PSE <- function(
    condition, 
    block = 7, 
    io.intercept.slope.PSE = d.IO_intercept.slope.PSE,
    mean = F
    ) {
  if (!("Block" %in% names(io.intercept.slope.PSE)))
    io.intercept.slope.PSE %<>% crossing(Block = 1:9)
  
  if (condition == "prior") {
    io.intercept.slope.PSE %>%
      select(Condition.Exposure, Block, intercept_scaled, slope_scaled) %>%
      filter(Condition.Exposure %in% paste0("prior", c(1:5)), Block == block) %>%
      summarise(across(c(intercept_scaled, slope_scaled), .fns = if (mean) { ~ mean(.x, na.rm = T) } else { ~ median(.x, na.rm = T) } )) %>%
      mutate(PSE = -intercept_scaled/slope_scaled) %>% 
      pull(PSE)
  } else {
    io.intercept.slope.PSE %>%
      select(Condition.Exposure, Block, intercept_scaled, slope_scaled) %>%
      filter(Condition.Exposure == condition, Block == block) %>%
      mutate(PSE = -intercept_scaled/slope_scaled) %>% 
      pull(PSE)
  }
}


# Get approximate f0 of synthesised stimuli from VOT values
############################################################################

predict_f0 <- function(VOT, Mel = TRUE) {
  # Intercept and slope values are obtained from linear model based on original measurements of recordings of the exposure talker
  f0 <- 245.47 + 0.04 * (VOT)

  if (Mel) f0 <- phonR::normMel(f0)
  return(f0)
}

predict_vowel_duration <- function(VOT) {
  # Intercept and slope values are obtained from linear model computed separately for positive and negative VOT values
  vowel_duration <-
    ifelse(VOT > 0, (128.7 -0.32 * (VOT)), (122.9 - 0.18 * (VOT)))
  return(vowel_duration)
}


# Make IOs of each talker in a database and plot their categorization functions---------------------------------------------------

make_IOs_from_data <- function(
    data = d.chodroff_wilson,
    cues,
    groups = NULL,
    lapse_rate = 0,
    with_noise = T
) {
  data %>%
    make_MVG_ideal_observer_from_data(
      data = .,
      cues = cues,
      lapse_rate = lapse_rate,
      group = groups,
      Sigma_noise = if(with_noise == FALSE & length(cues) == 1) {
        matrix(c(0), ncol = 1, dimnames = list(cues, cues))
      } else if(with_noise == FALSE & length(cues) == 2) {
        matrix(rep(0, 4), ncol = 2, dimnames = list(cues, cues))
      } else if(with_noise == FALSE & length(cues) == 3) {
        matrix(rep(0, 9), ncol = 3, dimnames = list(cues, cues))
      } else if(with_noise == TRUE & length(cues) == 1) {
        matrix(c(80), ncol = 1, dimnames = list(cues, cues))
      } else if(with_noise == TRUE & length(cues) == 2) {
        m <- diag(2) * c(80, 878)
        dimnames(m) <- list(cues, cues)
        m
      } else if(with_noise == TRUE & length(cues) == 3) {
        m <- diag(3) * c(80, 878, 80)
        dimnames(m) <- list(cues, cues)
        m
      })
}

add_x_to_IO <- function(
    io,
    VOTs = seq(0, 130, .5),
    f0s = predict_f0(VOTs, Mel = T),
    vowel_durations = predict_vowel_duration(VOTs)
) {
  n_cues <- length(first(first(io$io)$mu))

  # Attach data based on dimensionality of IO
  io %>%
    { if (n_cues == 1) {
      crossing(., tibble(x = VOTs))
    } else if (n_cues == 2) {
      crossing(., tibble(x = map2(VOTs, f0s, ~ c(.x, .y))))
    } else if (n_cues == 3) {
      crossing(., tibble(x = pmap(list(VOTs, f0s, vowel_durations), ~ c(...))))
    }} %>%
    nest(x = x)
}

get_diff_in_likelihood_from_io <- function(x, io, predict_cues = NULL) {
  if (!is.null(predict_cues)) {
    for (cue in names(predict_cues)) {
      x <- c(x, predict_cues[[cue]](x[1]))
    }
  }

  # Find absolute difference from .5/.5
  y <-
    abs(
      dmvnorm(x, io$mu[[2]], io$Sigma[[2]] + io$Sigma_noise[[2]], log = F) /
        (dmvnorm(x, io$mu[[1]], io$Sigma[[1]] + io$Sigma_noise[[1]], log = F) +
           dmvnorm(x, io$mu[[2]], io$Sigma[[2]] + io$Sigma_noise[[2]], log = F)) - .5)

  return(y)
}

get_PSE_from_io <- function(io, find_along = "VOT", predict_cues = NULL) {
  message("get_PSE_from_io() finds the PSE along one phonetic cue (by default: VOT), assuming uniform priors.")

  # Keeping asserts here even though they check inputs for get_diff_in_likelihood_from_io() since
  # get_diff_in_likelihood_from_io() would otherwise get very slow.
  assert_that(is.MVG_ideal_observer(io))
  cues <- get_cue_labels_from_model(io)
  assert_that(find_along %in% cues,
              msg = paste(find_along, "must be a cue in the model (io)."))
  if (length(cues) > 1) {
    assert_that(is.list(predict_cues),
                msg = "For models (io) with more than one cue dimension, predict_cues must be a list of named functions.")
    assert_that(is_empty(setdiff(c(find_along, names(predict_cues)), cues)),
                msg = "The names of the functions in predict_cues must match the cue names in the model (io).")
  }

  # Set bounds for optimization to be the two category means
  # and initialize optimization half-way between the two means
  # of the find_along dimension
  min.pars <-
    io$mu %>%
    reduce(rbind) %>%
    apply(., MARGIN = 2, min)
  max.pars <-
    io$mu %>%
    reduce(rbind) %>%
    apply(., MARGIN = 2, max)
  pars = min.pars + (max.pars - min.pars) / 2

  # Find and return VOT values that minimize the difference in log-likelihoods
  o <- optim(
    par = pars[1],
    fn = get_diff_in_likelihood_from_io,
    method = "L-BFGS-B",
    control = list(factr = 10^-10),
    lower = min.pars[1],
    upper = max.pars[1],
    io = io,
    predict_cues = predict_cues)

  return(o$par)
}

add_PSE_and_categorization_to_IO <- function(io) {
  message("add_PSE_and_categorization_to_IO() assumes that observations x have already been attached to the IO.")

  io %>%
    mutate(
      PSE =
        map_dbl(
          io,
          function(io) {
            assert_that(is.MVG_ideal_observer(io))
            cues <- get_cue_labels_from_model(io)

            prediction_cues <- if (length(cues) == 1) {
              find_along <- cues
              predict_cues <- NULL
            } else {
              find_along <- cues[1]
              cues <- setdiff(cues, cues[1])
              predict_cues <- list()
              for (c in cues) {
                if (tolower(c) == "f0") {
                  predict_cues[[c]] <- function(x) predict_f0(x, Mel = F)
                } else if (tolower(c) == "f0_mel") {
                  predict_cues[[c]] <- predict_f0
                } else if (tolower(c) == "vowel_duration") {
                  predict_cues[[c]] <- predict_vowel_duration
                } else {
                  stop("add_PSE_and_categorization_to_IO() does not know how to predict cues other than f0, f0_Mel, and vowel_duration.")
                }
              }
            }

            get_PSE_from_io(io = io, find_along = find_along, predict_cues = predict_cues)
            } ),
      categorization =
        map2(
          x, io,
          ~ get_categorization_from_MVG_ideal_observer(x = .x$x, model = .y, decision_rule = "proportional") %>%
            filter(category == "/t/") %>%
            mutate(VOT = map(x, ~ .x[1]) %>% unlist())))
}

add_gaussians_as_geoms_to_io <- function(
    io,
    alpha = .3,
    linetype = 1,
    linewidth = .5,
    plot.colour = colours.category_greyscale
) {
  message("add_gaussians_as_geoms_to_io() assumes univariate IOs along VOT.")

  io %>%
    mutate(
      gaussian =
        pmap(
          list(x, category, mu, Sigma, Sigma_noise),
          ~ geom_function(
            data = ..1 %>% rename(VOT = x),
            aes(x = VOT, colour = ..2),
            fun = function(x) dnorm(x, mean = ..3[[1]][[1]], sd = sqrt(..4[[1]][[1]])),
            alpha = alpha,
            linetype = linetype,
            linewidth = linewidth)))
}

# Make non-parametric density plot ---------------------------------------------------
# ensuring that the corresponding proportion of points are included within each contour region as defined by their quantile
# adjusted from https://stackoverflow.com/questions/75598144/interpretation-of-2d-density-estimate-charts
# we can get the 2d density with MASS::kde2d, then convert to a raster using terra. We can then order the points according to the density in the associated 2d density grid and find the density at which a quantile is passed with approx
density_quantiles <- function(x, y, quantiles) {
  dens <- MASS::kde2d(x, y, n = 500)
  df   <- cbind(expand.grid(x = dens$x, y = dens$y), z = c(dens$z))
  r    <- terra::rast(df)
  ind  <- sapply(seq_along(x), function(i) terra::cellFromXY(r, cbind(x[i], y[i])))
  ind  <- ind[order(-r[ind][[1]])]
  vals <- r[ind][[1]]
  ret  <- approx(seq_along(ind)/length(ind), vals, xout = quantiles)$y
  replace(ret, is.na(ret), max(r[]))
}

# Evaluate psychometric model predictions ---------------------------------------------------
# note: predictions are of the whole model which includes the lapse rates
# this is not strictly-speaking, equivalent to the IA/IO models which assume no lapsing
get_pyschometric_accuracy <- function(
    model_fit,
    newdata,
    phase,
    sd
) {
  blocks <- if (phase == "test") c(1, 3, 5, 7) else (c(2, 4, 6))
  epred_draws(
    object = model_fit,
    newdata = newdata %>%
      # taking VOTs from exposure phase only because they have intended categories
      filter(Phase == "exposure", Item.Labeled == F) %>%
      group_by(Condition.Exposure) %>%
      filter(ParticipantID == first(ParticipantID)) %>%
      reframe(Condition.Exposure, Item.VOT, category) %>%
      # expand to number of test/exposure blocks
      expand_grid(Block = blocks) %>%
      mutate(VOT_gs = (Item.VOT - VOT.mean_test) / (2 * sd)),
    seed = 928,
    ndraws = 1000,
    # ignored group-level variability
    re_formula = NA) %>%
    mutate(proportional = ifelse(category == "/t/", .epred, 1 -.epred),
           criterion = case_when(category == "/t/" & .epred >= .5 ~ 1,
                                 category == "/d/" & .epred < .5 ~ 1,
                                 .default = 0)) %>%
    group_by(Condition.Exposure, Block, .draw) %>%
    summarise(across(c(proportional, criterion), mean, .names = "{.col}_mean")) %>%
    group_by(Condition.Exposure, Block) %>%
    median_hdci(proportional_mean, criterion_mean) %>%
    pivot_longer(
      cols = c(3:8),
      names_to = c("decision", ".value"),
      names_sep = "_") %>%
    rename(median = mean,
           lower = mean.lower,
           upper = mean.upper) %>%
    select(-c(3:5))
}


# Evaluation of IOs --------------------------------------------------------
get_average_accuracy_of_IO <- function(observations, responses, model) {
  get_categorization_from_MVG_ideal_observer(x = observations, model = model, decision_rule = "proportional") %>%
    # we only need one posterior since the other one is simply 1 minus that
    filter(category == "/t/") %>%
    mutate(
      human_response = .env$responses,
      accuracy = ifelse(category == human_response, response, 1 - response)) %>%
    summarize(mean_accuracy = mean(accuracy))
}

get_average_log_likelihood_of_perception_data_under_IO <- function(observed_inputs, observed_responses, model) {
  require(assertthat)

  d.likelihood <-
    get_categorization_from_MVG_ideal_observer(x = observed_inputs, model = model, decision_rule = "proportional") %>%
    # we only need one posterior since the other one is simply 1-that
    filter(category == "/t/")

  if (is.character(observed_responses)) {
    message("Observed responses seems to be a vector of categories. Proceeding under that assumption.")
    assert_that(
      all(unique(observed_responses) %in% model$category),
      msg = "If observed_responses is a character vector, each of its elements must be one of the categories in the model.")

    d.likelihood %<>%
      mutate(
        human_response = .env$observed_responses,
        likelihood = case_when(
          human_response == "/t/" ~ response,
          human_response == "/d/" ~ 1 - response,
          T ~ NA_real_))
  } else if (is.numeric(observed_responses)) {
    message("Observed responses seems to be a vector of numbers. Proceeding under the assumption that these numbers indicate the probability of /t/-responses.")
    assert_that(
      all(between(observed_responses, 0, 1)),
      msg = "If observed_responses is a numeric vector, each of its elements must be a number between 0 and 1.")

    d.likelihood %<>%
      mutate(
        probability_of_human_t_response = .env$observed_responses,
        likelihood = probability_of_human_t_response * response + ((1 - probability_of_human_t_response) * (1 - response)))
  } else {
    stop("Unrecognized type of observed_responses.")
  }

 d.likelihood %>%
    summarize(log_likelihood_per_response = mean(log(likelihood), na.rm = T))
}


# Assumes that the model is in a column called "model", the input data and response is
# in a column data (data$x and data$Response.Category, respectively):
get_likelihood_from_grouped_data <- function(data) {
  data %>%
    mutate(
      ll = map2(
        model,
        data,
        ~ evaluate_model(
          model = .x,
          x = .y$x,
          response_category = .y$Response.Category,
          decision_rule = "proportional",
          noise_treatment = "marginalize",
          lapse_treatment = "marginalize",
          method = "likelihood-up-to-constant",
          return_by_x = F))) %>%
    pull(ll) %>%
    # Sum up all the lls of the different exposure conditions
    reduce(`+`)
}

# General functions for model updating ------------------------------------
slice_into_unique_exposure_test <- function(df, block_order, block_var = "Block", condition_var = "Condition") {
  df %<>%
    mutate(Block = map(!! sym(block_var), ~ which(block_order == .x)) %>% unlist())

  df.new <- tibble()
  for (i in grep("test", block_order))
    for (g in unique(df[[condition_var]])) #different exposure scenarios
      df.new %<>%
    rbind(
      df %>%
        # Include only exposure blocks from the current exposure condition and the current test block
        # from the current exposure condition (but not earlier test blocks)
        filter(!! sym(condition_var) == g, Block <= i, Block == i | !(Block %in% grep("test", block_order))) %>%
        # could be further simplified by recognizing that by test block7 everyone has seen the same thing
        mutate(ExposureGroup = if (i == 1) "no exposure" else paste0("Cond ", g, "_Up to ", block_order[i])))

  df.new  %>%
    select(ExposureGroup, Block, everything())
}

# Update normalization -----------------------------------------------------
update_normalization_and_normalize_test_data <- function(
    mu_0 = first(prior_marginal_VOT_f0_stats$x_mean),
    kappa,
    data
) {
  # Get normalization parameters for each exposure data (exposure condition, list, block)
  exposure.normalization <-
    data %>%
    filter(Phase == "exposure") %>%
    group_by(ExposureGroup) %>%
    filter(ParticipantID == first(ParticipantID)) %>%
    summarise(
      x_N = length(x),
      x_mean = list(colMeans(reduce(x, rbind))),
      x_cov = list(cov(reduce(x, rbind)))) %>%
    # Apply normalization based on exposure to test
    mutate(
      mu_n = map2(x_N, x_mean, ~ 1 / (.env$kappa + .x) * (.env$kappa * .env$mu_0 + .x * .y))) %>%
    bind_rows(tibble(ExposureGroup = "no exposure", x_N = 0, x_mean = list(.env$mu_0), x_cov = NULL, mu_n = list(.env$mu_0)))

  # Apply normalization to each test data, and return those test data
  data %>%
    filter(Phase == "test") %>%
    left_join(
      exposure.normalization %>%
        select(ExposureGroup, mu_n),
      by = "ExposureGroup") %>%
    mutate(x = map2(x, mu_n, ~ .x - (.y - .env$mu_0)))
}

get_likelihood_from_updated_normalization <- function(
    par,
    prior = m_IO.VOT_f0,
    data = d_for_ASP.for_normalization
) {
  kappa <- exp(par[1])

  ll <-
    update_normalization_and_normalize_test_data(
      kappa = kappa,
      data = data) %>%
    group_by(ExposureGroup) %>%
    select(ExposureGroup, x, Response.Category) %>%
    nest(data = c(x, Response.Category)) %>%
    # Since the model never changes (only the cues do), there is only
    # one group of data for normalization
    mutate(model = list(prior)) %>%
    get_likelihood_from_grouped_data()

  history.optimization_normalization <<-
    bind_rows(
      history.optimization_normalization,
      tibble(kappa = kappa, log_likelihood = ll))

  return(ll)
}

# Update decision biases ---------------------------------------------------
#
# This function assumes that the error signal is relative to the expected response.
# This assumes that listeners can figure out---even on unlabelled trials---what the
# expected response is.
update_decision_bias_by_group <- function(
    prior,
    beta,
    data
) {
  cues <- get_cue_labels_from_model(prior)
  u <-
    data %>%
    group_map(
      .f = ~ update_model_decision_bias_incrementally(
        model = prior,
        beta = beta,
        exposure = .x,
        exposure.category = "Item.ExpectedResponse.Category",
        exposure.cues = cues,
        noise_treatment = "marginalize",
        lapse_treatment = "marginalize",
        keep.update_history = TRUE,
        keep.exposure_data = FALSE) %>%
        nest(posterior = everything()) %>%
        bind_cols(.y)) %>%
    reduce(bind_rows)
}

format_updated_bias_models_and_join_test_data <- function(
    models,
    prior,
    data.test
) {
  models %>%
    mutate(
      posterior =
        map(
          posterior,
          ~ filter(.x, observation.n %in% c(48, 96, 144)) %>%
            mutate(observation.n = case_when(
              observation.n == 48 ~ "_Up to test3",
              observation.n == 96 ~ "_Up to test5",
              observation.n == 144 ~ "_Up to test7")))) %>%
    unnest(posterior) %>%
    # Remap the different update steps onto the ExposureGroup
    mutate(ExposureGroup = paste0(gsub("^(.*)_.*$", "\\1", ExposureGroup), observation.n)) %>%
    select(-observation.n) %>%
    # If test data contains tests for "no exposure" condition, bind prior model back in
    # and call it "no exposure" (necessary for likelihood calculation)
    { if ("no exposure" %in% data.test$ExposureGroup)
      bind_rows(
        .,
        bind_cols(
          prior,
          tibble(ExposureGroup = "no exposure")) %>%
          crossing(ParticipantID = unique(as.character(data.test$ParticipantID)))) else . } %>%
    # Nest updated model and join test responses
    nest(model = -c(ExposureGroup, ParticipantID)) %>%
    left_join(data.test, by = join_by(ExposureGroup, ParticipantID))
}

get_likelihood_from_updated_bias <- function(
    par,
    prior = m_IO.VOT_f0,
    # These data sets are set as a default to speed up computation, since this allows us
    # to move any steps that would have to repeated on each optimization step but that do
    # *not* depend on beta out of the optimization.
    data.exposure = d_for_ASP.for_decision_changes.exposure,
    data.test = d_for_ASP.for_decision_changes.test
) {
  beta <- exp(par[1])

  ll <-
    suppressWarnings(
      update_decision_bias(
        prior = prior,
        beta = beta,
        data = data.exposure)) %>%
    format_updated_bias_models_and_join_test_data(prior = prior, data.test = data.test) %>%
    get_likelihood_from_grouped_data()

  message("For beta = ", beta, " found log-likelihood of ", ll)
  history.optimization_bias <<-
    bind_rows(
      history.optimization_bias,
      tibble(
        beta = beta,
        log_likelihood = ll))

  return(ll)
}

# Make trivariate normal distribution ---------------------------------------------------
# Function takes as arguments the mean and covariance matrix of a trivariate normal distribution over (in this order)
# VOT, f0_Mel, and vowel_duration, as well as values for f0_Mel and vowel_duration. The function then returns a
# *function* that represents the conditional normal distribution over VOT at that combination of f0_Mel and
# vowel_duration values. This function takes VOT as input and returns a density value.
#
# This function was drafted with the help of Google's Bard: https://bard.google.com/chat/32a2428ef7e806bb
conditional_univariate_normal_from_trivariate_normal <- function(mu, Sigma, f0_Mel, vowel_duration) {
  # Calculate the conditional mean and conditional standard deviation of x given y = f0_Mel and z = vowel_duration
  mu_x <- mu[1] + Sigma[1, 2] / Sigma[2, 2] * (f0_Mel - mu[2]) + Sigma[1, 3] / Sigma[3, 3] * (vowel_duration - mu[3])
  sigma_x <- sqrt(Sigma[1, 1] - Sigma[1, 2] / Sigma[2, 2] * Sigma[2, 1] - Sigma[1, 3] / Sigma[3, 3] * Sigma[3, 1])

  # Define the function for the conditional univariate normal distribution over x given f0_Mel and vowel_duration
  f <- function(x) {
    1 / (sqrt(2 * pi) * sigma_x) * exp(-0.5 * ((x - mu_x) / sigma_x)^2)
  }

  return(f)
}

# This functions takes as arguments the means and covariance matrices of two trivariate normal distributions,
# as well as values for f0_Mel and vowel_duration. The function then returns a *function* that represents the
# posterior of the category corresponding to the second trivariate normal distribution.  This function takes
# VOT as input and returns a posterior probability.
conditional_univariate_posterior_t <- function(mu_d, Sigma_d, mu_t, Sigma_t, f0_Mel, vowel_duration) {
  density_t <- conditional_univariate_normal_from_trivariate_normal(mu_t, Sigma_t, f0_Mel, vowel_duration)
  density_d <- conditional_univariate_normal_from_trivariate_normal(mu_d, Sigma_d, f0_Mel, vowel_duration)

  f <- function(x) {
    density_t(x) / (density_t(x) + density_d(x))
  }
}
