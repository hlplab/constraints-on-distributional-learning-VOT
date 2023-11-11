# General functionality ----------------------------------------------
logit_to_prob <- function(model, term, index = 1) {
  paste0(round(plogis(as.numeric(summary(model)$fixed[term, index])) * 100, 1), "%")
}

# function to transform Gelman-scaled values back
descale <- function(x, mean, sd) {
  x_0 = (x * 2 * sd) + mean
  return(x_0)
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
remove_x_guides <-
theme(axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank())

# from https://stackoverflow.com/questions/44283852/merging-expressions-for-plotting
comb_plotmath <- function(...) {
  Reduce(function(x, y) substitute(x ~ y, env = list(x = x, y = y)),
         list(...))
}


# Load data --------------------------------------------------------------
get_ChodroffWilson_data <- function(
    database_filename = "all_observations_with_non-missing_vot_cog_f0.csv",
    categories = c("/b/", "/d/", "/g/", "/p/", "/t/", "/k/"),
    min.n_per_talker_and_category = 0,
    limits.VOT = c(-Inf, Inf),
    limits.f0 = c(0, Inf),
    max.p_for_multimodality = 1
) {
  require(tidyverse)
  require(magrittr)
  require(diptest)

  # Standardizing variable names and values to confirm to what we usually use.
  d.chodroff_wilson <-
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
      word_duration, speech_rate)

  d.chodroff_wilson %<>%
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
    d.chodroff_wilson %<>%
      group_by(Talker) %>%
      mutate(f0_Mel = phonR::normMel(f0)) %>%
      group_by(Talker, category) %>%
      mutate(
        f0_Mel.multimodal = dip.test(f0_Mel)$p.value < max.p_for_multimodality) %>%
      filter(!f0_Mel.multimodal) %>%
      droplevels())

  # Keep only talkers with at least n.min observations for each stop
  d.chodroff_wilson %<>%
    group_by(Talker, category) %>%
    mutate(n_per_category = length(category)) %>%
    group_by(Talker) %>%
    mutate(n_per_category = ifelse(any(is.na(n_per_category)), 0, min(n_per_category))) %>%
    ungroup() %>%
    filter(n_per_category > min.n_per_talker_and_category)

  # Get Mel and Semitones, then C-CuRE
  d.chodroff_wilson %<>%
    group_by(Talker) %>%
    mutate(f0_semitones = 12 * log(f0 / mean(f0)) / log(2)) %>%
    ungroup()
}


# Get info from psychometric model -----------------------------------------------------------

# function to get PSE from a model already in tibble format
get_PSE <- function(model, y) {
  y <- model %>% pull(y)
  as.numeric(model[which(abs(y - .5) == min(abs(y - .5))), 1])
}

# Function for calculating CI from logits of a model summary
get_coefficient_fr_model <- function(model, term) {
  round(as.numeric(summary(model)$fixed[term, 1]), 1)
}

get_bf <- function(model, hypothesis) {
  h <- hypothesis(model, hypothesis)[[1]]
  if (is.infinite(h$Evid.Ratio)) ER <- "> 8000" else ER <- round(h$Evid.Ratio, 1)
  paste0(
    "\\hat{\\beta} = ", round(h$Estimate, 2),
    ",\\ 90\\%{\\rm -CI} = [", round(h$CI.Lower, 3), ", ", round(h$CI.Upper, 3),
    "],\\ BF = ", ER,
    ",\\ p_{posterior} = ", signif(h$Post.Prob, 3))
}

print_CI <- function(model, term) {
  paste0(round(plogis(as.numeric(summary(model)$fixed[term, 1])) * 100, 1),
         "%, 95%-CI: ",
         paste0(round(plogis(as.numeric(summary(model)$fixed[term, 3:4])) * 100, 1), collapse = " to "), "%")
}

# Function to get identity CI of a model summary
get_CI <- function(model, term, hypothesis) {
  paste0(round(as.numeric(summary(model)$fixed[term, 1]), 1), " 95%-CI: ",
         paste(round(as.numeric(summary(model)$fixed[term, 3:4]), 1), collapse = " to "),
         "; ",
         get_bf(model = model, hypothesis = hypothesis))
}


# plotting the Bayesian psychometric fit
geom_linefit <- function(data, x, y, fill, legend.position, legend.justification = NULL) {
  list(
    geom_ribbon(aes(x = {{ x }}, y = {{ y }}, group = Condition.Exposure,
                    ymin = lower__, ymax = upper__, fill = Condition.Exposure), alpha = .1),
    geom_line(aes(
      x = {{ x }}, y = {{ y }}, group = Condition.Exposure, colour = Condition.Exposure), linewidth = .7, alpha = 0.6),
    geom_rug(data = data %>% distinct(Item.VOT), aes(x = {{ x }}), alpha = 0.5, colour = "grey", inherit.aes = F),
    stat_summary(
      data = data, fun.data = mean_cl_boot, mapping = aes(x = {{ x }}, y = Response.Voiceless, colour = Condition.Exposure),
      geom = "pointrange", size = 0.1, alpha = 0.7, position = position_dodge2(width = 2), inherit.aes = F),
    scale_x_continuous("VOT (ms)"),
    scale_y_continuous("Proportion \"t\"-responses"),
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


# NORMALIZATION -----------------------------------------------------------

# if there is newdata return the newdata scaled by the old data
# if there is no newdata return data scaled by itself
prep_predictors_for_CCuRE <- function(data, newdata = NULL){
  if(!is.null(newdata)) {
    mean.vowel_duration = mean(data$vowel_duration)
    sd.vowel_duration = sd(data$vowel_duration)

    newdata %>%
      mutate(vowel_duration = (vowel_duration - mean.vowel_duration) / sd.vowel_duration)
  } else {
    data %>%
      ungroup() %>%
      mutate(across(c(vowel_duration), ~ (.x - mean(.x)) / sd(.x)))
  }
}

get_CCuRE_model <- function(data, tidy_result = TRUE, cue = "VOT") {
  f <- formula(paste(cue, "~ 1 + vowel_duration + (1 | Talker)"))
  m <- lme4::lmer(f, data =  prep_predictors_for_CCuRE(data))
  return(if (tidy_result) tidy(m, effects = "fixed") else m)
}

apply_ccure <- function(x, data) {
  m <- get_CCuRE_model(
    data = data %>% mutate(current_outcome = .env$x),
    cue = "current_outcome",
    tidy_result = F)
  # Get residuals (remove predicted/expected cue value--including expectations
  # about the talker---from each token's actual cue value) and then add the
  # overall cue mean back (this latter step is done for visualization purposes,
  # so that the C-CuREd cues can still be expressed in the familiar cue space;
  # the intercept is that predicted mean since get_CCuRE_model calls prep_predictors_for_CCuRE,
  # which centers and scales all predictors.
  x - predict(m) + fixef(m)[1]
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

# Make ideal observer based on exposure conditions
make_VOT_IOs_from_exposure <- function(data){
  data %>%

    # This takes all of the actual data to make ideal observers. Alternatively, one could
    # first distinct the data to one full list of each condition.
    make_MVG_ideal_observer_from_data(
      group = "Condition.Exposure",
      cues = c("VOT"),
      Sigma_noise = matrix(80, dimnames = list("VOT", "VOT"))) %>%
    # Add prior based on Chodroff & Wilson (2018)
    # (comment out to not express gray reference line below)
    bind_rows(
      d.chodroff_wilson %>%
        make_MVG_ideal_observer_from_data(
          cues = c("VOT"),
          Sigma_noise = matrix(80, dimnames = list("VOT", "VOT"))) %>%
        mutate(Condition.Exposure = "prior")) %>%
    nest(io = -c(Condition.Exposure))
}

# Estimate the intercept, slope, and PSE that these ideal observers would have
# were they analyzed the same way the human data is analyzed.
get_logistic_parameters_from_model <- function(
    model,
    x,
    model_col = "model",
    groups = NULL,
    resolution = 10^12
) {
  f <-
    if (any(map(model[[model_col]], is.MVG_ideal_observer) %>% unlist()))
      get_categorization_from_MVG_ideal_observer else
        if (any(map(model[[model_col]], is.NIW_ideal_adaptor) %>% unlist()))
          get_categorization_from_NIW_ideal_adaptor else stop("Model type not recognized.")

  model %>%
    # Cross in test tokens
    left_join(x, by = "Condition.Exposure") %>%
    mutate(x = map(x, ~ c(.x))) %>%
    nest(x = x) %>%
    # Get categorization proportions (turned into counts below)
    mutate(
      categorization =
        map2(x, !! sym(model_col),
             ~ f(
               x = .x$x, model = .y, decision_rule = "proportional") %>%
               mutate(VOT = map(x, ~ .x[1]) %>% unlist()))) %>%
    unnest(cols = categorization, names_repair = "unique") %>%
    # Prepare data frame for logistic regression
    pivot_wider(names_from = category, values_from = response, names_prefix = "response_") %>%
    mutate(n_d = round(`response_/d/` * .env$resolution), n_t = .env$resolution - n_d) %>%
    group_by(!!! syms(groups)) %>%
    nest() %>%
    # Fit logistic regression and extract relevant information
    # (the regression only uses VOT regardless of what cues are used for the categorization
    # so that this matches the analysis of the human responses)
    mutate(
      model_unscaled = map(data, ~ glm(
        cbind(n_t, n_d) ~ 1 + VOT,
        family = binomial,
        data = .x)),
      intercept_unscaled = map_dbl(model_unscaled, ~ tidy(.x)[1, 2] %>% pull()),
      slope_unscaled = map_dbl(model_unscaled, ~ tidy(.x)[2, 2] %>% pull()),
      model_scaled = map(data, ~ glm(
        cbind(n_t, n_d) ~ 1 + I((VOT - VOT.mean_test) / (2 * VOT.sd_test)),
        family = binomial,
        data = .x)),
      intercept_scaled = map_dbl(model_scaled, ~ tidy(.x)[1, 2] %>% pull()),
      slope_scaled = map_dbl(model_scaled, ~ tidy(.x)[2, 2] %>% pull()),
      PSE = -intercept_unscaled/slope_unscaled)
}

############################################################################
# Get approximate f0 of synthesised stimuli from VOT values
############################################################################
# Get the linear prediction parameters for exposure stimuli based on f0 measurements aligned with Chodroff-Wilson

predict_f0 <- function(VOT, intercept = 245.46968, slope = 0.03827, Mel = FALSE) {
  f0 <- intercept + slope * (VOT)
  if (Mel) f0 <- phonR::normMel(f0)
  return(f0)
}

############################################################################
#  function to optimise minimal difference in likelihoods of 2 categories  #
############################################################################
get_diff_in_likelihood_from_io <- function(x, io, add_f0 = F, io.type = NULL) {
  # Since we want to only consider cases that have F0 values that are in a certain linear relation to VOT
  # (the way we created our stimuli), we set the F0 based on the VOT.
  if (add_f0) x <- c(x, normMel(predict_f0(x)))
  #else if (add_f0 & io.type == "VOT_F0.centered.input") x <- c(x, normMel(predict_f0(x)) + (chodroff.mean_f0_Mel - f0.mean_exp1))
  #else if (add_f0 & io.type == "VOT_F0.centered.input_block1") x <- c(x, normMel(predict_f0(x)) + (chodroff.mean_f0_Mel - f0.mean_test))

  # abs(dmvnorm(x, io$mu[[1]], io$Sigma[[1]], log = T) - dmvnorm(x, io$mu[[2]], io$Sigma[[2]], log = T))
  y <- abs(dmvnorm(x, io$mu[[2]], io$Sigma[[2]] + io$Sigma_noise[[2]], log = F) / (dmvnorm(x, io$mu[[1]], io$Sigma[[1]] + io$Sigma_noise[[1]], log = F) + dmvnorm(x, io$mu[[2]], io$Sigma[[2]] + io$Sigma_noise[[2]], log = F)) - .5)

  # message(paste("Explored VOT =", x, "and found p(t) of", y, "\n"))
  return(y)
}

get_PSE_from_io <- function(io, io.type = NULL) {
  # Set bounds for optimization to be the two category means
  # and initialize optimization half-way between the two means
  min.pars <-
    io$mu %>%
    reduce(rbind) %>%
    apply(., MARGIN = 2, min)
  max.pars <-
    io$mu %>%
    reduce(rbind) %>%
    apply(., MARGIN = 2, max)
  pars = (max.pars - min.pars) / 2

  # Find and return VOT values that minimize the difference in log-likelihoods
  o <- optim(
    par = pars[1],
    fn = get_diff_in_likelihood_from_io,
    method = "L-BFGS-B",
    control = list(factr = 10^-10),
    lower = min.pars[1],
    upper = max.pars[1],
    io = io,
    io.type = io.type,
    add_f0 = length(pars) > 1)

  return(o$par)
}

############################################################################
# This can be used to implement different hypotheses about speech perception. There are quite a few choices for the researcher as to what specific hypothesis you want to test:
#
# 1) currently uses VOT and f0, but could just use VOT (if you use f0, make sure to choose the relation between f0 and VOT in the arguments)
#
# 2) what knowledge do listeners have?
# grouping by talker makes talker-specific IOs
# grouping by (only) gender makes gender-specific IOs over all data from that gender
# grouping by talker and gender and then aggregating down to gender (not yet implemented, but would just require adding aggregate_models_by_group_structure()) give 'typical' (average) gender-specific IOs
#
# 3) were listeners' IOs based on centered or uncentered cues?
#
# 4) do listeners center during experiment? one can plot this relative to the center of the cues in the experiment or not
############################################################################
get_IO_categorization <- function(
    data = d.chodroff_wilson,
    cues,
    groups,
    lapse_rate = 0,
    with_noise = TRUE,
    VOTs = seq(0, 85, .5),
    F0s = normMel(predict_f0(VOTs)),
    alpha = .1,
    linewidth = .3
) {
  data %<>%
    make_MVG_from_data(cues = cues, group = groups) %>%
    group_by(!!! syms(groups)) %>%
    nest(mvg = -all_of(groups)) %>%
    mutate(
      io = map(
        mvg,
        ~ lift_MVG_to_MVG_ideal_observer(
          .x,
          group = NULL,
          prior = c("/d/" = .5, "/t/" = .5),
          lapse_rate = lapse_rate,
          lapse_bias = c("/d/" = .5, "/t/" = .5),
          Sigma_noise =
            if(with_noise == FALSE & length(cues) == 1) {
              matrix(c(0), ncol = 1, dimnames = list(cues, cues))
            } else if(with_noise == FALSE & length(cues) == 2) {
              matrix(c(0, 0, 0, 0), ncol = 2, dimnames = list(cues, cues))
            } else if(with_noise == TRUE & length(cues) == 1) {
              matrix(c(80), ncol = 1, dimnames = list(cues, cues))
            } else if(with_noise == TRUE & length(cues) == 2) {
              matrix(c(80, 0, 0, 878), ncol = 2, dimnames = list(cues, cues))
            })))

  {if(length(cues) == 1) {
    crossing(data,
             tibble(
               !! sym(cues[1]) := VOTs,
               x = map(!! sym(cues[1]), ~ c(.x))))
  } else {
    crossing(data,
             tibble(
               !! sym(cues[1]) := VOTs,
               !! sym(cues[2]) := F0s,
               x = map2(!! sym(cues[1]),
                        !! sym(cues[2]), ~ c(.x, .y))))
  }} %>%

    select(- all_of(cues)) %>%
    nest(x = x) %>%
    mutate(
      PSE = map_dbl(
        io, ~ get_PSE_from_io(io = .x)),
      categorization =
        map2(
          x, io,
          ~ get_categorization_from_MVG_ideal_observer(x = .x$x, model = .y, decision_rule = "proportional") %>%
            filter(category == "/t/") %>%
            mutate(VOT = map(x, ~ .x[1]) %>% unlist())),
      line = map2(categorization, gender,
                  ~ geom_line(data = .x,
                              aes(x = VOT,
                                  y =  response,
                                  color = .y),
                              alpha = alpha,
                              linewidth = linewidth)))
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


############################################################################
# function to prepare variables for modelling
############################################################################
prepVars <- function(d, test_mean = NULL, levels.Condition = NULL, contrast_type) {
  d %<>%
    drop_na(Condition.Exposure, Phase, Block, Item.MinimalPair, ParticipantID, Item.VOT, Response)

  message("VOT mean:", signif(mean(d$Item.VOT, na.rm = T)))
  message("VOT sd:", signif(sd(d$Item.VOT, na.rm = T)))
  #print(paste("VOT test mean:", test_mean))

  d %<>%
    ungroup() %>%
    mutate(
      Block_n = as.numeric(as.character(Block)),
      across(c(Condition.Exposure, Block, Item.MinimalPair), factor),

      Condition.Exposure = factor(Condition.Exposure, levels = levels.Condition)) %>%

    drop_na(Block, Response, Item.VOT) %>%
    mutate(VOT_gs = (Item.VOT - mean(Item.VOT, na.rm = TRUE)) / (2 * sd(Item.VOT, na.rm = TRUE))) %>%
    droplevels()

  contrasts(d$Condition.Exposure) <- cbind("_Shift10 vs. Shift0" = c(-2/3, 1/3, 1/3),
                                          "_Shift40 vs. Shift10" = c(-1/3,-1/3, 2/3))
  if (all(d$Phase == "test") & n_distinct(d$Block) > 1 & contrast_type == "difference") {
    contrasts(d$Block) <- MASS::fractions(MASS::contr.sdif(6))
    dimnames(contrasts(d$Block))[[2]] <- c("_Test2 vs. Test1", "_Test3 vs. Test2", "_Test4 vs. Test3", "_Test5 vs. Test4", "_Test6 vs. Test5")

    message("Condition contrast is:", contrasts(d$Condition.Exposure))
    message("Block contrast is:", contrasts(d$Block))
  } else if (all(d$Phase == "test") & n_distinct(d$Block) > 1 & contrast_type == "helmert"){
    contrasts(d$Block) <- cbind("_Test2 vs. Test1" = c(-1/2, 1/2, 0, 0, 0, 0),
                                "_Test3 vs. Test2_1" = c(-1/3, -1/3, 2/3, 0, 0, 0),
                                "Test4 vs. Test3_2_1" = c(-1/4, -1/4, -1/4, 3/4, 0, 0),
                                "_Test5 vs. Test4_3_2_1" = c(-1/5, -1/5, -1/5, -1/5, 4/5, 0),
                                "_Test6 vs. Test5_4_3_2_1" = c(-1/6, -1/6, -1/6, -1/6, -1/6, 5/6))
    message(contrasts(d$Condition.Exposure))
    message(contrasts(d$Block))
  } else if (all(d$Phase == "exposure") & n_distinct(d$Block) > 1 & contrast_type == "difference"){
    contrasts(d$Block) <- cbind("_Exposure2 vs. Exposure1" = c(-2/3, 1/3, 1/3),
                                "_Exposure3 vs. Exposure2" = c(-1/3,-1/3, 2/3))
    message("Condition contrast is:", MASS::fractions(contrasts(d$Condition.Exposure)))
    message("Block contrast is:", MASS::fractions(contrasts(d$Block)))
  } else if (n_distinct(d$Block) > 1 & contrast_type == "difference"){
    contrasts(d$Block) <- MASS::fractions(MASS::contr.sdif(9))
    dimnames(contrasts(d$Block))[[2]] <- c("_Exp1 vs. Test1", "_Test2 vs. Exp1", "_Exp2 vs. Test2", "_Test3 vs. Exp2", "_Exp3 vs. Test3", "_Test4 vs. Exp3", "_Test5 vs. Test4", "_Test6 vs. Test5")
    message("Condition contrast is:", MASS::fractions(contrasts(d$Condition.Exposure)))
    message("Block contrast is:", MASS::fractions(contrasts(d$Block)))
  } else {
    message(contrasts(d$Condition.Exposure))
  }
  return(d)
}

### function for non-parametric density plot
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


### function for formatting hypothesis tables
align_tab <- function(hyp) {
  map_chr(hyp, ~ ifelse(class(.x) == "numeric", "r","l"))
}

make_hyp_table <- function(hyp_readable, hyp, caption, col1_width = "15em") {
  cbind(hyp_readable, hyp) %>%
    dplyr::select(-2) %>%
    mutate(
      across(where(is.numeric), ~ round(., digits = 3)),
      CI = paste0("[", CI.Lower, ", ", CI.Upper, "]")) %>%
    dplyr::select(-c(CI.Upper, CI.Lower)) %>%
    relocate(CI, .before = "Evid.Ratio") %>%
    kbl(caption = caption, align = align_tab(hyp),
        format = "latex",
        booktabs = TRUE,
        escape = FALSE,
        col.names = c("Hypothesis", "Estimate", "SE", "90\\%-CI", "BF", "$p_{posterior}$")) %>%
    kable_styling(full_width = FALSE) %>%
    column_spec(1, width = col1_width)
}

### function to fit Bayesian model; priorSD argument refers to the SD for the VOT estimate
fit_model <- function(data, phase, formulation = "standard", priorSD = 2.5, adapt_delta = .99) {
  require(tidyverse)
  require(magrittr)
  require(brms)

  VOT.mean_test <-
    data %>%
    filter(Phase == "test") %>%
    ungroup() %>%
    summarise(mean = mean(Item.VOT, na.rm = T)) %>%
    pull(mean)
  levels_Condition.Exposure <- c("Shift0", "Shift10", "Shift40")
  contrast_type <- "difference"
  chains = 4

  data %<>%
    filter(Phase == phase & Item.Labeled == F) %>%
    prepVars(test_mean = VOT.mean_test, levels.Condition = levels_Condition.Exposure, contrast_type = contrast_type)

  prior_overwrite <- if (phase == "exposure" & formulation == "nested_slope") {
    c(set_prior(paste0("student_t(3, 0, ", priorSD, ")"), coef = "IpasteCondition.ExposureBlocksepEQxShift0x2:VOT_gs", dpar = "mu2"),
      set_prior(paste0("student_t(3, 0, ", priorSD, ")"), coef = "IpasteCondition.ExposureBlocksepEQxShift0x4:VOT_gs", dpar = "mu2"),
      set_prior(paste0("student_t(3, 0, ", priorSD, ")"), coef = "IpasteCondition.ExposureBlocksepEQxShift0x6:VOT_gs", dpar = "mu2"),
      set_prior(paste0("student_t(3, 0, ", priorSD, ")"), coef = "IpasteCondition.ExposureBlocksepEQxShift10x2:VOT_gs", dpar = "mu2"),
      set_prior(paste0("student_t(3, 0, ", priorSD, ")"), coef = "IpasteCondition.ExposureBlocksepEQxShift10x4:VOT_gs", dpar = "mu2"),
      set_prior(paste0("student_t(3, 0, ", priorSD, ")"), coef = "IpasteCondition.ExposureBlocksepEQxShift10x6:VOT_gs", dpar = "mu2"),
      set_prior(paste0("student_t(3, 0, ", priorSD, ")"), coef = "IpasteCondition.ExposureBlocksepEQxShift40x2:VOT_gs", dpar = "mu2"),
      set_prior(paste0("student_t(3, 0, ", priorSD, ")"), coef = "IpasteCondition.ExposureBlocksepEQxShift40x4:VOT_gs", dpar = "mu2"),
      set_prior(paste0("student_t(3, 0, ", priorSD, ")"), coef = "IpasteCondition.ExposureBlocksepEQxShift40x6:VOT_gs", dpar = "mu2"))
  } else if (phase == "test" & formulation == "nested_slope") {
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
    formula = if (formulation == "nested_slope") {
      bf(Response.Voiceless ~ 1,
         mu1 ~ 0 + offset(0),
         mu2 ~ 0 + I(paste(Condition.Exposure, Block, sep = "x")) / VOT_gs +
           (0 + Block / VOT_gs | ParticipantID) +
           (0 + I(paste(Condition.Exposure, Block, sep = "x")) / VOT_gs | Item.MinimalPair),
         theta1 ~ 1)
    } else {
      bf(Response.Voiceless ~ 1,
         mu1 ~ 0 + offset(0),
         mu2 ~ 1 + VOT_gs * Condition.Exposure * Block + (1 + VOT_gs * Block | ParticipantID) + (1 + VOT_gs * Condition.Exposure * Block | Item.MinimalPair),
         theta1 ~ 1)},
    data = data,
    prior = my_priors,
    cores = 4,
    chains = chains,
    init = 0,
    iter = 4000,
    warmup = 2000,
    family = mixture(bernoulli("logit"), bernoulli("logit"), order = F),
    control = list(adapt_delta = adapt_delta),
    file = paste0("../models/", phase, "-", formulation, "-priorSD", priorSD, "-", adapt_delta, ".rds")
  )
}
