# It is recommended to put all your functions that are used across reports for this project in one place.
# Try hard to assure backward compatibility.

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

percent <- function(x) paste0(round(x * 100, 1), "%")

# function to get PSE from a model already in tibble format
get_PSE <- function(model, y) {
  y <- model %>% pull(y)
  as.numeric(model[which(abs(y - .5) == min(abs(y - .5))), 1])
}


# function to get posterior probability
get_posterior <- function(
    x,
    mu_1 = mu_d, sigma_1 = var_d^.5, prior_1 = .5,
    mu_2 = mu_t, sigma_2 = var_t^.5, prior_2 = .5,
    # perceptual noise
    sigma_noise = sqrt(var_noise),
    # lapse rate: proportion of trials on which participants do not respond based on stimulus. Take the lapse rate that was fitted in the data
    lapse = plogis(as.numeric(summary(fit_mix)$fixed["theta1_Intercept", 1]))
) {
  density_1 <- dnorm(x, mu_1, sqrt(sigma_1^2 + sigma_noise^2))
  density_2 <- dnorm(x, mu_2, sqrt(sigma_2^2 + sigma_noise^2))

  p_2_based_on_stimulus <- (density_2  * prior_2) / (density_1 * prior_1 + density_2 * prior_2) # Bayes theorem
  p_2_during_lapse <- prior_2 / (prior_1 + prior_2)
  p_2 <- lapse * p_2_during_lapse + (1 - lapse) * p_2_based_on_stimulus

  return(p_2)
}

# Function for calculating CI from logits of a model summary
get_coefficient_fr_model <- function(model, term) {
  round(as.numeric(summary(model)$fixed[term, 1]), 1)
}

get_bf <- function(model, hypothesis) {
  h <- hypothesis(model, hypothesis)[[1]]
  paste0(
    "\\hat{\\beta} = ", round(h$Estimate, 2),
    ",\\ 90\\%{\\rm -CI} = [", round(h$CI.Lower, 3), ", ", round(h$CI.Upper, 3),
    "],\\ BF = ", round(h$Evid.Ratio, 1),
    ",\\ p_{posterior} = ", signif(h$Post.Prob, 3))
}

make_CI <- function(model, term) {
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


logit_to_prob <- function(model, term, index = 1) {
  paste0(round(plogis(as.numeric(summary(model)$fixed[term, index])) * 100, 1), "%")
}

# function to transform Gelman-scaled values back
descale <- function(x, mean, sd) {
  x_0 = (x * 2 * sd) + mean
  return(x_0)
}



get_ChodroffWilson_data <- function(
    database_filename,
    min.n_per_talker_and_stop = 0,
    limits.VOT = c(-Inf, Inf),
    limits.f0 = c(0, Inf),
    max.p_for_multimodality = 1
) {
  require(tidyverse)
  require(magrittr)
  require(diptest)

  d.chodroff_wilson <-
    read_csv(database_filename, show_col_types = FALSE) %>%
    rename(category = stop, VOT = vot, f0 = usef0, Talker = subj, Word = word, Trial = trial, Vowel = vowel, vowel_duration = vdur) %>%
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
    select(Talker, Word, Trial, Vowel, gender, category, poa, voicing, VOT, f0, vowel_duration)

  # Filter VOT and f0 for absolute values to deal with outliers
  d.chodroff_wilson %<>%
    filter(
      between(VOT, min(limits.VOT), max(limits.VOT)),
      between(f0, min(limits.f0), max(limits.f0)))

  # Keep only talkers with at last n.min observations for each stop
  # (this is done both prior to and after the multimodality test in order to avoid low N warnings)
  d.chodroff_wilson %<>%
    group_by(Talker, category) %>%
    mutate(n = length(category)) %>%
    group_by(Talker) %>%
    mutate(n = ifelse(any(is.na(n)), 0, min(n))) %>%
    ungroup() %>%
    filter(n > min.n_per_talker_and_stop)

  # Identify and remove talkers with bimodal f0 distributions
  # (indicating pitch halving/doubling)
  d.chodroff_wilson %<>%
    group_by(Talker) %>%
    mutate(f0_Mel = phonR::normMel(f0)) %>%
    group_by(Talker, category) %>%
    mutate(
      f0_Mel.multimodal = dip.test(f0_Mel)$p.value < max.p_for_multimodality) %>%
    filter(!f0_Mel.multimodal) %>%
    droplevels()

  # Keep only talkers with at least n.min observations for each stop
  d.chodroff_wilson %<>%
    group_by(Talker, category) %>%
    mutate(n = length(category)) %>%
    group_by(Talker) %>%
    mutate(n = ifelse(any(is.na(n)), 0, min(n))) %>%
    ungroup() %>%
    filter(n > min.n_per_talker_and_stop)

  # Get Mel and Semitones, then C-CuRE
  d.chodroff_wilson %<>%
    group_by(Talker) %>%
    mutate(
      f0_semitones = 12 * log(f0 / mean(f0)) / log(2)) %>%
    ungroup()
}

# NORMALIZATION -----------------------------------------------------------
apply_ccure <- function(x, data) {
  require(lme4)
  x - predict(lmer(x ~ 1 + (1 | Talker), data = data), random.only = T)
  # deducts only talker specific intercepts (more precisely, the offset value of each talker's mean from the grand mean) from each cue value
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

# defining experiment cue means and SDs. This is necessary for the following functions to work
chodroff.mean_VOT <- 38.6103
chodroff.mean_f0_Mel <- 237.997
VOT.mean_exp1 <- 47.6304
VOT.sd_exp1 <- 51.7727
f0.mean_exp1 <- 340.923
f0.sd_exp1 <- 2.58267
VOT.mean_test <- 36
f0.mean_test <- 340
VOT.mean_exp2 <- 40.587
VOT.sd_exp2 <- 28.495
f0.mean_exp2 <- 340.24
f0.sd_exp2 <- 1.6484

############################################################################
# Get approximate f0 of synthesised stimuli from VOT values
############################################################################
# set the linear prediction parameters for exposure stimuli
predict_f0 <- function(VOT, intercept = 245.46968, slope = 0.03827, Mel = FALSE) {
  f0 <- intercept + slope * (VOT)
  if (Mel) f0 <- phonR::normMel(f0)
  return(f0)
}

############################################################################
#  function to optimise minimal difference in likelihoods of 2 categories  #
############################################################################
get_diff_in_likelihood_from_io <- function(x, io, add_f0 = F, io.type) {
  # Since we want to only consider cases that have F0 values that are in a certain linear relation to VOT
  # (the way we created our stimuli), we set the F0 based on the VOT.
  if (add_f0) x <- c(x, normMel(predict_f0(x)))
  else if (add_f0 & io.type == "VOT_F0.centered.input") x <- c(x, normMel(predict_f0(x)) + (chodroff.mean_f0_Mel - f0.mean_exp1))
  else if (add_f0 & io.type == "VOT_F0.centered.input_block1") x <- c(x, normMel(predict_f0(x)) + (chodroff.mean_f0_Mel - f0.mean_test))

  # abs(dmvnorm(x, io$mu[[1]], io$Sigma[[1]], log = T) - dmvnorm(x, io$mu[[2]], io$Sigma[[2]], log = T))
  y <- abs(dmvnorm(x, io$mu[[2]], io$Sigma[[2]] + io$Sigma_noise[[2]], log = F) / (dmvnorm(x, io$mu[[1]], io$Sigma[[1]] + io$Sigma_noise[[1]], log = F) + dmvnorm(x, io$mu[[2]], io$Sigma[[2]] + io$Sigma_noise[[2]], log = F)) - .5)

  # message(paste("Explored VOT =", x, "and found p(t) of", y, "\n"))
  return(y)
}

get_PSE_from_io <- function(io, io.type) {
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
    data = d.chodroff_wilson.selected,
    cues,
    groups,
    lapse_rate = 0,
    with_noise = TRUE,
    VOTs = seq(0, 85, .5),
    F0s = normMel(predict_f0(VOTs)),
    alpha = .1,
    linewidth = .3,
    io.type
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
          prior = c(.5, .5),
          lapse_rate = lapse_rate,
          lapse_bias = c(.5, .5),
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
      PSE = map2_dbl(
        io, io.type, ~ get_PSE_from_io(io = .x, io.type = .y)),
      categorization =
        map2(
          x, io,
          ~ get_categorization_from_MVG_ideal_observer(x = .x$x, model = .y, decision_rule = "proportional") %>%
            filter(category == "/t/") %>%
            mutate(VOT = map(x, ~ .x[1]) %>% unlist())),
      line = pmap(
        list(categorization, gender, io.type),
        ~ geom_line(data = ..1,
                    aes(x = if (str_detect(..3, ".*\\.centered\\.input$")) VOT - (chodroff.mean_VOT - VOT.mean_exp1)
                        else if (str_detect(..3, ".*\\.centered\\.input_block1$")) VOT - (chodroff.mean_VOT - VOT.mean_testblock1)
                        else VOT,
                        y =  response,
                        color = ..2),
                    alpha = alpha, linewidth = linewidth)),
      io.type = io.type
    )
}


############################################################################
# function to evaluate the IOs
############################################################################
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


############################################################################
# function to plot IOs in exp1 (section 2.3) & exp2 Block 1
############################################################################
add_psychometric_fit_CI <- function(data.perception){
  geom_ribbon(
    data = data.perception,
    mapping = aes(
      x = Item.VOT,
      ymin = lower__,
      ymax = upper__),
    alpha = .08,
    inherit.aes = F)
}

add_psychometric_fit <- function(data.perception){
  geom_line(
    data = data.perception,
    mapping = aes(x = Item.VOT,
                  y = estimate__),
    linewidth = 1.2,
    colour = "#333333",
    alpha = .8,
    inherit.aes = F)
}

add_PSE_perception_CI <- function(data.percept.PSE){
  geom_errorbarh(
    data = data.percept.PSE %>%
      mutate(y = .018),
    mapping = aes(xmin = .lower, xmax = .upper, y = y),
    color = "#333333",
    height = 0,
    alpha = .5,
    size = .8,
    inherit.aes = F)
}

add_PSE_perception_median <- function(data.percept.PSE){
  geom_point(
    data = data.percept.PSE %>%
      mutate(y = .018),
    mapping = aes(x = PSE, y = y),
    color = "#333333",
    size = 1,
    alpha = .5,
    inherit.aes = F)
}

add_rug <- function(data.test) {
  geom_rug(data = data.test %>%
             ungroup() %>%
             distinct(Item.VOT),
           mapping = aes(x = Item.VOT),
           colour = "grey",
           alpha = .6,
           inherit.aes = F)
}

add_annotations <- function(data.percept.PSE){
  annotate(
    geom = "text",
    x = 70,
    y = .02,
    label = paste(round(data.percept.PSE[[2]]), "ms", "-", round(data.percept.PSE[[3]]), "ms"),
    size = 1.8,
    colour = "darkgray")
}


plot_IO_fit <- function(
    data.production,
    data.perception,
    data.percept.PSE = posterior.sample,
    data.test,
    PSEs
) {
  plot <- ggplot() +
    # geom_ribbon(
    #   data =
    #     data %>%
    #     select(-x) %>%
    #     unnest(categorization) %>%
    #     group_by(gender, x) %>%
    #     summarise(
    #       VOT = map(x, ~ .x[1]) %>% unlist(),
    #       across(response, list("lower" = ~ quantile(.x, .025), "upper" = ~ quantile(.x, .975)))),
    #   mapping = aes(x = VOT, ymin = response_lower, ymax = response_upper, fill = gender),
    #   alpha = .1) +
  data.production$line +
    scale_x_continuous("VOT (ms)", breaks = c(0, 25, 50, 75), limits = c(-15, 85), expand = c(0, 0)) +
    scale_y_continuous('Proportion "t"-responses') +
    scale_colour_manual("Model",
                        values = c(colours.sex),
                        labels = c("IO (female)", "IO (male)"),
                        aesthetics = c("color", "fill")) +
    geom_errorbarh(
      data = PSEs %>%
        mutate(y = ifelse(gender == "male", -.025, - .068)),
      mapping = aes(xmin = PSE.lower, xmax = PSE.upper, y = y, color = gender),
      height = 0, alpha = .5, size = .8) +
    geom_point(
      data = PSEs %>%
        mutate(y = ifelse(gender == "male", -.025, - .068)),
      mapping = aes(x = PSE.median, y = y, color = gender),
      size = 1) +
    annotate(geom = "text",
             y = -.025, x = 70,
             label = paste(PSEs[[1, 2]], "ms", "-", PSEs[[1, 4]], "ms"),
             size = 1.8,
             colour = "#87bdd8") +
    annotate(geom = "text",
             y = -.068, x = 70,
             label = paste(PSEs[[2, 2]], "ms", "-", PSEs[[2, 4]], "ms"),
             size = 1.8,
             colour = "#c1502e")
  plot +
    # add plot specifics of the perception data
    add_psychometric_fit_CI(data.perception) +
    add_psychometric_fit(data.perception) +
    add_PSE_perception_CI(data.percept.PSE) +
    add_PSE_perception_median(data.percept.PSE) +
    add_annotations(data.percept.PSE) +
    add_rug(data.test)
}

############################################################################

get_PSE_quantiles <- function(data, group) {
  data %>%
  group_by(!!! syms(group)) %>%
  summarise(
    PSE.lower = round(quantile(PSE, probs = c(.025))),
    PSE.median = round(quantile(PSE, probs = c(.5))),
    PSE.upper = round(quantile(PSE, probs = c(.975))))
}

############################################################################
# function to prepare variables for modelling
############################################################################
prepVars <- function(d, levels.Condition = NULL, contrast_type) {
  d %<>%
    drop_na(Condition.Exposure, Phase, Block, Item.MinimalPair, ParticipantID, Item.VOT, Response)

  message("VOT mean:", signif(mean(d$Item.VOT, na.rm = T)))
  message("VOT sd:", signif(sd(d$Item.VOT, na.rm = T)))

  d %<>%
    ungroup() %>%
    mutate(
      Block_n = as.numeric(as.character(Block)),
      across(c(Condition.Exposure, Block, Item.MinimalPair), factor),

      Condition.Exposure = factor(Condition.Exposure, levels = levels.Condition)) %>%

    drop_na(Block, Response, Item.VOT) %>%
    mutate(VOT_gs = (Item.VOT - mean(Item.VOT, na.rm = TRUE)) / (2 * sd(Item.VOT, na.rm = TRUE))) %>%
    droplevels()

  message("mean VOT is", mean(d$Item.VOT), "and SD is", sd(d$Item.VOT))

  contrasts(d$Condition.Exposure) <- cbind("_Shift10 vs. Shift0" = c(-2/3, 1/3, 1/3),
                                          "_Shift40 vs. Shift10" = c(-1/3,-1/3, 2/3))
  require(MASS)
  if (all(d$Phase == "test") & n_distinct(d$Block) > 1 & contrast_type == "difference") {
    contrasts(d$Block) <- fractions(contr.sdif(6))
    dimnames(contrasts(d$Block))[[2]] <- c("_Test2 vs. Test1", "_Test3 vs. Test2", "_Test4 vs. Test3", "_Test5 vs. Test4", "_Test6 vs. Test5")

    message(contrasts(d$Condition.Exposure))
    message(contrasts(d$Block))
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
    message(contrasts(d$Block))
  } else {
    message(contrasts(d$Condition.Exposure))
  }

  return(d)
}

### function for non-parametric density plot
# ensuring that a particular proportion of points are included within each contour line
# adjusted from https://stackoverflow.com/questions/75598144/interpretation-of-2d-density-estimate-charts
# we can get the 2d density with MASS::kde2d, then convert to a raster using terra. We can then order the points according to the density in the associated 2d density grid and find the density at which a quantile is passed with approx
density_quantiles <- function(x, y, quantiles) {
  require(terra)
  dens <- MASS::kde2d(x, y, n = 500)
  df   <- cbind(expand.grid(x = dens$x, y = dens$y), z = c(dens$z))
  r    <- terra::rast(df)
  ind  <- sapply(seq_along(x), function(i) cellFromXY(r, cbind(x[i], y[i])))
  ind  <- ind[order(-r[ind][[1]])]
  vals <- r[ind][[1]]
  ret  <- approx(seq_along(ind)/length(ind), vals, xout = quantiles)$y
  replace(ret, is.na(ret), max(r[]))
}


### function for formatting tables
align_tab <- function(hyp) {
  map_chr(hyp, ~ ifelse(class(.x) == "numeric", "r","l"))
}

make_hyp_table <- function(hyp_readable, hyp, caption, col1_width = "15em") {
  cbind(hyp_readable, hyp) %>%
    select(-2) %>%
    mutate(
      across(where(is.numeric), ~ round(., digits = 3)),
      CI = paste0("[", CI.Lower, ", ", CI.Upper, "]")) %>%
    select(-c(CI.Upper, CI.Lower)) %>%
    relocate(CI, .before = "Evid.Ratio") %>%
    kbl(caption = caption, align = align_tab(hyp),
        format = "latex",
        booktabs = TRUE,
        escape = FALSE,
        col.names = c("Hypothesis", "Estimate", "SE", "90\\%-CI", "BF", "$p_{posterior}$")) %>%
    kable_styling(full_width = FALSE) %>%
    column_spec(1, width = col1_width)
}

get_speech_rate_model <- function(data) {
  mean.vowel_duration <- mean(data$vowel_duration)
  sd.vowel_duration <- sd(data$vowel_duration)
  mean.VOT <- mean(data$VOT)
  sd.VOT <- sd(data$VOT)
  # scale variables
  data %>%
    mutate(
      VOT.scaled = (VOT - mean.VOT)/sd.VOT,
      vowel_duration.scaled = (vowel_duration - mean.vowel_duration)/sd.vowel_duration
    )

  m <- lmer(VOT.scaled ~ 1 + vowel_duration.scaled + (1 + vowel_duration.scaled | Talker), data = data)
  tidy(m, effects = "fixed")
}

# speech rate correction
get_speech.corrected.VOT <- function(data){
  mean.vowel_duration <- mean(data$vowel_duration)
  sd.vowel_duration <- sd(data$vowel_duration)
  mean.VOT <- mean(data$VOT)
  sd.VOT <- sd(data$VOT)
  # scale variables
  data %<>%
    mutate(
      VOT.scaled = (VOT - mean.VOT)/sd.VOT,
      vowel_duration.scaled = (vowel_duration - mean.vowel_duration)/sd.vowel_duration
    )

  m <- lmer(VOT.scaled ~ 1 + vowel_duration.scaled + (1 + vowel_duration.scaled | Talker), data = data)
  intercept <- fixef(m)[[1]]
  slope <- fixef(m)[[2]]

  data %>% mutate(
    VOT.predict_scaled = predict(m),
    VOT.resid_scaled = VOT.scaled - VOT.predict_scaled,
    VOT.speech_corrected.scaled = VOT.resid_scaled + intercept,
    VOT.speech_corrected = (VOT.speech_corrected.scaled * sd.VOT) + mean.VOT
  )
}
