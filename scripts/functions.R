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
make_CI <- function(model, coef, hypothesis) {
  paste0(paste(round(plogis(as.numeric(summary(model)$fixed[coef, 1])) * 100, 1), "%, "), " 95%-CI: ", 
         paste(round(plogis(as.numeric(summary(model)$fixed[coef, 3:4])) * 100, 1), collapse = " to "), "%", "; ", get_bf(model = model, hypothesis = hypothesis))
}

# Function to get identity CI of a model summary
get_CI <- function(model, coef, hypothesis) {
  paste0(round(as.numeric(summary(model)$fixed[coef, 1]), 1), " 95%-CI: ", 
         paste(round(as.numeric(summary(model)$fixed[coef, 3:4]), 1), collapse = " to "),
         "; ",
         get_bf(model = model, hypothesis = hypothesis))
}

logit_to_prob <- function(model, coef, index = 1) {
  paste0(round(plogis(as.numeric(summary(model)$fixed[coef, index])) * 100, 1), "%")
}

get_bf <- function(model, hypothesis) {
  paste0("Bayes factor: ", round(hypothesis(model, hypothesis)[[1]]$Evid.Ratio, 2), " 90%-CI : ", round(hypothesis(model, hypothesis)[[1]]$CI.Lower, 2), " to ", round(hypothesis(model, hypothesis)[[1]]$CI.Upper, 2) )
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
    rename(category = stop, VOT = vot, f0 = usef0, Talker = subj, Word = word, Trial = trial, Vowel = vowel) %>%
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
    select(Talker, Word, Trial, Vowel, gender, category, poa, voicing, VOT, f0)
  
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

############################################################################
# Get approximate f0 of synthesised stimuli from VOT values
############################################################################
predict_f0 <- function(VOT) {
  predict_f0 = 245.46968 + 0.03827 * (VOT)
  return(predict_f0)
}
############################################################################


############################################################################
#  function to optimise minimal difference in likelihoods of 2 categories  #
############################################################################
get_diff_in_likelihood_from_io <- function(x, io, add_f0 = F) {
  # Since we want to only consider cases that have F0 values that are in a certain linear relation to VOT 
  # (the way we created our stimuli), we set the F0 based on the VOT.
  if (add_f0) x <- c(x, normMel(predict_f0(x)))
  
  abs(dmvnorm(x, io$mu[[1]], io$Sigma[[1]], log = T) - dmvnorm(x, io$mu[[2]], io$Sigma[[2]], log = T))
}

get_PSE_from_io <- function(io) {
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
  
  # Find and return values that minimize the difference in log-likelihoods
  optim(
    par = pars[1], 
    fn = get_diff_in_likelihood_from_io, 
    method = "L-BFGS-B",
    control = list(factr = 10^-10),
    lower = min.pars[1], 
    upper = max.pars[1], 
    io = io,
    add_f0 = length(io$mu[[1]]) > 1)$par
}
############################################################################


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
    lapse_rate = plogis(summary(fit_mix)$fixed[3, 1]),
    with_noise = TRUE,
    VOTs = seq(0, 85, .5),            
    F0s = normMel(predict_f0(VOTs)),                      
    alpha = .2,
    size = .5,
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
            if(with_noise == FALSE) {
              matrix(c(0), ncol = 1, dimnames = list(cues, cues))
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
      line = map2(categorization, gender, ~ geom_line(data = .x, aes(x = VOT, y = response, color = .y), alpha = alpha, size = size)),
      io.type = io.type
    )
}  
############################################################################


############################################################################
# function to evaluate the IOs
############################################################################
get_average_accuracy_of_IO <- function(observations, responses, model) {
  get_categorization_from_MVG_ideal_observer(x = observations, model = model, decision_rule = "proportional") %>%
    # we only need one posterior since the other one is simply 1-that
    filter(category == "/t/") %>%
    mutate(
      human_response = .env$responses,
      accuracy = ifelse(category == human_response, response, 1 - response)) %>%
    summarize(mean_accuracy = mean(accuracy))
}

get_average_log_likelihood_of_perception_data_under_IO <- function(observations, responses, model) {
  get_categorization_from_MVG_ideal_observer(x = observations, model = model, decision_rule = "proportional") %>%
    # we only need one posterior since the other one is simply 1-that
    filter(category == "/t/") %>%
    mutate(
      human_response = .env$responses,
      likelihood = ifelse(category == human_response, response, 1 - response)) %>%
    summarize(log_likelihood_per_response = mean(log(likelihood), na.rm = T))
}
############################################################################


############################################################################
# function to plot IOs in experiment 1 (section 2.3) 
############################################################################
plot_IO_fit <- function(
    data,
    PSEs,
    x,
    centered = FALSE
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
  data$line + 
    scale_x_continuous("VOT (msec)", breaks = scales::pretty_breaks(n = 3), limits = c(-15, 85), expand = c(0, 0)) +
    scale_y_continuous('Proportion "t"-responses') +
    scale_colour_manual("Model", 
                        values = c(colours.sex), 
                        labels = c("IO (female)", "IO (male)"),
                        aesthetics = c("color", "fill")) +
    geom_errorbarh(
      data = PSEs %>%
        mutate(y = ifelse(gender == "male", -.025, - .06)),
      mapping = aes(xmin = PSE.lower, xmax = PSE.upper, y = y, color = gender),
      height = 0, alpha = .5, size = 1) +
    geom_point(
      data = PSEs %>%
        mutate(y = ifelse(gender == "male", -.025, - .06)),
      mapping = aes(x = PSE.median, y = y, color = gender),
      size = 1.2) +
    annotate(geom = "text",
             y = -.025, x = 70,
             label = paste(PSEs[[1, 2]], "ms", "-", PSEs[[1, 4]], "ms"),
             size = 1.8,
             colour = "#87bdd8") +
    annotate(geom = "text",
             y = -.06, x = 70,
             label = paste(PSEs[[2, 2]], "ms", "-", PSEs[[2, 4]], "ms"),
             size = 1.8,
             colour = "#c1502e") 
  plot + 
  geom_line(
    data = psychometric_fit_data,
    mapping = aes(x = x, 
                  y = estimate__),
    colour = "#333333", 
    size = 1,
    alpha = .8,
    inherit.aes = F) +
    geom_ribbon(
      data = psychometric_fit_data, 
      mapping = aes(x = x, 
                    ymin = lower__, 
                    ymax = upper__),
      alpha = .08,
      inherit.aes = F) +
    geom_errorbarh(
      data = post_sample_norm %>% 
        mutate(y = .01),
      mapping = aes(xmin =  if (centered != FALSE) .lower + (chodroff.mean_VOT - VOT.mean_norm) else .lower, 
                    xmax =  if (centered != FALSE) .upper + (chodroff.mean_VOT - VOT.mean_norm) else .upper, 
                    y = y), 
      color = "#333333",
      height = 0,
      alpha = .5,
      size = 1) +
    geom_point(
      data = post_sample_norm %>% 
        mutate(y = .01),
      mapping = aes(x = if (centered != FALSE) PSE + (chodroff.mean_VOT - VOT.mean_norm) else PSE, y = y), 
      color = "#333333", size = 1.3) +
    annotate(
      geom = "text", 
      y = .02, x = 70,
      label = if (centered != FALSE) paste(round(post_sample_norm[[2]] + (chodroff.mean_VOT - VOT.mean_norm)), "ms", "-", round(post_sample_norm[[3]] + (chodroff.mean_VOT - VOT.mean_norm)), "ms") else
        paste(round(post_sample_norm[[2]]), "ms", "-", round(post_sample_norm[[3]]), "ms"),
      size = 1.8) +
    geom_rug(
      data = d.test.excluded %>% 
        ungroup() %>% 
        distinct(Item.VOT),
      mapping = aes(x = Item.VOT),
      colour = "grey",
      alpha = .6,
      inherit.aes = F)
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
  
