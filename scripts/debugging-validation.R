add_epred_draws(d_for_analysis %>%
                    filter(Phase == "test") %>%
                    prepVars(levels.Condition = levels_Condition.Exposure, contrast_type = "difference") %>%
                    distinct(Block, Condition.Exposure) %>%
                    mutate(VOT_gs = 0),
                  fit_mix_test,
                  ndraws = 4000, re_formula = NA) %>% group_by(Block, Condition.Exposure) %>% summarise(mean(.epred)) -> x
x


add_epred_draws(d_for_analysis %>%
                    filter(Phase == "test") %>%
                    prepVars(levels.Condition = levels_Condition.Exposure, contrast_type = "helmert") %>%
                    distinct(Block, Condition.Exposure) %>%
                    mutate(VOT_gs = 0), fit_mix_helmert,
                  ndraws = 4000, re_formula = NA) %>% group_by(Block, Condition.Exposure) %>% summarise(mean(.epred)) -> y

y
x
# minimal example to validate sliding difference coding effects
nlevel <- 3
d <-
  crossing(
    Block = factor(1:nlevel),
    Condition = factor(LETTERS[1:nlevel]))

contrasts(d$Block) <- MASS::contr.sdif(nlevel)
colnames(contrasts(d$Block)) <- c("2vs1", "3vs2")
contrasts(d$Condition) <- MASS::contr.sdif(nlevel)
colnames(contrasts(d$Condition)) <- c("BvsA", "CvsB")

X <- model.matrix(~ Block * Condition, data = d)
beta <- c(1:9)
# this multiplies the contrast design, X with the vector of regression coefficients (where the first number represents the coeff for the intercept)
# %*% is the matrix multiplication operator or dot product
d$y <- X %*% beta

# Cell means by Block (from which we can infer the main effects of Block)
d %>%
  group_by(Block) %>%
  summarise(mean(y))

# Cell means by Condition (from which we can infer the main effects of Condition)
d %>%
  group_by(Condition) %>%
  summarise(mean(y))

# Interaction between Block 2vs1 and Condition BvsA
d %>%
  filter(Block %in% c(1, 2), Condition %in% c("A", "B"))
# In Block 1, Cond B vs. A = -4.67
# In Block 2, Cond B vs. A = +7.33
# Interaction = 6

# Interaction between Block 3vs2 and Condition BvsA
d %>%
  filter(Block %in% c(2, 3), Condition %in% c("A", "B"))
# In Block 2, Cond B vs. A = 3.663
# In Block 3, Cond B vs. A = 10.67
# Interaction = 7


# Interaction between Block 3vs1 and Condition BvsA
d %>%
  filter(Block %in% c(1, 3), Condition %in% c("A", "B"))
# In Block 1, Cond B vs. A = -2.33
# In Block 3, Cond B vs. A = 10.67
# Interaction = 13



# Interaction between Block 3vs2 and Condition CvsA
d %>%
  filter(Block %in% c(3, 1), Condition %in% c("A", "C"))
# In Block 1, Cond C vs. A = -5.66
# In Block 3, Cond C vs. A = 24.3
# Interaction = 30


# Interaction between Block 2vs1 and Condition CvsA
d %>%
  filter(Block %in% c(1, 2), Condition %in% c("A", "C"))
# In Block 1, Cond C vs. A = -5.66
# In Block 2, Cond C vs. A = 8.33
# Interaction = 14


# Simple effect coding
X <- model.matrix(~ Block / Condition, data = d)
beta <- c(1:9) * 2
d$y <- X %*% beta

# Cell means by Block (from which we can infer the main effects of Block)
d %>%
  group_by(Block) %>%
  summarise(mean(y))


# Cell means by Condition (from which we can infer the main effects of Condition)
d %>%
  group_by(Condition) %>%
  summarise(mean(y))

# Interaction between Block 2vs1 and Condition BvsA
d %>%
  filter(Block %in% c(1, 2), Condition %in% c("A", "B"))
# In Block 1, Cond B vs. A = +4.67
# In Block 2, Cond B vs. A = +7.33
# Interaction = +2.67



p.intercept <-
  d.estimates %>%
  ggplot(aes(x = Block, y = Intercept_mean, colour = Condition.Exposure, group = Condition.Exposure)) +
  geom_rect(aes(ymin = -Inf, ymax = Inf, xmin = 0.75, xmax = 1.25),
            fill = "grey",
            alpha = .009,
            inherit.aes = F) +
  geom_rect(aes(ymin = -Inf, ymax = Inf, xmin = 2.75, xmax = 3.25),
            fill = "grey",
            alpha = .009,
            inherit.aes = F) +
  geom_rect(aes(ymin = -Inf, ymax = Inf, xmin = 4.75, xmax = 5.25),
            fill = "grey",
            alpha = .009,
            inherit.aes = F) +
  geom_rect(aes(ymin = -Inf, ymax = Inf, xmin = 6.75, xmax = 7.25),
            fill = "grey",
            alpha = .009,
            inherit.aes = F) +
  geom_rect(aes(ymin = -Inf, ymax = Inf, xmin = 7.75, xmax = 8.25),
            fill = "grey",
            alpha = .009,
            inherit.aes = F) +
  geom_rect(aes(ymin = -Inf, ymax = Inf, xmin = 8.75, xmax = 9.25),
            fill = "grey",
            alpha = .009,
            inherit.aes = F) +
  geom_point(position = position_dodge(.3), size = 1) +
  geom_linerange(aes(ymin = Intercept_lower, ymax = Intercept_upper), linewidth = .6, position = position_dodge(.3), alpha = .5) +
  stat_summary(geom = "line", position = position_dodge(.3)) +
  geom_hline(yintercept = d.io.categorisation[[7]][1], linetype = 2, linewidth = 0.8, colour = "#cc0000", alpha = 0.5) +
  geom_hline(yintercept = d.io.categorisation[[7]][2], linetype = 2, linewidth = .8, colour = "#12D432", alpha = 0.5) +
  geom_hline(yintercept = d.io.categorisation[[7]][3], linetype = 2, linewidth = .8, colour = "#0481F3", alpha = 0.5) +
  scale_colour_manual("Condition",
                      labels = c("baseline", "+10ms", "+40ms"),
                      values = c("#cc0000", "#12D432","#0481F3"),
                      aesthetics = "color") +
  scale_y_continuous("Intercept") +
  scale_x_discrete("Block", labels = c("1" = "Test 1", "2" = "Exposure 1", "3" = "Test 2", "4" = "Exposure 2", "5" = "Test 3", "6" = "Exposure 3", "7" = "Test 4", "8" = "Test 5", "9" = "Test 6")) +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 22.5, hjust = 1))

p.slope_1to7 <-
  d.estimates %>%
  ggplot(aes(x = Block, y = slope_mean,
             colour = Condition.Exposure,
             group = Condition.Exposure)) +
  geom_rect(aes(ymin = -Inf, ymax = Inf, xmin = 0.55, xmax = 1.45),
            fill = "grey",
            alpha = .009,
            inherit.aes = F) +
  geom_rect(aes(ymin = -Inf, ymax = Inf, xmin = 2.55, xmax = 3.45),
            fill = "grey",
            alpha = .009,
            inherit.aes = F) +
  geom_rect(aes(ymin = -Inf, ymax = Inf, xmin = 4.55, xmax = 5.45),
            fill = "grey",
            alpha = .009,
            inherit.aes = F) +
  geom_rect(aes(ymin = -Inf, ymax = Inf, xmin = 6.55, xmax = 7.45),
            fill = "grey",
            alpha = .009,
            inherit.aes = F) +
  geom_rect(aes(ymin = -Inf, ymax = Inf, xmin = 7.55, xmax = 8.45),
            fill = "grey",
            alpha = .009,
            inherit.aes = F) +
  geom_rect(aes(ymin = -Inf, ymax = Inf, xmin = 8.55, xmax = 9.45),
            fill = "grey",
            alpha = .009,
            inherit.aes = F) +
  geom_point(position = position_dodge(.3), size = 1) +
  geom_linerange(aes(ymin = slope_lower, ymax = slope_upper), linewidth = .6, position = position_dodge(.3), alpha = .5) +
  stat_summary(geom = "line", position = position_dodge(.3)) +
  geom_hline(yintercept = 23, linetype = 2, linewidth = 0.8, colour = "#cc0000", alpha = 0.5) +
  geom_hline(yintercept = 23, linetype = 2, linewidth = .8, colour = "#12D432", alpha = 0.5) +
  geom_hline(yintercept = 23, linetype = 2, linewidth = .8, colour = "#0481F3", alpha = 0.5) +
  scale_colour_manual("Condition",
                      labels = c("baseline", "+10ms", "+40ms"),
                      values = c("#cc0000", "#12D432","#0481F3"),
                      aesthetics = "color") +
  scale_y_continuous("Slope") +
  scale_x_discrete("Block", labels = c("1" = "Test 1", "2" = "Exposure 1", "3" = "Test 2", "4" = "Exposure 2", "5" = "Test 3", "6" = "Exposure 3", "7" = "Test 4", "8" = "Test 5", "9" = "Test 6")) +
  theme(legend.position = "top",
        legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
        legend.box.just = "right",
        legend.justification = c(1.3, 0),
        legend.box.spacing = unit(0, "cm"),
        legend.text = element_text(size = 6, lineheight = 0),
        legend.title = element_text(size = 6),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank())

p.PSE_1to7 <-d.estimates %>%
  ggplot(aes(x = Block, y = PSE_mean, colour = Condition.Exposure, group = Condition.Exposure)) +
  geom_rect(aes(ymin = -Inf, ymax = Inf, xmin = 0.55, xmax = 1.45),
            fill = "grey",
            alpha = .009,
            inherit.aes = F) +
  geom_rect(aes(ymin = -Inf, ymax = Inf, xmin = 2.55, xmax = 3.45),
            fill = "grey",
            alpha = .009,
            inherit.aes = F) +
  geom_rect(aes(ymin = -Inf, ymax = Inf, xmin = 4.55, xmax = 5.45),
            fill = "grey",
            alpha = .009,
            inherit.aes = F) +
  geom_rect(aes(ymin = -Inf, ymax = Inf, xmin = 6.55, xmax = 7.45),
            fill = "grey",
            alpha = .009,
            inherit.aes = F) +
  geom_rect(aes(ymin = -Inf, ymax = Inf, xmin = 7.55, xmax = 8.45),
            fill = "grey",
            alpha = .009,
            inherit.aes = F) +
  geom_rect(aes(ymin = -Inf, ymax = Inf, xmin = 8.55, xmax = 9.45),
            fill = "grey",
            alpha = .009,
            inherit.aes = F) +
  geom_point(position = position_dodge(.3), size = 1) +
  geom_linerange(aes(ymin = PSE_lower, ymax = PSE_upper), linewidth = .6, position = position_dodge(.3), alpha = .5) +
  stat_summary(geom = "line", position = position_dodge(.3)) +
  geom_hline(yintercept = d.io.categorisation[[9]][1], linetype = 2, linewidth = 0.8, colour = "#cc0000", alpha = 0.5) +
  geom_hline(yintercept = d.io.categorisation[[9]][2], linetype = 2, linewidth = .8, colour = "#12D432", alpha = 0.5) +
  geom_hline(yintercept = d.io.categorisation[[9]][3], linetype = 2, linewidth = .8, colour = "#0481F3", alpha = 0.5) +
  scale_colour_manual("Condition",
                      labels = c("baseline", "+10ms", "+40ms"),
                      values = c("#cc0000", "#12D432","#0481F3"),
                      aesthetics = "color") +
  scale_y_continuous("PSE") +
  scale_x_discrete("Block", labels = c("1" = "Test 1", "2" = "Exposure 1", "3" = "Test 2", "4" = "Exposure 2", "5" = "Test 3", "6" = "Exposure 3", "7" = "Test 4", "8" = "Test 5", "9" = "Test 6")) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 22.5, hjust = 1)) +
  guides(colour = "none")



p.params <- (p.slope_1to7 / p.PSE_1to7) +
  plot_layout(guides = "collect") &
  theme(legend.position = "top", 
        axis.text = element_text(size = 8))



p.histogram_conditions <- 
  d.exposure_trials %>% 
  na.omit() %>% 
  filter(image_selection == "forward" & list_LSQ_variant == "A") %>%
  mutate(block = case_when(
    block == 2 ~ "Block A",
    block == 4 ~ "Block B",
    block == 6 ~ "Block C")) %>% 
  ggplot() +
  geom_histogram(aes(x = VOT, fill = paste(condition, category, labelling), 
                     color = paste(condition, category, labelling),
                     linetype = labelling), 
                 alpha = .8) +
  scale_colour_manual(
    "Labelling",
    values = c(
      "baseline /d/ labeled" = "#800000", 
      "baseline /d/ unlabeled" = "#ff9999",
      "baseline /t/ labeled" = "#cc0000", 
      "baseline /t/ unlabeled" = "#ffe6e6",
      "+10ms /d/ labeled" = "#0a751c",
      "+10ms /d/ unlabeled" = "#b9f9c3",
      "+10ms /t/ labeled" = "#12D432",
      "+10ms /t/ unlabeled" = "#e8fdeb",
      "+40ms /d/ labeled" = "#02427e", 
      "+40ms /d/ unlabeled" = "#b4dafe",
      "+40ms /t/ labeled" = "#0481F3", 
      "+40ms /t/ unlabeled" = "#e6f3ff"),
    aesthetics = c("color", "category", "fill"),
    labels = c("/d/ labeled", "/d/ unlabeled", "/t/ labeled", "/t/ unlabeled", "", "", "", "", "", "", "", "")) +
  guides(colour = guide_legend(
    override.aes = list(
      colour = c("#383838", "#C0C0C0", "#606060", "#F0F0F0", 0, 0, 0, 0, 0, 0, 0, 0),
      fill = c("#383838", "#C0C0C0", "#606060", "#F0F0F0", 0, 0, 0, 0, 0, 0, 0, 0),
      linetype = c(2, 1, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0),
      values = c("/d/ labeled", "/d/ unlabeled", "/t/ labeled", "/t/ unlabeled", 0, 0, 0, 0, 0, 0, 0, 0)), nrow = 2)) +
  scale_x_continuous("VOT (ms)", breaks = seq(-50, 150, 30)) +
  scale_y_continuous("Count") +
  facet_grid(condition ~ block, scales = "free_y", margins = F) +
  guides(linetype = "none") +
  theme(
    legend.position = "top",
    strip.text.y = element_blank())

p.histogram_conditions


p.histo_true_shift <- d.exposure_trials %>% 
  na.omit() %>% 
  filter(image_selection == "forward" & list_LSQ_variant == "A") %>%
  ggplot() +
  geom_histogram(aes(
    x = VOT, fill = paste(condition, category, labelling), 
    color = paste(condition, category, labelling),
    linetype = labelling), 
    alpha = .8) +
  scale_colour_manual(
    "Labelling",
    values = c(
      "baseline /d/ labeled" = "#800000", 
      "baseline /d/ unlabeled" = "#ff9999",
      "baseline /t/ labeled" = "#cc0000", 
      "baseline /t/ unlabeled" = "#ffe6e6",
      "+10ms /d/ labeled" = "#0a751c",
      "+10ms /d/ unlabeled" = "#b9f9c3",
      "+10ms /t/ labeled" = "#12D432",
      "+10ms /t/ unlabeled" = "#e8fdeb",
      "+40ms /d/ labeled" = "#02427e", 
      "+40ms /d/ unlabeled" = "#b4dafe",
      "+40ms /t/ labeled" = "#0481F3", 
      "+40ms /t/ unlabeled" = "#e6f3ff"),
    aesthetics = c("color", "category", "fill"),
    labels = c("/d/ labeled", "/d/ unlabeled", "/t/ labeled", "/t/ unlabeled", "", "", "", "", "", "", "", "")) +
  guides(colour = guide_legend(
    override.aes = list(
      colour = c("#383838", "#C0C0C0", "#606060", "#F0F0F0", 0, 0, 0, 0, 0, 0, 0, 0),
      fill = c("#383838", "#C0C0C0", "#606060", "#F0F0F0", 0, 0, 0, 0, 0, 0, 0, 0),
      linetype = c(2, 1, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0),
      values = c("/d/ labeled", "/d/ unlabeled", "/t/ labeled", "/t/ unlabeled", 0, 0, 0, 0, 0, 0, 0, 0)), nrow = 2)) +
  stat_function(fun = function(x) 72 * 5 * dnorm(x, 25, sqrt(var_d)),
                color = "black", linewidth = .6, alpha = .7, linetype = 2) +
  stat_function(
    fun = function(x) 72 * 5 * dnorm(x, 70, sqrt(var_t)),
    color = "black", linewidth = .6, alpha = .5, linetype = 2) +
  geom_rug(data = tibble(VOT = c(25, 70)), aes(x = VOT), sides = "t") +
  scale_x_continuous("VOT (ms)", breaks = seq(-50, 150, 30)) +
  scale_y_continuous("Count") +
  geom_text(data = d.means,
            aes(x = 103, 
                y = 17,
                label = paste("mean", category, "=", mean)),
            size = 3,
            position = position_dodge2v(height = -8),
            inherit.aes = F) +
  facet_grid(. ~ condition, scales = "free_y", margins = F) +
  guides(linetype = "none") +
  theme(legend.position = "top",
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 6),
        legend.justification = c(1.3, 0),
        legend.box.just = "right",
        legend.key.width = unit(12, "pt"),
        legend.key.height = unit(12, "pt"),
        legend.box.spacing = unit(1, "pt"))

p.histo_true_shift

ggsave("p.slope_1to7.png", p.slope_1to7, width = 12, height = 6.5, units = "cm", path = "~/Desktop/")
ggsave("p.PSE_1to7.png", p.PSE_1to7, width = 12, height = 7, units = "cm", path = "~/Desktop/")
ggsave("p.params.png", p.params, width = 14, height = 12, units = "cm", path = "~/Desktop/")

ggsave("p.histo_true_shift.png", p.histo_true_shift, width = 16.5, height = 8, units = "cm", path = "~/Desktop/")



mean.vowel_duration <- mean(d.chodroff_wilson.selected$vowel_duration)
sd.vowel_duration <- sd(d.chodroff_wilson.selected$vowel_duration)
mean.VOT <- mean(d.chodroff_wilson.selected$VOT)
sd.VOT <- sd(d.chodroff_wilson.selected$VOT)

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




d.temp <- tibble(VOT = d.chodroff_wilson.selected$VOT,
                 VOT.speech_corrected = d.chodroff_wilson.selected$VOT.speech_corrected,
                   Talker = factor(d.chodroff_wilson.selected$Talker))

d.temp %>% group_by(Talker) %>% 
  mutate(VOT.speech_corrected_centered = apply_ccure(VOT.speech_corrected, data = .))

lmer(VOT ~ 1 + (1 | Talker), data = d.temp)

mean(d.temp$VOT.speech_corrected)
mean(d.temp$VOT)

d.temp %>% 
  ggplot(aes(y = VOT.speech_corrected, x = vowel_duration)) +
  geom_point(alpha = .1) 



description <- tibble(
  label = c("Lorem ipsum dolor **sit amet,** consectetur adipiscing elit,
    sed do *eiusmod tempor incididunt* ut labore et dolore magna
    aliqua.", "More description about the PSEs and predicted slopes. A naive Bayesian learner is expected to converged on the dashed lines... etc..."),
  x = c(0.05, .65),
  y = c(.5, .5),
  hjust = c(0, 0),
  vjust = c(1, 1),
  orientation = c("upright", "upright"),
  color = c("black", "blue"),
  fill = c("cornsilk", "white"))

my_text <- 
  description %>% 
  ggplot() +
  aes(x, y, label = label, colour = color, fill = fill,
      hjust = hjust, vjust = vjust, 
      orientation = orientation) +
  geom_textbox(
    width = unit(7, "cm") 
  ) +
  scale_discrete_identity(aesthetics = c("color", "fill", "orientation")) +
  xlim(0, 1) + ylim(0, 1) +
  theme_void() +
  remove_axes_titles









