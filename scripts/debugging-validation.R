add_epred_draws(d.test_exposure_for_analysis %>% 
                    filter(Phase == "test") %>% 
                    prepVars(levels.Condition = levels_Condition.Exposure, contrast_type = "difference") %>% 
                    distinct(Block, Condition.Exposure) %>% 
                    mutate(VOT_gs = 0), 
                  fit_mix_uniform_bias,
                  ndraws = 4000, re_formula = NA) %>% group_by(Block, Condition.Exposure) %>% summarise(mean(.epred)) -> x
x


add_epred_draws(d.test_exposure_for_analysis %>% 
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


d.test_exposure_for_analysis %>%
           group_by(Phase, Block) %>%
           mutate(
             Block = factor(case_when(
               Block == 1 ~ "Test 1",
               Block == 3 ~ "Test 2",
               Block == 5 ~ "Test 3",
               Block == 7 ~ "Test 4",
               Block == 8 ~ "Test 5",
               Block == 9 ~ "Test 6",
               Block == 2 ~ "Exposure 1",
               Block == 4 ~ "Exposure 2",
               Block == 6 ~ "Exposure 3")),
             Block = fct_relevel(Block, c("Test 1", "Exposure 1", "Test 2", "Exposure 2", "Test 3", "Exposure 3",  "Test 4", "Test 5", "Test 6"))) %>%
           distinct(Item.VOT)















