

#####################################################################################

#' YOU DO NOT NEED TO RUN THIS SCRIPT FOR THE WORKSHOP. THIS SCRIPT IS ONLY SUPPLIED
#' AS A RECORD OF HOW THE REALISTIC DATA USED THROUGHOUT WAS SIMULATED. SIMULATION
#' CAN BE VERY USEFUL FOR LEARNING HOW UNDERLYING STATISTICAL PROCESSES WORK.
#' 
#' THIS CODE WAS WRITTEN BY DR. CALUM WEBB, THE UNIVERSITY OF SHEFFIELD

#####################################################################################



library(tidyverse)
library(faux)
library(lavaan)
library(simsem)

n <- 5e3 

# Manifest variables of family stress: 
#   - In the last month, felt that caring for children takes up more time and energy than
#     they have to give?
#   - In the last month, ever had an argument with a co-parent about their parenting 
#     practices that didn't lead to a good resolution?
#   - In the last month, felt like the responsibilities of being a parent are overwhelming?
#   - In the last week, felt like co-parent doesn't do enough to help with raising child(ren)?
#   - In the last month, had an argument with a co-parent in front of the children?

set.seed(1630)
mvsim_dat <- faux::rnorm_multi(
  n = n,
  mu = c(
    stress_lv = 0,
    stress_lv2 = 0, # hidden but related factor to avoid perfect fit!
    income     = 0,
    investment = 0
  ),
  r = c(  1,     0.95,  -0.6,  -0.3,
          0.95,     1,  -0.6,  -0.3,
          -0.6, -0.6,     1,  0.75,
          -0.3, -0.3, 0.75,     1 
  ),
  varnames = c("stress_lv", "stress_lv2", "income", "investment")
)

# simulate question responses (r truncated norm and then rounded)

stress_q1 <- numeric()
stress_q2 <- numeric()
stress_q3 <- numeric()
stress_q4 <- numeric()
stress_q5 <- numeric()

set.seed(80)
for (i in 1:n) {
  
  stress_q1[i] <- MASS::mvrnorm(n = 1, mu = 0 + 0.95 * mvsim_dat$stress_lv[i], Sigma = 1-0.95^2) 
  stress_q2[i] <- MASS::mvrnorm(n = 1, mu = 0 + 0.8 * mvsim_dat$stress_lv2[i], Sigma = 1-0.8^2) 
  stress_q3[i] <- MASS::mvrnorm(n = 1, mu = 0 + 0.7 * mvsim_dat$stress_lv[i], Sigma = 1-0.7^2) 
  stress_q4[i] <- MASS::mvrnorm(n = 1, mu = 0 + 0.85 * mvsim_dat$stress_lv2[i], Sigma = 1-0.85^2) 
  stress_q5[i] <- MASS::mvrnorm(n = 1, mu = 0 + 0.9 * mvsim_dat$stress_lv2[i], Sigma = 1-0.9^2) 
  
}


sim_dat <- tibble(
  stress_q1,
  stress_q2,
  stress_q3,
  stress_q4,
  stress_q5,
  income = mvsim_dat$income,
  investment = mvsim_dat$investment
)

# Check means = 0 and std = 1

sim_dat %>%
  summarise_all(list(mean=mean, sd=sd)) %>%
  pivot_longer(everything()) %>%
  print(n = 40)


# Turn standardised scores back into 10 point scale, truncated and rounded
sim_dat <- sim_dat %>%
  mutate_at(
    vars(stress_q1:stress_q5),
    ~ round((scale(.) * 3) + 5, 0)
  ) %>%
  mutate_at(
    vars(stress_q1:stress_q5),
    ~ ifelse(. > 10, 10, ifelse(. < 0, 0, .))
  )


cfa_mod <- "s =~ stress_q1 + stress_q2 + stress_q3 + stress_q4 + stress_q5 
"

stress_cfa <- sem(cfa_mod, 
                  data = sim_dat) 

summary(stress_cfa, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


# Generate further outcomes

stress_lv <- lavPredict(stress_cfa)[,1]

# Stirling Children's Wellbeing Scale: truncated discrete normal between 12 and 60, mean = 44
scws <- numeric()
for (i in 1:n) {
  scws[i] <- round(rtnorm(n = 1, mean = 44 + (-3 * scale(stress_lv)[i]) + (1*scale(sim_dat$income)[i]) + (2.5*scale(sim_dat$investment)[i]), sd = 5, upper = 60, lower = 12), 0)
}
  
dat1 <- tibble(
  std_income = sim_dat$income,
  std_investment = sim_dat$investment,
  stress_q1 = sim_dat$stress_q1[,1],
  stress_q2 = sim_dat$stress_q2[,1],
  stress_q3 = sim_dat$stress_q3[,1],
  stress_q4 = sim_dat$stress_q4[,1],
  stress_q5 = sim_dat$stress_q5[,1],
  scws = scws
)

# un-standardise income and investment
dat1 <- dat1 %>%
  mutate(
    income = (std_income * 6) + 15,
    investment = (std_investment * 3) + 7
  )

cov(dat1$std_income, dat1$std_investment)
cov(dat1$income, dat1$investment)
sd(dat1$income)
sd(dat1$investment)
sd(dat1$scws)

##### Higher investment leading to lower scws for some reason?

lm(data = dat1, formula = scale(scws) ~ scale(income) + scale(investment) + scale(stress_lv)) %>% summary()

sem_mod <- "

# Create the latent variable family stress (note: NA* is used to free)
# factor loading, and later stress ~~ resvar*stress is used to standardise
# the latent variable so that it has a mean of 0 and standard deviation of 1
stress =~ NA*stress_q1 + stress_q2 + stress_q3 + stress_q4 + stress_q5

# Regress stress on income and investment, and label the paths a1 & a2
stress ~ a1*income + a2*investment

# Regress investment on income and label the path a3
investment ~ a3*income

# Regress child wellbeing on the Stirling Child Wellbeing Score on stress, 
# income, and investment, label paths b1, c1, and b2
scws ~ b1*stress + c1*income + b2*investment

# make stress latent variable standardised (total variance = 1), 
# residual variance = 1 - beta^2
stress ~~ resvar*stress

# Manual calculation of standardised coefficients
# (note: you can estimate the model and use 1 - the R-squared, then
# add this manually for calculating resvar, or use std.lv = TRUE)
# standardised coefficients = beta*sd(x)/sd(y)
# covariance between predictors = 2*beta1*cov(x1,x2)*a2
std_a1 := a1*(5.044198/1)
std_a2 := a2*(3.008222/1)
std_a3 := a3*(5.044198/3.008222)
std_b1 := b1*(1/7.22341)
std_b2 := b2*(3.008222/7.22341)
std_c1 := c1*(5.044198/7.22341)
resvar == 1 - (std_a1^2 + std_a2^2 + 2*a1*10.67699*a2)

# Calculate indirect effects from income through investment,
# from income through investment and stress
# From income through stress, and for income total (all indirect and direct)
inc_inv := a3*b2
inc_inv_st := a3*a2*b1
inc_str := a1*b1
inc_tot := c1+inc_inv+inc_inv_st+inc_str

# standardised indirect effects
std_inc_inv    := std_a3*std_b2
std_inc_inv_st := std_a3*std_a2*std_b1
std_inc_str    := std_a1*std_b1
std_inc_tot    := std_c1+std_inc_inv+std_inc_inv_st+std_inc_str

# Absolute difference between total indirect income effects and stress effects
# -1* is necessary here because b1 is negative and std_inc_tot is positive
std_diff_incind_st := std_inc_tot - -1*std_b1

"

sem_out <- sem(sem_mod, data = dat1)

summary(sem_out, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
parameterestimates(sem_out)


dat1 <- dat1 %>% select(-std_income, -std_investment)

# Add binary and continuous outcome for children
inv_log <- function (x) 
{
  p <- 1/(1 + exp(-x))
  p <- ifelse(x == Inf, 1, p)
  p
}

p_gcses <- numeric(0)

set.seed(81)
for (i in 1:nrow(dat1)) {
  p_gcses[i] <- inv_log(log(1.8) + 0.05*scale(dat1$income)[i] + 0.4*scale(dat1$scws)[i] + 0.3*scale(dat1$investment)[i]
                     + 0.1*scale(dat1$stress_q3)[i] + rnorm(1, 0, 0.3)
                   )
}

hist(p_gcses)

# Add grouping variable to test for invariance (include one example where invariance broken and not broken)
set.seed(81)
gcse_min <- numeric()
for (i in 1:nrow(dat1)) {
 gcse_min[i] <- rbinom(1, 1, p_gcses[i])
}

dat1$gcse_min <- gcse_min

summary(glm(data = dat1, formula = gcse_min ~ scale(income) + scale(investment) + scale(scws) + scale(stress_q3), family = binomial))

set.seed(81)
dat1$parent_gender <- sample(x = c("Woman", "Man", "Other"), size = 5e3, replace = TRUE, prob = c(0.6, 0.35, 0.05))

write_csv(dat1, "data/example-data-sem-1.csv")


# country, added dataset, where stress has different factor loadings
# 1 and 3 more closely correlated
# 2 and 5 much weaker across the board


set.seed(1631)
mvsim_dat2 <- faux::rnorm_multi(
  n = n,
  mu = c(
    stress_lv = 0,
    stress_lv2 = 0, # hidden but related factor to avoid perfect fit!
    income     = 0,
    investment = 0
  ),
  r = c(  1,     0.65,  -0.6,  -0.3,
          0.65,     1,  -0.4,  -0.3,
          -0.6, -0.4,     1,  0.75,
          -0.3, -0.3, 0.75,     1 
  ),
  varnames = c("stress_lv", "stress_lv2", "income", "investment")
)

# simulate question responses (r truncated norm and then rounded)

stress_q1 <- numeric()
stress_q2 <- numeric()
stress_q3 <- numeric()
stress_q4 <- numeric()
stress_q5 <- numeric()

set.seed(80)
for (i in 1:n) {
  
  stress_q1[i] <- MASS::mvrnorm(n = 1, mu = 0 + 0.8 * mvsim_dat2$stress_lv[i], Sigma = 1-0.8^2) 
  stress_q2[i] <- MASS::mvrnorm(n = 1, mu = 0 + 0.4 * mvsim_dat2$stress_lv2[i], Sigma = 1-0.4^2) 
  stress_q3[i] <- MASS::mvrnorm(n = 1, mu = 0 + 0.3 * mvsim_dat2$stress_lv[i], Sigma = 1-0.3^2) 
  stress_q4[i] <- MASS::mvrnorm(n = 1, mu = 0 + 0.6 * mvsim_dat2$stress_lv2[i], Sigma = 1-0.6^2) 
  stress_q5[i] <- MASS::mvrnorm(n = 1, mu = 0 + 0.8 * mvsim_dat2$stress_lv2[i], Sigma = 1-0.8^2) 
  
}


sim_dat2 <- tibble(
  stress_q1,
  stress_q2,
  stress_q3,
  stress_q4,
  stress_q5,
  income = mvsim_dat2$income,
  investment = mvsim_dat2$investment
)

# Check means = 0 and std = 1

sim_dat2 %>%
  summarise_all(list(mean=mean, sd=sd)) %>%
  pivot_longer(everything()) %>%
  print(n = 40)


# Turn standardised scores back into 10 point scale, truncated and rounded
sim_dat2 <- sim_dat2 %>%
  mutate_at(
    vars(stress_q1:stress_q5),
    ~ round((scale(.) * 3) + 5, 0)
  ) %>%
  mutate_at(
    vars(stress_q1:stress_q5),
    ~ ifelse(. > 10, 10, ifelse(. < 0, 0, .))
  )


cfa_mod <- "s =~ stress_q1 + stress_q2 + stress_q3 + stress_q4 + stress_q5 
"

stress_cfa <- sem(cfa_mod, 
                  data = sim_dat2) 

summary(stress_cfa, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


# Generate further outcomes

stress_lv <- lavPredict(stress_cfa)[,1]

# Stirling Children's Wellbeing Scale: truncated discrete normal between 12 and 60, mean = 44
scws <- numeric()
for (i in 1:n) {
  scws[i] <- round(rtnorm(n = 1, mean = 44 + (-1 * scale(stress_lv)[i]) + (3*scale(sim_dat2$income)[i]) + (3*scale(sim_dat2$investment)[i]), sd = 5, upper = 60, lower = 12), 0)
}

dat2 <- tibble(
  std_income = sim_dat2$income,
  std_investment = sim_dat2$investment,
  stress_q1 = sim_dat2$stress_q1[,1],
  stress_q2 = sim_dat2$stress_q2[,1],
  stress_q3 = sim_dat2$stress_q3[,1],
  stress_q4 = sim_dat2$stress_q4[,1],
  stress_q5 = sim_dat2$stress_q5[,1],
  scws = scws
)

# un-standardise income and investment
dat2 <- dat2 %>%
  mutate(
    income = (std_income * 6) + 10,
    investment = (std_investment * 4) + 5
  )


dat2$country <- "Country B"

dat1_b <- dat1 %>% select(-gcse_min, -parent_gender)

dat1_b$country <- "Country A"

# Merge country A and B for equivalence testing
dat3 <- bind_rows(dat1_b, dat2)


# Save data for worked example
write_csv(dat3, "data/example-data-sem-3.csv")

# Codebook
varnames_ex1 <- names(dat1)
varlabels_ex1 <- c("How often have you gotten into a disagreement with a co-parent or family member about the parenting of your child(ren)? (10 Almost daily, 0 Never)",
                   "To what extent do you agree with the statement: 'my co-parents or family members often undermine the parenting of my child/children'? (10 Entirely, 0 Not at all)",
                   "To what extent do you agree with the statement: 'I have had to think seriously about my own and my child(ren)'s safety because of some of my relationships'? (10 Entirely, 0 Not at all)",
                   "To what extent do you agree with the statement: 'I have the skills to be a great parent, but I often feel like there are too many things that get in the way of me using them' (10 Entirely, 0 Not at all)",
                   "To what extent do you agree with the statement: 'I often worry about how much I need to rely on others when it comes to raising my children' (10 Entirely, 0 Not at all)",
                   "Stirling Child Wellbeing Scale Score (Max = 60, Min = 12, higher = better wellbeing)",
                   "Household income (equivalised, in £1000s per annum)",
                   "Income invested in children and children's/family activities and expenses (equivalised, in £1000s per annum)",
                   "Whether child achieved five or more GCSEs at grade C or above at age 16",
                   "Gender of the parent answering questions"
                   )

codebook_ex1 <- bind_cols(varnames_ex1, varlabels_ex1)
codebook_ex1 <- codebook_ex1 %>% rename("Variable name" = 1, "Variable label" = 2)

write_csv(codebook_ex1, "data/example-data-sem-1-codebook.csv")

# Codebook for data example 2

dat3 <- dat3 %>% select(-std_income, -std_investment)
varnames_ex2 <- names(dat3)
varlabels_ex2 <- c("How often have you gotten into a disagreement with a co-parent or family member about the parenting of your child(ren)? (10 Almost daily, 0 Never)",
                   "To what extent do you agree with the statement: 'my co-parents or family members often undermine the parenting of my child/children'? (10 Entirely, 0 Not at all)",
                   "To what extent do you agree with the statement: 'I have had to think seriously about my own and my child(ren)'s safety because of some of my relationships'? (10 Entirely, 0 Not at all)",
                   "To what extent do you agree with the statement: 'I have the skills to be a great parent, but I often feel like there are too many things that get in the way of me using them' (10 Entirely, 0 Not at all)",
                   "To what extent do you agree with the statement: 'I often worry about how much I need to rely on others when it comes to raising my children' (10 Entirely, 0 Not at all)",
                   "Stirling Child Wellbeing Scale Score (Max = 60, Min = 12, higher = better wellbeing)",
                   "Household income (equivalised, in £1000s per annum)",
                   "Income invested in children and children's/family activities and expenses (equivalised, in £1000s per annum)",
                   "Country"
)

codebook_ex2 <- bind_cols(varnames_ex2, varlabels_ex2)
codebook_ex2 <- codebook_ex2 %>% rename("Variable name" = 1, "Variable label" = 2)

write_csv(codebook_ex2, "data/example-data-sem-2-codebook.csv")


# Test configural etc models

mg_inv_test <- "

  stress =~ c(l1, l1)*stress_q1 + c(l2, l2)*stress_q2 + c(l3, l3)*stress_q3 + c(l4, l4)*stress_q4 + c(l5, l5)*stress_q5

  stress ~~ c(r1, r1)*stress

"

mg_inv_test_out <- lavaan::sem(model = mg_inv_test, data = dat3, group = "country")
summary(mg_inv_test_out, standardized = TRUE, fit.measures = TRUE)


# Example data three: 
 # Convergent, concurrent and discriminant validity (example data 3)

# PSS 1: Having child(ren) has meant having too few choices and too little control over my life
# PSS 2: I sometimes worry whether I am doing enough for my child(ren).
# PSS 3: Having child(ren) has been a financial burden. 
# PSS 4: It is difficult to balance different responsibilities because of my child(ren). 

# PANX 1: I am worried or anxious when I am with my child
# PANX 2: My child can sense that I am tense or nervous.
# PANX 3: I am on my guard and attentive to danger when me and my child are out together. 
# PANX 4: I tell my child that people are not to be trusted. 
# PANX 5: Because of my anxiety, I cancel things me and my child had planned on doing.

# PREQ 1: I have someone who I can talk to about child-raising.
# PREQ 2: I'm not worried about raising my child without anyone's opinion.
# PREQ 3: I can do anything that my child needs
# PREQ 4: My child makes me feel energized.
# PREQ 5: There are people who would help my child in the future.

set.seed(8)
mvsim_dat <- faux::rnorm_multi(
  n = n,
  mu = c(
    stress_lv = 0,
    pss_lv = 0,
    panx_lv = 0,
    preq_lv = 0
  ),
  r = c(  1,      0.85,  0.6, -0.45,
          0.85,      1, 0.55, -0.55,
          0.6 ,   0.55,    1, -0.2,
          -0.45, -0.55, -0.2,    1
          ),
  varnames = c("stress_lv", "pss_lv", "panx_lv", "preq_lv")
)

# simulate question responses (r truncated norm and then rounded)

stress_q1 <- numeric()
stress_q2 <- numeric()
stress_q3 <- numeric()
stress_q4 <- numeric()
stress_q5 <- numeric()

pss_q1 <- numeric()
pss_q2 <- numeric()
pss_q3 <- numeric()
pss_q4 <- numeric()

panx_q1 <- numeric()
panx_q2 <- numeric()
panx_q3 <- numeric()
panx_q4 <- numeric()
panx_q5 <- numeric()

preq_q1 <- numeric()
preq_q2 <- numeric()
preq_q3 <- numeric()
preq_q4 <- numeric()
preq_q5 <- numeric()

# simulate standardised scores

for (i in 1:n) {
  
  stress_q1[i] <- MASS::mvrnorm(n = 1, mu = 0 + 0.95 * mvsim_dat$stress_lv[i], Sigma = 1-0.95^2)
  stress_q2[i] <- MASS::mvrnorm(n = 1, mu = 0 + 0.9 * mvsim_dat$stress_lv[i], Sigma = 1-0.9^2)
  stress_q3[i] <- MASS::mvrnorm(n = 1, mu = 0 + 0.7 * mvsim_dat$stress_lv[i], Sigma = 1-0.7^2)
  stress_q4[i] <- MASS::mvrnorm(n = 1, mu = 0 + 0.85 * mvsim_dat$stress_lv[i], Sigma = 1-0.85^2)
  stress_q5[i] <- MASS::mvrnorm(n = 1, mu = 0 + 0.8 * mvsim_dat$stress_lv[i], Sigma = 1-0.8^2)
  
  pss_q1[i] <- MASS::mvrnorm(n = 1, mu = 0 + 0.8 * mvsim_dat$pss_lv[i], Sigma = 1-0.8^2)
  pss_q2[i] <- MASS::mvrnorm(n = 1, mu = 0 + 0.8 * mvsim_dat$pss_lv[i], Sigma = 1-0.8^2)
  pss_q3[i] <- MASS::mvrnorm(n = 1, mu = 0 + 0.6 * mvsim_dat$pss_lv[i], Sigma = 1-0.6^2)
  pss_q4[i] <- MASS::mvrnorm(n = 1, mu = 0 + 0.7 * mvsim_dat$pss_lv[i], Sigma = 1-0.7^2)
  
  panx_q1[i] <- MASS::mvrnorm(n = 1, mu = 0 + 0.7 * mvsim_dat$panx_lv[i], Sigma = 1-0.7^2)
  panx_q2[i] <- MASS::mvrnorm(n = 1, mu = 0 + 0.7 * mvsim_dat$panx_lv[i], Sigma = 1-0.7^2)
  panx_q3[i] <- MASS::mvrnorm(n = 1, mu = 0 + 0.8 * mvsim_dat$panx_lv[i], Sigma = 1-0.8^2)
  panx_q4[i] <- MASS::mvrnorm(n = 1, mu = 0 + 0.55 * mvsim_dat$panx_lv[i], Sigma = 1-0.55^2)
  panx_q5[i] <- MASS::mvrnorm(n = 1, mu = 0 + 0.65 * mvsim_dat$panx_lv[i], Sigma = 1-0.65^2)
  
  preq_q1[i] <- MASS::mvrnorm(n = 1, mu = 0 + 0.8 * mvsim_dat$preq_lv[i], Sigma = 1-0.8^2)
  preq_q2[i] <- MASS::mvrnorm(n = 1, mu = 0 + 0.8 * mvsim_dat$preq_lv[i], Sigma = 1-0.8^2)
  preq_q3[i] <- MASS::mvrnorm(n = 1, mu = 0 + 0.7 * mvsim_dat$preq_lv[i], Sigma = 1-0.7^2)
  preq_q4[i] <- MASS::mvrnorm(n = 1, mu = 0 + 0.75 * mvsim_dat$preq_lv[i], Sigma = 1-0.75^2)
  preq_q5[i] <- MASS::mvrnorm(n = 1, mu = 0 + 0.7 * mvsim_dat$preq_lv[i], Sigma = 1-0.7^2)
  
}

mean(panx_q1)
sd(panx_q1)

mean(panx_q2)
sd(panx_q2)

mean(preq_q3)
sd(preq_q3)

sim_resp <- tibble(
  stress_q1,
  stress_q2,
  stress_q3,
  stress_q4,
  stress_q5,
  pss_q1,
  pss_q2,
  pss_q3,
  pss_q4,
  panx_q1,
  panx_q2,
  panx_q3,
  panx_q4,
  panx_q5,
  preq_q1,
  preq_q2,
  preq_q3,
  preq_q4,
  preq_q5
)

# Check means = 0 and std = 1

sim_resp %>%
  summarise_all(list(mean=mean, sd=sd)) %>%
  pivot_longer(everything()) %>%
  print(n = 40)


# Turn standardised scores back into 10 point scale, truncated and rounded
sim_resp <- sim_resp %>%
  mutate_all(
    ~ round((. * 3) + 5, 0)
  ) %>%
  mutate_all(
    ~ ifelse(. > 10, 10, ifelse(. < 0, 0, .))
  )

GGally::ggcorr(sim_resp)
#GGally::ggpairs(sim_resp)


# Test CFA and correlations

cfa_mod <- "
  s =~ stress_q1 + stress_q2 + stress_q3 + stress_q4 + stress_q5
  pss =~ pss_q1 + pss_q2 + pss_q3 + pss_q4
  panx =~ panx_q1 + panx_q2 + panx_q3 + panx_q4 + panx_q5
  preq =~ preq_q1 + preq_q2 + preq_q3 + preq_q4 + preq_q5
"

stress_cfa <- sem(cfa_mod, 
                  data = sim_resp) 


summary(stress_cfa, standardized = TRUE, fit.measures = TRUE)


# Validity and reliability testing dataset example
# 1: Reliability on test and validation (ex data 1)
# 2: Scale reliability in multigroup invariance testing (example data 1 and 2)
# 3: Convergent, concurrent, and discriminant validity (example data 3)
# LV standardisation, saving scores, etc. 


# Codebook for example data 3
validity_varnames <- names(sim_resp)
validity_varlabels <- c(
  "How often have you gotten into a disagreement with a co-parent or family member about the parenting of your child(ren)? (10 Almost daily, 0 Never)",
  "To what extent do you agree with the statement: 'my co-parents or family members often undermine the parenting of my child/children'? (10 Entirely, 0 Not at all)",
  "To what extent do you agree with the statement: 'I have had to think seriously about my own and my child(ren)'s safety because of some of my relationships'? (10 Entirely, 0 Not at all)",
  "To what extent do you agree with the statement: 'I have the skills to be a great parent, but I often feel like there are too many things that get in the way of me using them' (10 Entirely, 0 Not at all)",
  "To what extent do you agree with the statement: 'I often worry about how much I need to rely on others when it comes to raising my children' (10 Entirely, 0 Not at all)",
  "Having child(ren) has meant having too few choices and too little control over my life",
  "I sometimes worry whether I am doing enough for my child(ren).",
  "Having child(ren) has been a financial burden.",
  "It is difficult to balance different responsibilities because of my child(ren).",
  "I am worried or anxious when I am with my child",
  "My child can sense that I am tense or nervous.",
  "I am on my guard and attentive to danger when me and my child are out together.",
  "I tell my child that people are not to be trusted.",
  "Because of my anxiety, I cancel things me and my child had planned on doing.",
  "I have someone who I can talk to about child-raising.",
  "I'm not worried about raising my child without anyone's opinion.",
  "I can do anything that my child needs",
  "My child makes me feel energized.",
  "There are people who would help my child in the future."
)

example_3_codebook <- tibble(validity_varnames, validity_varlabels)

sim_resp
write_csv(sim_resp, "data/example-data-sem-2.csv")
write_csv(example_3_codebook, "data/example-data-sem-2-codebook.csv")


# Code book:

c(    1,  0.8,  0.7,  0.6,  0.5,
    0.8,    1,  0.6,  0.9,  0.7,
    0.7,  0.6,    1,  0.6,  0.8,
    0.6,  0.9,  0.6,    1,  0.8,
    0.5,  0.7,  0.8,  0.8,    1)

# stress_q1: Approximately how many times over the last year have you gotten into a disagreement with a co-parent or family member about the parenting of your child(ren)? (10 point scale, centered at 5)
# stress_q2: To what extent do you agree with the statement: "my co-parents or family members often undermine the parenting of my child/children"? (10 point scale, centered at 5)
# stress_q3: To what extent do you agree with the statement: "I have had to think seriously about my own and my children's safety because of some of my relationships"? (10 point scale, centered at 5)
# stress_q4: To what extent do you agree with the statement: "I have the skills to be a great parent, but I often feel like there are too many things that get in the way of using them" (10 point scale, centered at 5)
# stress_q5: To what extent do you agree with the statement: "I often worry about how much I need to rely on others when it comes to raising my children" (10 point scale, centered at 5)



# Latent growth curve model example: margarine consumption etc

t = 1:10
mu_t_margarine <- matrix(nrow = 50, ncol = 10)
mu_t_divorce <- matrix(nrow = 50, ncol = 10)

for (i in 1:50) {
  
  mus <- rnorm_multi(
    n = 1,
    mu = c(3.6, 5.5),
    sd = c(0.5, 0.75),
    r = c(1, 0.4,
          0.4, 1),
    varnames = c("margarine_mu", "divorce_mu")
  )
  
  slopes <- rnorm_multi(
    n = 1,
    mu = c(0.2, 0.2),
    sd = c(0.03, 0.06),
    r = c(1, 0.6,
          0.6, 1),
    varnames = c("margarine_slope", "divorce_slope")
  )
  
  mu_t_margarine[i, ] <- mus$margarine_mu - slopes$margarine_slope*t
  mu_t_divorce[i, ] <- mus$divorce_mu - slopes$divorce_slope*t
  
}


# correlated noise
noise_margarine <- matrix(nrow = 50, ncol = 10)
noise_divorce <- matrix(nrow = 50, ncol = 10)

for (i in 1:10) {
  noise <- rnorm_multi(
    n = 50,
    mu = c(0, 0),
    sd = c(0.3, 0.3),
    r = c(1, -0.1,
          -0.1, 1),
    varnames = c("margarine_e", "divorce_e")
  )
  
  noise_margarine[,i] <- noise$margarine_e
  noise_divorce[,i] <- noise$divorce_e
  
}



margarine <- as_tibble(ifelse(mu_t_margarine + noise_margarine < 0, 0, mu_t_margarine + noise_margarine))
names(margarine) <- paste0("margarine_", seq(1, 10, 1))

divorce <- as_tibble(ifelse(mu_t_divorce + noise_divorce < 0, 0, mu_t_divorce + noise_divorce))
names(divorce) <- paste0("divorce_", seq(1, 10, 1))

sim_margdiv <- bind_cols(margarine, divorce)



# correlated residual noise within time points

div_marg_growthmod <- "

i_div =~ 1*divorce_1 + 1*divorce_2 + 1*divorce_3 + 1*divorce_4 + 1*divorce_5 + 1*divorce_6 + 1*divorce_7 + 1*divorce_8 + 1*divorce_9 + 1*divorce_10
s_div =~ 0*divorce_1 + 1*divorce_2 + 2*divorce_3 + 3*divorce_4 + 4*divorce_5 + 5*divorce_6 + 6*divorce_7 + 7*divorce_8 + 8*divorce_9 + 9*divorce_10

i_marg =~ 1*margarine_1 + 1*margarine_2 + 1*margarine_3 + 1*margarine_4 + 1*margarine_5 + 1*margarine_6 + 1*margarine_7 + 1*margarine_8 + 1*margarine_9 + 1*margarine_10
s_marg =~ 0*margarine_1 + 1*margarine_2 + 2*margarine_3 + 3*margarine_4 + 4*margarine_5 + 5*margarine_6 + 6*margarine_7 + 7*margarine_8 + 8*margarine_9 + 9*margarine_10

i_div ~~ s_div + i_marg + s_marg
s_div ~~ i_marg + s_marg
i_marg ~~ s_marg


"

lavaan::growth(div_marg_growthmod, data = sim_margdiv) %>% summary(standardized = TRUE)



div_marg_growthmod <- "

i_div =~ 1*divorce_1 + 1*divorce_2 + 1*divorce_3 + 1*divorce_4 + 1*divorce_5 + 1*divorce_6 + 1*divorce_7 + 1*divorce_8 + 1*divorce_9 + 1*divorce_10
s_div =~ 0*divorce_1 + 1*divorce_2 + 2*divorce_3 + 3*divorce_4 + 4*divorce_5 + 5*divorce_6 + 6*divorce_7 + 7*divorce_8 + 8*divorce_9 + 9*divorce_10

i_marg =~ 1*margarine_1 + 1*margarine_2 + 1*margarine_3 + 1*margarine_4 + 1*margarine_5 + 1*margarine_6 + 1*margarine_7 + 1*margarine_8 + 1*margarine_9 + 1*margarine_10
s_marg =~ 0*margarine_1 + 1*margarine_2 + 2*margarine_3 + 3*margarine_4 + 4*margarine_5 + 5*margarine_6 + 6*margarine_7 + 7*margarine_8 + 8*margarine_9 + 9*margarine_10

i_div ~~ s_div + i_marg + s_marg
s_div ~~ i_marg + s_marg
i_marg ~~ s_marg

# Correlated within-time point residuals (fixed to be equal)
divorce_1 ~~ resid_cor*margarine_1
divorce_2 ~~ resid_cor*margarine_2
divorce_3 ~~ resid_cor*margarine_3
divorce_4 ~~ resid_cor*margarine_4
divorce_5 ~~ resid_cor*margarine_5
divorce_6 ~~ resid_cor*margarine_6
divorce_7 ~~ resid_cor*margarine_7
divorce_8 ~~ resid_cor*margarine_8
divorce_9 ~~ resid_cor*margarine_9
divorce_10 ~~ resid_cor*margarine_10


"

lavaan::growth(div_marg_growthmod, data = sim_margdiv) %>% summary(standardized = TRUE)

# write_csv(sim_margdiv, "data/margarines_divorce.csv")


margdiv_long <- sim_margdiv %>%
  pivot_longer(cols = everything()) %>%
  mutate(t = as.numeric(str_split(name, "_", simplify = TRUE)[,2]),
         name = str_split(name, "_", simplify = TRUE)[,1])

margdiv_long %>%
  ggplot() + 
  geom_point(aes(x = t, y = value, col = name))


