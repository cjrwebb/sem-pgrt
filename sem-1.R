
# An Introduction to SEM: Part 1, Structural Equation Models ------------
# Dr. Calum Webb, Sheffield Methods Institute, 
# The University of Sheffield

# Introduction to libraries
library(tidyverse)
library(dagitty)
library(ggdag)
library(lavaan)
library(lavaanPlot)
library(semTools)
library(sjPlot)
library(broom)

# Introduction to the data
d1 <- read_csv("data/example-data-sem-1.csv")
d1_codes <- read_csv("data/example-data-sem-1-codebook.csv")

# Preview the data
d1
d1_codes


# Part 1: Starting with a simple regression model: recap --------------------------

#' In this short course, we will be using an example dataset and research
#' question reflective of real world social science. In our quantitative
#' study together, we will be exploring whether the family stress model
#' of children's outcomes or the family investment model of children's 
#' outcomes is the best suited for policymaking to improve children's wellbeing
#' and GCSE results.
#' 
#' We will then be using this example to develop a latent variable approach
#' to measuring and modelling parental stress in the second part (sem-2.R)
#' and then we will explore approaches to assess the reliability and validity
#' of latent constructs. 


#' Predicting child wellbeing via parental stress (stress_q1), income,
#' and investment of income in children

mod1 <- lm(data = d1, formula = scws ~ stress_q1 + income + investment)
sjPlot::tab_model(mod1, show.std = TRUE)

#' What would we conclude is the most effective policy for improving 
#' child wellbeing from this regression?

# Developing a structural equation model (and plotting with ggdag)

# Our current model can be read as assuming a relationship like this:

dag1 <- dagify(
  scws ~ stress_q1 + income + investment,
  exposure = c('stress_q1', 'income', 'investment'),
  outcome = c("scws")
)

set.seed(10)
tidy_dagitty(dag1) %>% 
  ggdag_classic(dag1, layout = "circle", size = 4) +
  theme_void()


#' We are assuming that all of our independent variables are truly
#' independent or, at best, that they are only confounders (e.g.
#' income confounds the relationship between investment and 
#' child wellbeing). If we fully mapped out our theoretical
#' relationship, we might expect something more like:

dag2 <- dagify(
  scws ~ stress_q1 + income + investment,
  stress_q1 ~ income,
  investment ~ income,
  exposure = c('stress_q1', 'income', 'investment'),
  outcome = c("scws")
)

set.seed(20)
tidy_dagitty(dag2) %>% 
  ggdag_classic(dag2, layout = "circle", size = 4) +
  theme_void()

#' In this case, we can see that the effect of income on
#' child wellbeing is also mediated through its effect
#' on stress and investment.
#' 
#' When stress and investment are included in a model,
#' we are only describing the *direct effect* of income 
#' on child wellbeing. That is, the effect of income on
#' child wellbeing *after excluding any impact it has on*
#' *stress and investment*. 
#' 
#' But for a true description of the effect that income
#' has on child wellbeing, we should also take account of
#' the indirect paths between income and wellbeing via 
#' investment and stress.
#' 
#' Structural Equation Modelling is not the only way to
#' achieve this (we can also just estimate multiple models)
#' but can be a nice way to model comparisons all in a 
#' single framework.
#' 
#' To learn more about indirect and direct effects see:
#' Westreich, D., & Greenland, S. (2013). The table 2 fallacy: 
#' presenting and interpreting confounder and modifier coefficients. 
#' American journal of epidemiology, 177(4), 292-298.
#' 
#' or
#' 
#' Pearl, J. and Mackenzie, D.; The Book of Why.




# Part 2: Creating a structural equation model ------------------------------------

# Defining the model structure in lavaan syntax
#' Now let's create a structural equation model that matches
#' the structure of the above directed acyclic graph

mod2 <- "

  scws ~ stress_q1 + income + investment
  stress_q1 ~ income
  investment ~ income

"

# Estimating the model
#' Now we can estimate the model in the lavaan structural
#' equation modelling package

mod2 <- lavaan::sem(mod2, data = d1)

# Basic model output:
#' Some basic model output can be generated using `summary`
summary(mod2)

# We can also generate a diagram of our model with coefficients added
# using the lavaanPlot package, which can be useful to make sure
# your lavaan syntax matches your diagram
lavaanPlot::lavaanPlot(model = mod2, coefs = TRUE)

# Expanding the model output:

# Standardised effect sizes
#' We can also request standardised scores by specifying
#' standardized = TRUE (note the American spelling) in the
#' summary argument. To bring up all summary arguments for 
#' lavaan (they can be hard to remember), enter ?`summary,lavaan-method`
#' in your console

summary(mod2, standardized = TRUE)
lavaanPlot::lavaanPlot(model = mod2, coefs = TRUE, stand = TRUE)

# Std.lv refers to coefficients where only latent variables 
# (we have none at the moment) have been standardised, Std.all
# refers to all variables being standardised (including dummy
# variables, so be careful interpreting these!)

# We can also add other summary statistics like 95% confidence
# intervals and R-squared statistics (for any outcome variables)
summary(mod2, standardized = TRUE, ci = 0.95, rsquare = TRUE)

# Note that the R-Square value is equal to 1 - the standardized
# residual variance (.scws)


# Part 3: Estimating Indirect Effects -------------------------------------

# Indirect effects: Manual calculation
# Now let's say we want to calculate our indirect effects manually
# let's take another look at our DAG/path diagram

set.seed(20)
tidy_dagitty(dag2) %>% 
  mutate(
    mean_x = (x + xend) / 2,
    mean_y = (y + yend) / 2,
    path_label = c("a1", "c", "a2", "b1", "", "b2")
  ) %>%
  ggdag_classic(layout = "circle", size = 4) + 
  geom_text(aes(x = mean_x, y = mean_y+0.08, label = path_label), colour = "grey20") +
  theme_void()

#' So let's imagine we wanted to break the effect of a £1000 per annum
#' increase in income on child wellbeing down by its impact on family 
#' stress, its impact on investment, and its direct impact. 
#' To do this, all we need to do is find the paths and multiply them:
#' 
#' The indirect effect of +£1000 income in SCWS via investment = a1 * b1
#' The indirect effect of +£1000 income on SCWS via stress = a2 * b2
#' The direct effect of +£1000 income in SCWS = c
#' 
#' The *total* effect of income would be a1*b1 + a2*b2 + c
#' 
#' We can find these in the summary output. For example, a1 would be
#' the regression of investment on income (investment ~ income)

summary(mod2)

a1 <- 0.373
b1 <- 0.740

a2 <- -0.264
b2 <- -0.985

c <- 0.206

indir_invest <- a1*b1
indir_stress <- a2*b2

indir_invest
indir_stress
c

# Total effect:
indir_invest + indir_stress + c
# A £1000 per annum increase in income would be expected
# to lead to an average increase of 0.742 points on the 
# Stirling-Warwick Child Wellbeing Scale.


# However, we would need to know the standardised effect in
# order to compare whether income or stress has a bigger 
# effect on child wellbeing.

#' YOUR TASK: Manually calculate the indirect effects and total
#' effects of income on child wellbeing using the *standardised*
#' *coefficients*. Is income or stress more strongly related to
#' child wellbeing after accounting for indirect pathways?

summary(mod2, standardized = TRUE)






# Part 4: Estimating indirect effects with error ----------------------------------

#' Manually calculating indirect effects is useful to get a sense for them,
#' but we need to specify our indirect effects within our lavaan syntax
#' if we want to easily calculate things like standard errors and 
#' test hypotheses. Let's revise our model to add *labels for paths*
#' and *defined quantities*.

# Defining indirect effects and total effects to be estimated with error

mod3 <- "
  
  # Labels, and constraints, are specified using * before
  # a variable
  scws ~ b2*stress_q1 + c*income + b1*investment
  stress_q1 ~ a2*income
  investment ~ a1*income

  # defined quantities are specified using :=
  indir_inv := a1*b1
  indir_str := a2*b2
  total_eff := c + indir_inv + indir_str

"

mod3 <- lavaan::sem(mod3, data = d1)
summary(mod3, standardized = TRUE, ci = 0.95)
lavaanPlot::lavaanPlot(model = mod3, coefs = TRUE)


# YOUR TASK: Do the defined parameters from the model output
# match the ones you calculated by hand? Are they statistically
# significant?




# Indirect effects can often have unusual distributions 
# (wide tailed or bimodal), so it is usually recommended to 
# bootstrap these coefficients (by resampling with replacement)
# across the sample and fitting many many models, which is
# more resource intensive. Models can be bootstrapped using
# the bootstrapLavaan() function.

# Bootstrapping a model with 1,000 draws - will take a while!
set.seed(101)
mod3_boot <- bootstrapLavaan(mod3, R = 1000, verbose = TRUE)
mod3_boot
summary(mod3_boot)

# We can then visualise the bootstrapped coefficients to judge 
# whether they are reliably in a single direction, e.g.

# for path a1
hist(mod3_boot[, "a1"])

# for path a1*b1
hist(mod3_boot[, "a1"]*mod3_boot[, "b1"])

# for path a2*b2
hist(mod3_boot[, "a2"]*mod3_boot[, "b2"])

# for total effect
hist(mod3_boot[, "a1"]*mod3_boot[, "b1"] + mod3_boot[, "a2"]*mod3_boot[, "b2"] + mod3_boot[, "c"] ) 

# We can also create summary statistics like 95% confidence 
# intervals for reporting
as_tibble(mod3_boot) %>%
  mutate(
    # generate the indirect and total effects
    indir_invest = a1*b1,
    indir_stress = a2*b2,
    total_eff = c + indir_invest + indir_stress
  ) %>%
  summarise_all(
    # create summary of bootstrapped coefficients
    list(
      median_est = ~median(.),
      std_err = ~sd(.),
      ci_95_low = ~quantile(., 0.025),
      ci_95_high = ~quantile(., 0.975)
    )
  ) %>%
  pivot_longer(everything()) %>%
  mutate(
    stat = str_extract(name, "median_est|std_err|ci_95_low|ci_95_high"),
    name = str_remove(name, "_median_est|_std_err|_ci_95_low|_ci_95_high")
  ) %>%
  pivot_wider(values_from = value, names_from = stat)


# YOUR TASK: To what extent do the bootstrapped confidence intervals differ
# from the ones calculated assuming a normal distribution in the summary?






# Part 5: Non-continuous outcomes -----------------------------------------

#' In structural equation modelling with lavaan, non-continuous predictors 
#' can be included as normal as a set of dummy variables. However,
#' non-continuous outcomes need to be specified and are modelled using 
#' a *probit* link, which is a little different to the usual *logistic*
#' link function. 

#' Let's expand our structural equation model to include a non-continuous
#' outcome, whether a child achieved their minimum 5 GCSEs at A* to C.

dag3 <- dagify(
  gcse_min ~ scws,
  scws ~ stress_q1 + income + investment,
  stress_q1 ~ income,
  investment ~ income,
  exposure = c('stress_q1', 'income', 'investment', "scws"),
  outcome = c("gcse_min")
)

set.seed(20)
tidy_dagitty(dag3) %>% 
  ggdag_classic(layout = "circle", size = 4) +
  theme_void()


# Expanding structural equation modelling for non-continuous outcomes

mod4 <- "
  
  # Labels, and constraints, are specified using * before
  # a variable
  gcse_min ~ d*scws
  scws ~ b2*stress_q1 + c*income + b1*investment
  stress_q1 ~ a2*income
  investment ~ a1*income

  # defined quantities are specified using :=
  indir_inv := a1*b1
  indir_str := a2*b2
  total_eff := c + indir_inv + indir_str

  # Add indirect effects of income and stress on gcse_min
  gcse_indir_stress := b2 * d
  gcse_indir_income := total_eff * d

  # To estimate an intercept for marginal effects,
  # we need to free the intercept by fixing the threshold
  # of the ordinal variable to 0 (so when the value
  # crosses 0 it indicates a greater likelihood of
  # achieving 5+ GCSEs)
  gcse_min | 0*t1  # fix threshold to 0
  gcse_min ~ NA*1  # free the intercept

"

# IMPORTANT: we specify our non-continuous outcomes using the
# "ordered" argument when estimating the model, with the name
# of the variable(s) included in the argument

#' When interpreting probit regression it is also a *very good*
#' idea to *centre* all of your predictors, as you will be 
#' using marginal effects to interpret them... The quickest
#' way to do this would be by mutating the data itself.
#' You might also want to standardised all predictors for
#' comparison, except for the outcome

mod4 <- lavaan::sem(mod4, 
                    data = d1 %>% mutate_at(vars(stress_q1:investment), ~scale(., center = TRUE, scale = TRUE)), 
                    ordered = c("gcse_min"))

summary(mod4, ci = 0.95)
lavaanPlot::lavaanPlot(model = mod4, coefs = TRUE)

# Communicating the effect of probit regression

# For a 1 standard deviation increase in wellbeing score, the
# probability of receiving 5 GCSEs at A* to C for a child
# increases from:

pnorm(0.348) # The probability when everything is average

pnorm(0.348 + 0.337) # The probability when wellbeing is 1SD higher

pnorm(0.348 + 0.337) - pnorm(0.348) 
# An ~11.7% increase in probability (at mean values)


# We can apply the same to calculate the change in percentage,
# rather than Z-scores, for a 1 standard deviation increase
# in stress (via wellbeing) and a 1 standard deviation increase
# in income (via investment, stress, and wellbeing directly)

pnorm(0.348) # The probability when everything is average
pnorm(0.348 + -0.102) # The probability when stress is 1SD higher

pnorm(0.348 + -0.102) - pnorm(0.348) 
# The change in probability

# YOUR TASK: Now compute the probability change for a 1SD increase
# in income via all indirect pathways: is it greater or less than 
# the effect of stress, and by how much?







# If you have a lot of coefficients and you need to calculate 
# marginal effects, this can be done in an efficient way *if*
# the predictors are all centered:

# If all of your predictors are not centered, the marginal
# effect may represent a very unlikely change!

broom::tidy(mod4) %>% 
  filter(op %in% c("~", ":=")) %>%
  filter(label %in% c("d", "gcse_indir_stress", "gcse_indir_income")) %>%
  mutate(
    margin = pnorm(0.348 + estimate) - pnorm(0.348),
    .after = estimate
  )


#' Homework task: Try adding in direct regressions of income, investment
#' and stress on GCSE results and analyse the new indirect paths and total
#' effects that emerge. How does this change your findings? What would
#' be a more efficient policy intervention, assuming changing income by
#' 1 standard deviation cost the same as changing stress by 1 standard
#' deviation, for improving GCSE outcomes for children?




