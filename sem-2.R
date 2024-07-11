
# An Introduction to SEM: Part 2, Latent Variables and Model Fit ------------
# Dr. Calum Webb, Sheffield Methods Institute, 
# The University of Sheffield

library(tidyverse)
library(dagitty)
library(ggdag)
library(lavaan)
library(lavaanPlot)
library(semTools)
library(sjPlot)
library(broom)
library(GGally)

# Read in the data
d1 <- read_csv("data/example-data-sem-1.csv")
d1_codes <- read_csv("data/example-data-sem-1-codebook.csv")

# Remind ourselves about the data
d1
d1_codes



# Part 1: How do we choose a variable to measure a construct? -------------

#' In the last exercise, we used the first question from the
#' stress questionnaire as our measure of stress. Here's the model
#' we ran, as a reminder. Take note of the strength of the effect
#' of stress on child wellbeing.

mod1a <- "

  scws ~ stress_q1 + income + investment
  stress_q1 ~ income
  investment ~ income

"

mod1a <- lavaan::sem(mod1a, data = d1)
summary(mod1a, standardized = TRUE, rsquare = TRUE)
lavaanPlot::lavaanPlot(model = mod1a, coefs = TRUE, stand = TRUE)

#' However, we have other questions measuring stress: the one we
#' have picked is somewhat arbitrary. What happens if we change
#' the model to use the second question in the stress questionnaire
#' (stress_q2) instead?

mod1b <- "

  scws ~ stress_q2 + income + investment
  stress_q2 ~ income
  investment ~ income

"

mod1b <- lavaan::sem(mod1b, data = d1)
summary(mod1b, standardized = TRUE, rsquare = TRUE)
lavaanPlot::lavaanPlot(model = mod1b, coefs = TRUE, stand = TRUE)

#' The relationship is a lot weaker, and the coefficient for
#' income increase somewhat.
#' 
#' We cannot put all of the stress variables into the model:
#' look what happens when we do.

mod1c <- "

  scws ~ stress_q1 + stress_q2 + stress_q3 + stress_q4 + stress_q5 + income + investment
  stress_q1 + stress_q2 + stress_q3 + stress_q4 + stress_q5 ~ income
  investment ~ income

"

mod1c <- lavaan::sem(mod1c, data = d1)
summary(mod1c, standardized = TRUE, rsquare = TRUE)
lavaanPlot::lavaanPlot(model = mod1c, coefs = TRUE, stand = TRUE)

#' It looks like the effect is being split somewhat arbitrarily across 
#' the stress questions, and they are likely to have high collinearity
#' (because they are measuring the same underlying factor: family stress)
#' 
#' For example, in this model it is saying that greater perceived risk for
#' children's safety was not associated children's wellbeing, which seems
#' unlikely!
#' 
#' Investment's impact also looks much bigger than before, but we know that
#' someone who scores highly one one stress score is likely to score
#' high on the other stress questions (they are not independnet) so it
#' is likely underestimating the impact.
#' 
#' We can confirm this by looking just at the regression of scws in a
#' typical model and checking the VIF using the car package.

mod1c_lm <- lm(data = d1, formula = scws ~ stress_q1 + stress_q2 + stress_q3 + stress_q4 + stress_q5)
summary(mod1c_lm)

car::vif(mod1c_lm)

#' Most recent literature suggests that a VIF greater than 2.5 is a 
#' cause for concern and a VIF greater than 5 is definitely a sign
#' of high colinearity. (Johnston R, Jones K, Manley D. Confounding and 
#' collinearity in regression analysis: a cautionary tale and an alternative 
#' procedure, illustrated by studies of British voting behaviour. Qual Quant. 
#' 2018;52(4):1957-1976. doi:10.1007/s11135-017-0584-6). 
#' 
#' Estimates are likely to be unstable but also hard to interpret: what 
#' does it mean that other forms of family stress have an impact but safety 
#' doesn't; surely these things are related but not necessarily as mediators?



# Part 2: Creating a latent variable ----------------------------------------------

#' A latent variable is a variable that *represents an underlying construct*,
#' and is measured by a set of *indicator* or *manifest variables*.
#' 
#' In this sense, we can say that we have an *underlying construct* of 
#' *family stress* that we are trying to measure using the family stress
#' related questions of: 
#' 
#' * How often have you gotten into a disagreement with a co-parent or family 
#'   member about the parenting of your child(ren)? (10 Almost daily, 0 Never)
#' * To what extent do you agree with the statement: 'my co-parents or family 
#'   members often undermine the parenting of my child/children'? (10 Entirely, 
#'   0 Not at all)
#' * To what extent do you agree with the statement: 'I have had to think seriously 
#'   about my own and my child(ren)'s safety because of some of my relationships'? 
#'   (10 Entirely, 0 Not at all)
#' * To what extent do you agree with the statement: 'I have the skills to be a great 
#'   parent, but I often feel like there are too many things that get in the way of me 
#'   using them' (10 Entirely, 0 Not at all)
#' * To what extent do you agree with the statement: 'I often worry about how much I 
#'   need to rely on others when it comes to raising my children' (10 Entirely, 0 Not 
#'   at all)
#'   
#' Any of these questions *individually* may act as a *proxy* for family stress,
#' but the commonality across them would be the best way to average out the 
#' error that comes from focusing on any one specific indicator.
#' 
#' Why wouldn't we just add all of the questions together into a single score?



# Creating a latent variable in lavaan:

mod2 <- "
  
  # Latent variables are defined using =~
  stress =~ stress_q1 + stress_q2 + stress_q3 + stress_q4 + stress_q5

"

mod2 <- lavaan::sem(mod2, data = d1)
summary(mod2, standardized = TRUE, rsquare = TRUE)
# plot using standardised coefficients
lavaanPlot::lavaanPlot(model = mod2, coefs = TRUE, stand = TRUE)

#' Using this simple code, the latent variable has the following
#' features:
#' 
#' * It is on the same scale as the indicator variable (the one
#'   with a loading of 1.000).
#'   
#' * The std.all column refers to the prediction of a 1SD increase
#'   in the latent variable on all of the indicators *simultaneously*
#'   
#' This means that:
#' 
#'  * For a 1 unit increase in the stress latent variable, we would 
#'    expect the response to question 1 to be 1 point higher,
#'    the response for question 2 to be 0.866 points higher, the
#'    response to question 3 to be 0.764 higher, the response for
#'    question 4 to be 0.931 higher, and the response to question
#'    5 to be 0.993 points higher. These are called *factor loadings*
#'    
#'  * Because they are all on the same scale this means we can say
#'    that the stress latent variable is probably doing a good job of
#'    approximating all of the indicator variables, but the standardised
#'    factor loadings can confirm this. We can see that the latent factor
#'    was most strongly associated with responses to question 1 (disagreements
#'    with parenting) 0.906, followed by question 5 (worry about relying on
#'    others) 0.88, followed by question 4 (unable to use skills) 0.831,
#'    followed by question 2 (undermining parenting) 0.77, followed
#'    by question 3 (fear for safety) at 0.69.
#'    
#'  * We can also use the R-squared statistics to describe the extent of the
#'    variance in our indicator variables that a single latent factor is 
#'    able to explain.

#' To better understand the relationship between the latent variable and
#' the underlying indicators, we can save the latent variable scores and 
#' visualise them with the data

# Create a data frame of just the stress questions
d_stress <- d1 %>% select(stress_q1:stress_q5)

# Get the assigned factor score for each participant
stress_scores <- lavPredict(mod2)

# Add the stress scores to the stress questions data frame
d_stress <- bind_cols(d_stress, stress_scores)

# Visualise all of the relationships using GGally's ggpairs function
GGally::ggpairs(d_stress, 
                lower = list(continuous = wrap("points", alpha = 0.01))) + 
  theme_minimal()


#' We may want to specify our latent variable to be standardised, rather
#' than on the same scale as our indicator variables (i.e. mean of 0 and
#' a standard deviation of 1)
#'   
#'  Here is how we would specify the latent variable to be constructed 
#'  as standardised *manually*:
#'   


mod3 <- "
  
  # Latent variables are defined using =~
  # We need both a name for the first factor loading as well as 
  # to 'free' the factor loading (where it is usually fixed to one) 

  stress =~ NA*stress_q1 + stress_q2 + stress_q3 + stress_q4 + stress_q5

  # we then set the variance of the latent variable to 1 to make it standardised
  # this gets a fair bit more complicated when stress is regressed by something else!

  stress~~1*stress

"

mod3 <- lavaan::sem(mod3, data = d1)
summary(mod3, standardized = TRUE, rsquare = TRUE)


#' Alternatively, we can use std.lv = TRUE in the lavaan::sem 
#' function to always ensure latent variables are standardised

mod2 <- "
  
  # Latent variables are defined using =~
  stress =~ stress_q1 + stress_q2 + stress_q3 + stress_q4 + stress_q5

"

mod2 <- lavaan::sem(mod2, data = d1, std.lv = TRUE)
summary(mod2, standardized = TRUE, rsquare = TRUE)


# Note that now the values in the "Estimate" column will
# be equal to the values in the Std.lv column in both this
# and the previous model summary: a one standard deviation
# increase in the stress latent variable increases the predicted
# stress score for question 1 by 2.479 points.


#' Note also that if we want to make your links between our factor
#' and our indicators follow a probit distribution (which may fit 
#' much better and be more appropriate if we have ordinal indicators
#' like here), we can just add them to the ordered grouping.

mod2_ordinal <- "
  
  # Latent variables are defined using =~
  stress =~ stress_q1 + stress_q2 + stress_q3 + stress_q4 + stress_q5

"

mod2_ordinal <- lavaan::sem(mod2_ordinal, data = d1, std.lv = TRUE, 
                    ordered = c("stress_q1", "stress_q2", "stress_q3", 
                                "stress_q4", "stress_q5"))
summary(mod2_ordinal, standardized = TRUE, rsquare = TRUE)

#' Note that we get slightly better R-squared scores here because
#' our predictions can no longer be impossible to select values
#' (e.g. 2.5 on a 10 point scale). Note that the latent variable
#' is *continuous*, which can be very convenient for interpretation.


#' Interpreting probit models with multiple outcomes is somewhat more difficult
#' but the main coefficients can be interpreted as Z-scores, in other words,
#' as changes in standard deviation towards or away from higher or lower
#' values.
#' 
#' To get a sense of what is "going on" in the relationship, it can be helpful
#' to use visualisation of the cumulative probability of being in each 
#' category of response for a 1 standard deviation increase in the latent variable.
#' This is a lot more difficult but a lot more intuitive!


#' Extract the threshold values on the probit scale
thresholds <- as_tibble(parameterestimates(mod2_ordinal)) %>%
  filter(op == "|")

# Convert the thresholds into cumulative probability
thresholds <- thresholds %>%
  mutate(
    prob = pnorm(est)
  )

# Plot the distribution of responses
ggplot() + 
  xlim(min(thresholds$est), 3) + 
  geom_function(fun = pnorm) +
  geom_segment(data = thresholds %>% filter(lhs == "stress_q1"), 
               aes(x = est, xend = est, y = 0, yend = prob)) +
  geom_text(data = thresholds %>% filter(lhs == "stress_q1"), 
            aes(x = est + 0.1, y = 0.05, label = rhs)) +
  annotate("curve", x = 0, xend = 0+0.915, y = pnorm(0), yend = pnorm(0+0.915), 
           curvature = -0.5) +
  annotate("point", x = 0, y = pnorm(0), colour = "pink") +
  annotate("point", x = 0 + 0.915, y = pnorm(0 + 0.915), colour = "firebrick") +
  theme_minimal()

#' So we might say that "for a 1 standard deviation increase
#' in the latent stress variable, we'd predict someone who
#' answered 5 to the first stress questionnaire score about
#' disgreements about parenting to instead be predicted to 
#' answer 8.

#' We can extent this to produce a nice plot for the entire range of
#' questions on their ordinal scales

loadings <- as_tibble(parameterestimates(mod2_ordinal)) %>%
  filter(op == "=~")

loadings <- loadings %>%
  mutate(
    lhs = rhs
  )

ggplot() + 
  xlim(min(thresholds$est), 3) + 
  geom_function(fun = pnorm) +
  geom_text(data = thresholds, 
            aes(x = est + 0.15, y = 0.05, label = str_remove(rhs, "t") )) +
  geom_segment(data = thresholds, 
               aes(x = est, xend = est, y = 0, yend = prob)) +
  geom_curve(data = loadings, aes(x = 0, xend = 0+est, y = pnorm(0), yend = pnorm(0+est)), 
             curvature = -0.5) +
  geom_point(data = loadings, aes(x = 0, y = pnorm(0)), colour = "pink") +
  geom_point(data = loadings, aes(x = 0 + est, y = pnorm(0 + est)), colour = "firebrick") +
  facet_wrap(~lhs) +
  theme_minimal()

#' Here you can see how the impact on the third stress question is a lot
#' smaller.

#' We're going to stick with treating these variables as 'pseudo-continuous'
#' going forward, but you shouldn't be afraid to use probit links between
#' your factors and your manifest variables where appropriate! They are
#' still quite interpretable as changes in standard deviation.



# Part 3: Model fit and latent variables ------------------------------------------

#' Now that we have our latent variable, we need to assess *how good a fit*
#' it is to the data. While standardised factor loadings can be a good hint
#' (all should be more than 0.3), there are *fit indices* designed to answer
#' the question: is our model of one latent factor a good way of summarising
#' the patterns in the data?
#' 
#' Poor factor loadings = variables with a lot of error for measuring the
#' latent factor
#' 
#' Poor model fit = a bad fit to the correlations in the data overall, even
#' if the factor loadings look okay. It is likely that actually there is
#' more than one factor/a more complex process underlying the generation 
#' of these indicators.
#' 
#' Good model fit, poor factor loadings = a good fit to the data overall, 
#' but probably because there are not many strong relationships in the data
#' to begin with (you're basically modelling unrelated random noise)
#' 
#' Poor model fit, good factor loadings = some of your indicators are not
#' necessarily closely linked to each other, while others are, so the factor
#' is over-exaggerating the relationship between some indicators which is 
#' not an appropriate fit. It may be that the indicators may be relating to 
#' slightly different underlying factors. Your indicators share a lot of
#' commonality with a single factor, but not necessarily with one another in
#' isolation. 
#' 
#' Good model fit, good factor loadings = your model specification (e.g,
#' one underlying factor for all indicators) is appropriate for describing
#' the data (all of your 'outcome' variables, here, indicators) *and* all
#' of your indicator variables are related in a way that can be approximated
#' by a single factor.
#' 


# Model fit indices can be generated using the fit.measures = TRUE
# argument, which will add commonly reported fit measures to the header
# of the output, or using the fitmeasures() function
summary(mod2, fit.measures = TRUE, standardized = TRUE)
fitmeasures(mod2)


# Part 4: Cheat sheet for model fit -----------------------------------------------

#' https://davidakenny.net/cm/fit.htm
#' 
#' COMPARATIVE MODEL FIT
#' 
#' AIC / BIC / SABIC: These model fit indices are only useful when you are
#' *comparing two or more models with the same underlying data/variables*
#' *but with different model specifications in order to test which is the*
#' *better fit*. A lower AIC/BIC/SABIC indicates a *better* model fit. Some
#' cutoffs are available (e.g. a change of less than 2 in AIC indicates that
#' the models are equivalent) but generally you want to be looking for a big
#' or small change.
#' 
#' The difference between AIC and BIC is that BIC (Bayesian Information 
#' Criterion) applies a *greater penalty for model complexity*. This means
#' it will tend to favour more parsimonious models, and can therefore be
#' a good choice when you are trying to avoid "overfitting" data.
#' 
#' INCREMENTAL FIT INDICES
#' 
#' CFI & TLI are both incremental fit indices. Incremental fit indices try
#' to judge how close your model is to the *worst fitting model* or the 
#' *best fitting (but uninformative)* model for the data. Closer to the
#' worst fitting model will mean a TLI or CFI score close to 0, closer to 
#' the best fitting model would mean a TLI or CFI score close to 1.
#' 
#' Commonly used cutoffs for "good" model fit are a *CFI of over 0.95* and
#' a *TLI of over 0.9*, though some have suggested that a TLI of over 0.95
#' should be recommended. Still, others have warned against strict cutoffs
#' as the interpretation of these fit indices is fairly intuitive.
#' 
#' As with the distinction between AIC and BIC, the TLI imposes a penalty 
#' for model complexity so tends to favour less overfit models.
#' 
#' ABSOLUTE FIT INDICES
#' 
#' Absolute fit indices are fit indices that measure "badness" of fit,
#' the further away from 0 that they are, the worse the fit. Two commonly
#' reported absolute fit indices are the Root Mean Square Error of 
#' Approximation (RMSEA) and the Standardised Root Mean Square Residual
#' (SRMR). 
#' 
#' For *RMSEA*, MacCallum, Browne and Sugawara (1996) suggest *0.01, 0.05, and* 
#' *0.08 to indicate excellent, good, and mediocre fit*. However, others 
#' have suggested 0.10 as the cutoff for poor fitting models. (Kenny, 2020).
#' 
#' For *SRMR*, a value *less than 0.08* is usually considered a good fit.
#' 
#' 
#' It is usually recommended to report at least one absolute and one
#' incremental fit index when reporting results containing latent 
#' variables.
#' 


#' YOUR TASK: Using the model fit results, describe how well the 
#' stress latent variable fits the data.





# An example of poor model fit:
#' Just as an example, let's create a latent variable that also
#' includes income, investment, and scws (three very different)
#' latent concepts as indicators of 'stress' and see how our 
#' model fit changes:

mod4 <- "
  
  # Latent variables are defined using =~
  stress =~ stress_q1 + stress_q2 + stress_q3 + stress_q4 + stress_q5 + income + investment + scws

"

mod4 <- lavaan::sem(mod4, data = d1, std.lv = TRUE)
summary(mod4, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


#' How are our model fit and factor loadings now? Is a single
#' latent variable a good fit for all of these concepts together?



# Part 5: Adding our latent variable to our SEM -----------------------------------

#' Now we have a factor to represent stress, let's add this to our 
#' previous SEM predicting child wellbeing from the last exercises.

mod5 <- "

  # Start by creating our latent variable
  stress =~ stress_q1 + stress_q2 + stress_q3 + stress_q4 + stress_q5
  
  # Now, instead of one question, we will use the stress
  # latent factor to predict wellbeing
  scws ~ b2*stress + c*income + b1*investment
  stress ~ a2*income
  investment ~ a1*income

  # defined quantities are specified using :=
  indir_inv := a1*b1
  indir_str := a2*b2
  total_eff := c + indir_inv + indir_str

"

mod5 <- lavaan::sem(mod5, data = d1, std.lv = TRUE)
summary(mod5, fit.measures = TRUE, standardized = TRUE, ci = 0.95, rsquare = TRUE)
lavaanPlot::lavaanPlot(model = mod5, coefs = TRUE, stand = TRUE)


#' What kind of impact is a 1 standard deviation increase in stress
#' predicted to have on children's wellbeing?



#' Now we are capturing a 'general' measure of family stress, we might
#' also be interested in whether the residuals of our indicators (the
#' 'error' specific to them) have any relationship with out outcomes.
#' 
#' For example, to what extent does higher residual stress around 
#' children's safety have an effect on child wellbeing *beyond the*
#' *general effect of family stress*? This is not often done in 
#' practice but can be very interesting!
#' 

mod6 <- "

  # Start by creating our latent variable
  stress =~ stress_q1 + stress_q2 + stress_q3 + stress_q4 + stress_q5
  
  # Now, instead of one question, we will use the stress
  # latent factor to predict wellbeing
  scws ~ b2*stress + c*income + b1*investment + stress_q3
  stress ~ a2*income
  investment ~ a1*income

  # defined quantities are specified using :=
  indir_inv := a1*b1
  indir_str := a2*b2
  total_eff := c + indir_inv + indir_str

"

mod6 <- lavaan::sem(mod6, data = d1, std.lv = TRUE)
summary(mod6, fit.measures = TRUE, standardized = TRUE, ci = 0.95, rsquare = TRUE)
lavaanPlot::lavaanPlot(model = mod6, coefs = TRUE, stand = TRUE)



#' HOMEWORK TASK: Try extending the model to include GCSEs as an
#' outcome.


