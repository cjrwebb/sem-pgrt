
# An Introduction to SEM: Part 3, Reliability & Validity ------------
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
library(tidySEM)
library(patchwork)

# Read in the data
d1 <- read_csv("data/example-data-sem-1.csv")
d1_codes <- read_csv("data/example-data-sem-1-codebook.csv")

# Remind ourselves about the data
d1
d1_codes


# Part 1: How do we know if we have a good latent variable? -----------------------

#' We might want to know answers to the following questions about our
#' variables:
#' 
#' * How well do our questions measure the underlying construct within
#'   the sample (we just did this with model fit and factor loadings)
#'   (internal reliability)?
#'   
#' * How sensitive to our specific sample is the fit of the underlying
#'   factor? (external reliability)
#'   
#' * How sensitive is our underlying factor construction to differences
#'   in group? How cross-culturally applicable is it? (external reliability,
#'   multigroup invariance)
#'   
#' * Do our questions lead to similar responses to other questionnaires
#'   measuring the same underlying concepts? (Concurrent validity)
#'   
#' * Do our questions capture an underlying factor that is similar to 
#'   related factors, but is distinct from them? (Convergent validity)
#'   
#' * Is our underlying stress factor negatively or uncorrelated with 
#'   things that theoretically it should be uncorrelated or negatively
#'   correlated with? (Discriminant validity)
#'   

# This kind of research is common in psychometrics, but not in socio-
# metrics — especially where factors are constructed out of a mixture 
# of different indicators that haven't been pre-designed as a combined 
# metric!



# Part 2: Construction / Validation Splitting - Sample overfitting ----------------

#' A typical and very effective approach to avoiding overfitting latent factors
#' — protecting against finding a "good" result that is really just due to 
#' peculiarities in your specific sample — is to start by *splitting* your data
#' into a 'testing' or 'construction' subset and a 'validation' subset. 
#' 
#' You can then focus on building and refining your latent variable model on the
#' 'testing' subset, before 'validating' it on the validation/confirmation subset.
#' 
#' This is *especially* important when you are actively experimenting with the 
#' construction of the factor - e.g. comparing one versus two factor solutions,
#' comparing the inclusion or exclusion of some indicator variables, and so on.
#' This kind of experimenting leads to a 'garden of forking paths' (Gelman, 2013)
#' http://www.stat.columbia.edu/~gelman/research/unpublished/p_hacking.pdf 
#' where the final results end up more and more informed by the eccentricities
#' of the sample and less by the general patterns. If you are experimenting a 
#' lot and "following the significant" you might accidentally create something
#' that is over generalised to your sample. 
#' 
#' You can split your data into as many samples as you have the sample size for,
#' but it's generally a good idea to keep at least 100 cases per subset (and
#' ideally at least 400). You could have 1 testing sample and then validate the
#' results on 3 other subsets. It's the same logic as going out and conducting
#' 3 more surveys to validate our factor without the cost!


#' If we're assuming our data might ordered in a non-random way, here is how
#' we might split our sample into a testing and validation half:

set.seed(100)
d1 <- d1 %>%
 mutate(
   random_split = sample(c("testing", "validation"), nrow(d1), 
                         replace = TRUE, prob = c(0.5, 0.5))
 )

table(d1$random_split)

d1_test <- d1 %>% filter(random_split == "testing")
d1_validation <- d1 %>% filter(random_split == "validation")


#' Now we can test out models on both the testing and validation
#' half and see the extent to which they agree: we want to check
#' that the fit measures and the factor loadings are broadly similar
#' (i.e. standardised loadings within ±0.1 or ±0.2 on the standardised
#' scale, fit measures still good across both halves)

mod1 <- "
  
  # Latent variables are defined using =~
  stress =~ stress_q1 + stress_q2 + stress_q3 + stress_q4 + stress_q5

"

mod1_test <- lavaan::sem(mod1, data = d1_test)
mod1_validation <- lavaan::sem(mod1, data = d1_validation)

# We can compare the summaries directly
summary(mod1_test, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
summary(mod1_validation, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# And/or just compare the fit measures
fitmeasures(mod1_test, c("cfi", "tli", "rmsea", "srmr"))
fitmeasures(mod1_validation, c("cfi", "tli", "rmsea", "srmr"))

# We could also use tidySEM and patchwork to plot some diagrams side by side,
# but it is harder to customer the output compared to lavaanPlot at the moment.
tidySEM::graph_sem(model = mod1_test, fix_coord = TRUE) + 
tidySEM::graph_sem(model = mod1_validation, fix_coord = TRUE)


# Looking good!



# Part 4: Multigroup invariance ---------------------------------------------------

#' How do we know whether our latent variable can be used responsibly across
#' different populations? Imagine, for example, that the questionnaire about
#' family stress is used across two different countries, or if, for example,
#' we wanted to check whether assessments of family stress were similar across
#' parents genders?
#' 
#' This is called *multigroup invariance* testing, and we usually define *four*
#' levels of invariance, each more strict than the previous:
#' 
#' 1) *Configural invariance*: The underlying factor is measured by the same
#'    indicators across all groups. A lack of configural invariance means that 
#'    not all indicators are related to the same underlying factor for one
#'    of the groups, and thus that item is not a universal/comparable feature
#'    of the factor. For example, access to period products may be a indicator
#'    of the factor of poverty for people assigned female at birth but not for
#'    people assigned male at birth.
#' 2) *Metric invariance*: Each of the indicators is weighted approximately
#'    equally across all groups. A lack of metric invariance means that  
#'    different indicators are more important for the measurement of the
#'    underlying construct for one group than the other, even though all
#'    items may be relevant. For example, access to the internet may be an
#'    important item for measuring poverty among younger people but may be
#'    less relevant for older people.
#' 3) *Scalar invariance*: Scalar invariance refers to whether all of the
#'    different groups have similar scores on the indicator variables when
#'    the underlying factor is at an average score (fixed intercepts). 
#'    A lack of scalar invariance means that even though both groups may
#'    have a factor score of 0, they have different associated experiences
#'    of the manifest variables at this level. For example, 'average'
#'    levels of work related stress may imply greater perceptions of 
#'    stress indicators for manual labourers when compared to office 
#'    workers. So while the impact of a 1 unit increase in work stress may 
#'    still be comparable between groups, it would reflect going from
#'    'stressed' to 'very stressed' among manual labourers but from
#'    'not very stressed' to 'stressed for office workers'.
#' 4) *Strict/residual invariance*: Strict invariance refers to a latent
#'    factor meeting all of the above criteria but also that the unexplained
#'    variance in each of the indicators is also approximately equal across
#'    groups. A lack of strict invariance means that some groups may have a
#'    wider range of experiences or causal factors related to their item 
#'    scores than another group; they may have more extreme or diverse
#'    scores in general. For example, while children at two different 
#'    schools might have the same average scores, the same weighting, and the
#'    same indicators for a measure of English language aptitude, the 
#'    school with a more diverse cohort of students may have a larger 
#'    residual variance across the item scores than the school with a
#'    less diverse cohort.
#' 
#' What does it mean if your factors do not have invariance? For most studies,
#' metric invariance is sufficient (i.e. if you want to know the effect of a
#' 1sd increase in a factor on some other outcome), but for others it won't be,
#' for example, if you want to know whether there is a difference in the mean
#' factor score between groups you may want at least Scalar invariance (or to
#' enforce scalar invariance as long as your model fit is still reasonable).
#' Strict/residual variance is rarely required unless you explicitly want to
#' know whether the variance in something is associated with something else.
#' 
#' If you are working to develop socio or psychometrics, invariance testing can
#' help you decide what questions may need revising or should be excluded when
#' doing cross-cultural/cross-group research. 
#' 
#' If you are using factors as part of your study, invariance testing can tell
#' you *something interesting and substantive* about differences across groups
#' and how they experience or report things related to underlying concepts. 
#' 
#' It is common for SEM studies with latent variables to be done *without* 
#' testing for multigroup invariance. This is fine, and you can reflect on
#' the need for invariance testing as a weakness of any studies where factors
#' have been used that have not been checked for invariance.
#' 

#' For invariance testing, we want to build up and compare models that
#' get increasingly strict by 'fixing' parameters across our groups to be 
#' more and more restricted by using *the same factor labels*.

# check how many groups we have
table(d1$parent_gender) # 3

configural_model <- "
  
  # Allow the factor loadings to all be different across all three
  # groups by giving them different labels
  stress =~ c(l1a, l1b, l1c)*stress_q1 + c(l2a, l2b, l2c)*stress_q2 + 
            c(l3a, l3b, l3c)*stress_q3 + c(l4a, l4b, l4c)*stress_q4 + 
            c(l5a, l5b, l5c)*stress_q5

  # Allow all indicator intercepts to be different across all three
  # groups by giving them different labels
  stress_q1 ~ c(i1a, i1b, i1c)*1
  stress_q2 ~ c(i2a, i2b, i2c)*1
  stress_q3 ~ c(i3a, i3b, i3c)*1
  stress_q4 ~ c(i4a, i4b, i4c)*1
  stress_q5 ~ c(i5a, i5b, i5c)*1

  # Allow all residual variances to be different across all three groups
  # by giving them different labels
  stress_q1 ~~ c(r1a, r1b, r1c)*stress_q1
  stress_q2 ~~ c(r2a, r2b, r2c)*stress_q2
  stress_q3 ~~ c(r3a, r3b, r3c)*stress_q3
  stress_q4 ~~ c(r4a, r4b, r4c)*stress_q4
  stress_q5 ~~ c(r5a, r5b, r5c)*stress_q5

"

#' For *configural invariance*, we are just checking that the model fit
#' is good under the typical thresholds.
#' We specify our grouping variable using the "group" option.

configural_model <- lavaan::sem(configural_model, data = d1, std.lv = TRUE,
                                group = "parent_gender")
summary(configural_model, fit.measures = TRUE)

# Model fit looks good! This will also be our baseline for further
# kinds of invariance.


# Part 4.1 Specifying the metric invariance model ----------------------------------

metric_model <- "
  
  # Fix the factor loadings to be the same across all three
  # groups by giving them the same labels
  stress =~ c(l1, l1, l1)*stress_q1 + c(l2, l2, l2)*stress_q2 + 
            c(l3, l3, l3)*stress_q3 + c(l4, l4, l4)*stress_q4 + 
            c(l5, l5, l5)*stress_q5

  # Allow all indicator intercepts to be different across all three
  # groups by giving them different labels
  stress_q1 ~ c(i1a, i1b, i1c)*1
  stress_q2 ~ c(i2a, i2b, i2c)*1
  stress_q3 ~ c(i3a, i3b, i3c)*1
  stress_q4 ~ c(i4a, i4b, i4c)*1
  stress_q5 ~ c(i5a, i5b, i5c)*1

  # Allow all residual variances to be different across all three groups
  # by giving them different labels
  stress_q1 ~~ c(r1a, r1b, r1c)*stress_q1
  stress_q2 ~~ c(r2a, r2b, r2c)*stress_q2
  stress_q3 ~~ c(r3a, r3b, r3c)*stress_q3
  stress_q4 ~~ c(r4a, r4b, r4c)*stress_q4
  stress_q5 ~~ c(r5a, r5b, r5c)*stress_q5

"

#' For *metric invariance*, we are just checking that the model fit
#' is substantially worse than the configural model

metric_model <- lavaan::sem(metric_model, data = d1, std.lv = TRUE,
                            group = "parent_gender")
summary(metric_model, fit.measures = TRUE)

# side by side model fit
fitmeasures(configural_model, c("aic", "bic", "cfi", "tli", "rmsea", "srmr"))
fitmeasures(metric_model, c("aic", "bic", "cfi", "tli", "rmsea", "srmr"))

#' Our metric model looks just as good, and has even improved under the
#' fit measures that impose penalties. Only the SRMR seems a little worse.

#' We can also conduct a likelihood ratio test to see if the new model
#' is a *significantly* worse fit to the data

lavTestLRT(configural_model, metric_model)

# A non-significant chi-square p-value here means that the more restrictive
# metric model is not significantly worse than the configural model.



# Part 4.2: Specifying the scalar invariance model ----------------------------------

scalar_model <- "
  
  # Fix the factor loadings to be the same across all three
  # groups by giving them the same labels
  stress =~ c(l1, l1, l1)*stress_q1 + c(l2, l2, l2)*stress_q2 + 
            c(l3, l3, l3)*stress_q3 + c(l4, l4, l4)*stress_q4 + 
            c(l5, l5, l5)*stress_q5

  # Fix all indicator intercepts to be the same across all three
  # groups by giving them the same labels
  stress_q1 ~ c(i1, i1, i1)*1
  stress_q2 ~ c(i2, i2, i2)*1
  stress_q3 ~ c(i3, i3, i3)*1
  stress_q4 ~ c(i4, i4, i4)*1
  stress_q5 ~ c(i5, i5, i5)*1

  # Allow all residual variances to be different across all three groups
  # by giving them different labels
  stress_q1 ~~ c(r1a, r1b, r1c)*stress_q1
  stress_q2 ~~ c(r2a, r2b, r2c)*stress_q2
  stress_q3 ~~ c(r3a, r3b, r3c)*stress_q3
  stress_q4 ~~ c(r4a, r4b, r4c)*stress_q4
  stress_q5 ~~ c(r5a, r5b, r5c)*stress_q5

"

#' For *scalar invariance*, we are checking that the model fit
#' is substantially worse than the metric model

scalar_model <- lavaan::sem(scalar_model, data = d1, std.lv = TRUE,
                            group = "parent_gender")
summary(scalar_model, fit.measures = TRUE)

# side by side model fit
fitmeasures(metric_model, c("aic", "bic", "cfi", "tli", "rmsea", "srmr"))
fitmeasures(scalar_model, c("aic", "bic", "cfi", "tli", "rmsea", "srmr"))

#' Looks like we have improvements across the board and only a insubstantial
#' increase in SRMR

# Likelihood ratio test
lavTestLRT(scalar_model, metric_model)

# Again, our scalar model is not significantly worse than our metric model,
# so we can assume we have scalar invariance.


# Part 4.3: Specifying the strict invariance model ----------------------------------

strict_model <- "
  
  # Fix the factor loadings to be the same across all three
  # groups by giving them the same labels
  stress =~ c(l1, l1, l1)*stress_q1 + c(l2, l2, l2)*stress_q2 + 
            c(l3, l3, l3)*stress_q3 + c(l4, l4, l4)*stress_q4 + 
            c(l5, l5, l5)*stress_q5

  # Fix all indicator intercepts to be the same across all three
  # groups by giving them the same labels
  stress_q1 ~ c(i1, i1, i1)*1
  stress_q2 ~ c(i2, i2, i2)*1
  stress_q3 ~ c(i3, i3, i3)*1
  stress_q4 ~ c(i4, i4, i4)*1
  stress_q5 ~ c(i5, i5, i5)*1

  # Allow all residual variances to be different across all three groups
  # by giving them different labels
  stress_q1 ~~ c(r1, r1, r1)*stress_q1
  stress_q2 ~~ c(r2, r2, r2)*stress_q2
  stress_q3 ~~ c(r3, r3, r3)*stress_q3
  stress_q4 ~~ c(r4, r4, r4)*stress_q4
  stress_q5 ~~ c(r5, r5, r5)*stress_q5

"

#' For *strict invariance*, we are checking that the model fit
#' is substantially worse than the scalar model

strict_model <- lavaan::sem(strict_model, data = d1, std.lv = TRUE,
                            group = "parent_gender")
summary(strict_model, fit.measures = TRUE)

# side by side model fit
fitmeasures(scalar_model, c("aic", "bic", "cfi", "tli", "rmsea", "srmr"))
fitmeasures(strict_model, c("aic", "bic", "cfi", "tli", "rmsea", "srmr"))

#' Again, this looks pretty defensible

# Likelihood ratio test
lavTestLRT(scalar_model, strict_model)

# Our strict model is not significantly worse than our scalar model,
# so we can assume we have strict invariance across different parents'
# genders.

#' CONCLUSIONS: Based on our invariance testing we can be confident that
#' the stress questionnaire and latent factor can be applied across responses
#' from different parents' genders with results being directly comparable
#' and reliable.



#' HOMEWORK/INDEPENDENT STUDY TASK: In the data/ folder, you will find
#' data titled 'example-data-sem-3.csv' and accompanying codebook, which
#' includes responses for the same stress questionnaire but this time from
#' two different countries (country A and country B). Your task is to test
#' whether the family stress questionnaire is invariance (and at what level)
#' across the two different country contexts by modifying the code used above,
#' but instead performing multigroup invariance testing by country, rather
#' than by parent's gender.



# Part 5: Concurrent, Convergent, and Discriminant Validity -----------------------

#' The last thing we might want to test is the *validity* of our factor:
#' to what extent does it accurately reflect the latent concept that we are
#' trying to model.
#' 
#' From a statistical standpoint, there are three things we can do to 
#' assess whether our latent construct appears to be valid:
#' 
#' 1) If other people have designed questionnaires measuring the same
#'    concept, we can see whether the factor is highly correlated with
#'    these questionnaires. This is sometimes called *"Concurrent Validity"*.
#'    This is usually not very applicable to research but may be
#'    appropriate when we are, for example, trying to adapt a questionnaire
#'    to a different cultural context which might have involved changing 
#'    the wording somewhat. It may also be appropriate if we are trying to
#'    design a 'cut down' version of a much longer questionnaire, or a 
#'    questionnaire measuring only one single domain of a more complex 
#'    questionnaire that measures several factors.
#'    
#' 2) The factor should be highly correlated with factors of related 
#'    concepts (*convergent validity*), but should still be measured 
#'    precisely enough to be considered uniquely different to the 
#'    related concept (*divergent validity*). For example, family 
#'    stress should be related to parental anxiety, but the two factors
#'    should have enough detail to differentiate between them.
#'    
#' 3) The factor should be uncorrelated with, or negatively correlated 
#'    with, concepts that measure its complement. For example, parenting
#'    stress should be negatively correlated with parenting resilience.
#'    This is sometimes also called *divergent validity*. However, opposite
#'    measures may not necessarily be distinct if they are truly the 
#'    "other side of the coin".
#' 


# We have another set of data where there are variables measuring
# responses from the family stress questionnaire; the parental
# stress scale (Child Outcomes Research Consortium, 2019); the
# the parenting anxiety questionnaire (Elfström and Ahlen, 2022); 
# and the parenting resilience questionnaire (Suzuki, et al. 
# 2015)
d2 <- read_csv("data/example-data-sem-2.csv")
d2_codes <- read_csv("data/example-data-sem-2-codebook.csv")

d2
d2_codes

#' To assess validity in a basic way, we want to estimate all of
#' these latent variables and then assess whether they are 
#' correlated to the extent that we think they should be 
#' correlated.

validity_model <- "
  
  # Construct latent variables for all questionnaires 
  stress =~ stress_q1 + stress_q2 + stress_q3 + stress_q4 + stress_q5;
  pss =~ pss_q1 + pss_q2 + pss_q3 + pss_q4;
  panx =~ panx_q1 + panx_q2 + panx_q3 + panx_q4 + panx_q5;
  preq =~ preq_q1 + preq_q2 + preq_q3 + preq_q4 + preq_q5;

  # Correlate all of the latent variables
  stress ~~ pss + panx + preq;
  pss ~~ panx + preq;
  panx ~~ preq;

"

validity_model <- lavaan::sem(validity_model, data = d2, std.lv = TRUE)
summary(validity_model, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

#' From the standardised correlations (Covariances header)
#' we can see that our stress latent factor was very highly
#' correlated with the parental stress scale (r = 0.858),
#' indicating concurrent validity; that it was moderately 
#' correlated with parental anxiety, (r = 0.599), as we would
#' expect it to be, and that it was negatively correlated with
#' parental resilience (r = -0.467). This provides some additional
#' evidence that our questionnaire is a valid measure.



# Part 6: Discriminant validity using AVE -----------------------------------------

#' One statistic, developed by Fornell & Larcker (1981), has been suggested
#' for assessing the extent to which a factor is distinctly valid from 
#' similar concepts. 
#' 
#' The *'Average Variance Extracted (AVE)'* is a measure of the proportion of 
#' the variance across all of the indicator variables that is captured by
#' the factor rather than by the measurement error. So, for example, if 
#' the factor captures 50% of the variance across all of the items, around
#' 50% of the variance remains related to measurement error (or, more 
#' specifically measurement error *plus* contributions from other related
#' constructs that are captured by the questions, e.g. anxiety versus 
#' stress).
#' 
#' The AVE is often used in assessments of discriminant validity. For 
#' example, we want out parental stress factor to be *related to* the
#' concept of parental anxiety, but to be sufficiently different from
#' this concept (uniquely identified). This is commonly claimed if the
#' AVE of a particular factor is greater than the correlation between 
#' that factor and a factor of a related, but distinct, concept. This
#' is known as the 'Fornell-Larcker criterion'. If the factor passes the
#' Fornell-Larcker criterion, we have evidence to suggest that our
#' questionnaire is capturing this construct as a distinct 'thing' 
#' when compared to the other, related construct. If the factor does not
#' pass the Fornell-Larcker criterion, then we cannot say that our 
#' variables are precise or differentiated enough from the related concept
#' (i.e. our stress latent fact may also be capturing parenting anxiety).
#' 
#' 
#' We can use the semTools package to calculate the AVE for our factors:

semTools::AVE(validity_model)


#' In this case, our AVE for the stress factor is 0.703. This is higher than
#' its absolute correlation with parental anxiety and parental resilience, 
#' which means we have evidence that our factor (and its associated indicators)
#' are doing an effective job of differentiating between the concepts of
#' anxiety, resilience, and stress. The stress factor is not differentiated from
#' the other questionnaire's stress factor, strengthening the claim
#' of concurrent validity.

#' The AVE can also be calculated within the model itself. It is defined as 
#' the sum of all of the factor loadings divided by the sum of all of the
#' factor loadings plus the sum of all of the residual variances for each
#' indicator, but *only when the latent variable is standardised (i.e. has*
#' *a variance of 1)*, which is achieved by using the lv.std = TRUE argument.


validity_model_ave <- "
  
  # Construct latent variables for all questionnaires 
  # add factor loadings for AVE calculation
  stress =~ l1*stress_q1 + l2*stress_q2 + l3*stress_q3 + l4*stress_q4 + l5*stress_q5;
  pss =~ pss_q1 + pss_q2 + pss_q3 + pss_q4;
  panx =~ panx_q1 + panx_q2 + panx_q3 + panx_q4 + panx_q5;
  preq =~ preq_q1 + preq_q2 + preq_q3 + preq_q4 + preq_q5;

  # Correlate all of the latent variables
  stress ~~ pss + panx + preq;
  pss ~~ panx + preq;
  panx ~~ preq;

  # label the residual variances for indicators
  stress_q1 ~~ r1*stress_q1;
  stress_q2 ~~ r2*stress_q2;
  stress_q3 ~~ r3*stress_q3;
  stress_q4 ~~ r4*stress_q4;
  stress_q5 ~~ r5*stress_q5;

  # Calculate the AVE by summing the loadings squared, summing the variances,
  # and taking the proportion
  sum_l := l1^2 + l2^2 + l3^2 + l4^2 + l5^2;
  sum_r := r1 + r2 + r3 + r4 + r5;
  ave_stress := sum_l / (sum_l + sum_r)

"

validity_model_ave <- lavaan::sem(validity_model_ave, data = d2, std.lv = TRUE)
summary(validity_model_ave, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

#' Why might we want to do this? Well, for one, I think it's good to get
#' a sense for how things work! But for another, you may want to test 
#' statistically whether the difference between the the AVE and the 
#' between factor correlation is significant. If your AVE is only slightly
#' higher than the correlation, this might be due to sampling error so
#' you could want to have a stronger claim for discriminant validity.
#' 
#' Because we calculated the AVE by hand, this is a fairly easy extension to
#' make: we just need to find and label the correlation between the
#' factors we're interested in:
#' 

validity_model_ave <- "
  
  # Construct latent variables for all questionnaires 
  # add factor loadings for AVE calculation
  stress =~ l1*stress_q1 + l2*stress_q2 + l3*stress_q3 + l4*stress_q4 + l5*stress_q5;
  pss =~ pss_q1 + pss_q2 + pss_q3 + pss_q4;
  panx =~ panx_q1 + panx_q2 + panx_q3 + panx_q4 + panx_q5;
  preq =~ preq_q1 + preq_q2 + preq_q3 + preq_q4 + preq_q5;

  # Correlate all of the latent variables
  # label the correlation between stress and parental anxiety
  stress ~~ pss + cor1*panx + cor2*preq;
  pss ~~ panx + preq;
  panx ~~ preq;

  # label the residual variances for indicators
  stress_q1 ~~ r1*stress_q1;
  stress_q2 ~~ r2*stress_q2;
  stress_q3 ~~ r3*stress_q3;
  stress_q4 ~~ r4*stress_q4;
  stress_q5 ~~ r5*stress_q5;

  # Calculate the AVE by summing the loadings squared, summing the variances,
  # and taking the proportion
  sum_l := l1^2 + l2^2 + l3^2 + l4^2 + l5^2;
  sum_r := r1 + r2 + r3 + r4 + r5;
  ave_stress := sum_l / (sum_l + sum_r)

  # Test whether the difference between the AVE and the absolute correlation 
  # between the factors is statistically significant (and it must also be 
  # positive to pass the criterion!)
  ave_cor1_d := ave_stress - abs(cor1)
  ave_cor2_d := ave_stress - abs(cor2)

"

validity_model_ave <- lavaan::sem(validity_model_ave, data = d2, std.lv = TRUE)
summary(validity_model_ave, fit.measures = TRUE, ci = 0.95, standardized = TRUE, rsquare = TRUE)

#' Yes; not only is the AVE greater than the correlation
#' but it is significantly greater. We could also do additional
#' checks such as bootstrap sampling (from part 1) if we wanted
#' to be even more certain. We can also see that our factor is
#' divergent from the parenting relilience factor, implying that
#' they are distinct concepts and not just "two sides of the same
#' coin".

#' HOMEWORK/INDEPENDENT STUDY TASK:  Now that we've tested our newly 
#' created stress factor against concurrent, convergent, and 
#' discriminant validity: use what you have learned about AVE to 
#' assess and test whether the *alternative* parenting stress measure 
#' (pss) has discriminant  validity against parenting anxiety and 
#' parenting resilience (note that this is simulated data and doesn't 
#' reflect the true quality of the other questionnaires, it is only 
#' used as an example)



#' Well done on completing the short course! There has been a lot of
#' information today, so don't worry if you can't remember what everything
#' means. Keep a copy of these scripts and examples and keep them for 
#' reference for your own research. There are also references in the
#' slide deck for you to learn more.
#' 
#' Dr. Calum Webb (c.j.webb@sheffield.ac.uk)

