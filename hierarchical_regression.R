# Regression for Prediction - Hierarchical Regression

# Let's say we want to use the Big 5 personality factors to predict job
# performance, but we only want to choose the two best predictors. We know from
# past research that Conscientiousness (C), is one of the best predictors of job
# performance, so we'll include that one. Let's use hierarchical regression to
# find a second predictor that will be most helpful for predicting the outcome.

# The dataset "selection.csv" is a dataset of 100 employees from a warehouse.
# The variables include their scores on each of the Big 5 personality factors,
# openness (O), conscientiousness (C), extraversion (E), agreeableness (A), and
# neuroticism (N). The data also has supervisor ratings of the employee's
# job_performance (JP). 

dat <- read.csv("data/selection.csv")

head(dat)

# The first thing we want to do is run a model with conscientiousness as the
# only predictor.

fit_C <- lm(formula = JP ~ C, data = dat)

# We can look at the results if we want to but what we really want is the
# R-squared value for that model. We can get this by saving the results from the
# `summary()` function. Then access R-squared with the $ sign.

sum_C <- summary(fit_C)
R2_C <- sum_C$r.squared

# Now we want to add a second predictor to the model. Let's start with openness.

fit_CO <- lm(JP ~ C + O, data = dat)

# Let's also get R-squared for that model.

sum_CO <- summary(fit_CO)
R2_CO <- sum_CO$r.squared

# Now we can get the change in R-squared. To do this, we subtract the R-squared
# value of the model with fewer predictors from the model with more predictors.

R2_CO - R2_C

# In this case, R-squared increased by one-percent or .01. This doesn't seem
# like much, but let's test to see if it is a statistically significant
# difference with an F-test. To do that, we'll use the `anova()` function. The
# `anova()` function is used to compared models in R to see which one fits the
# data better, and to test whether they are actually different. We put the two
# models into the function this so:

anova(fit_C, fit_CO)

# Our F-value is printed out, along with the degrees of freedom. DF1 is under
# the "DF" column, and DF2 is under the "Res.Df" in the second row (model 2). So
# we'd write our F-test as F(DF1, DF2) = F-value, or F(1, 97) = 1.13. 

# We see that our p-value is great than .05, so our F-test is non-significant,
# meaning that adding openness as a second predictor is not accounting for a
# significant amount of variance in job performance. Let's write up the results.

# I conducted a hierarchical linear regression to investigate whether Openness
# predicts unique variance in job performance over-and-above Conscientiousness.
# The results suggest that Openness does not predict unique variance
# over-and-above the variance explained by Conscientiousness, ΔR2 = 0.01, F(1,
# 97) = 1.13, p = .290.

# Let's try one more. Extraversion.

fit_CE <- lm(JP ~ C + E, data = dat)
sum_CE <- summary(fit_CE)

# Model R2
R2_CE <- sum_CE$r.squared

# ΔR2
R2_CE - R2_C

# F-test
anova(fit_C, fit_CE)

# We see that Extraversion does indeed explain a significant amount of unique
# variance in job performance over-and-above Conscientiousness.

# I conducted a hierarchical linear regression to test whether Extraversion
# predicts unique variance that Conscientiousness does not account for. The
# results suggest that Extraversion explains a significant amount of unique
# variance in job performance over-and-above Conscientiousness, ΔR2 = 0.15, F(1,
# 97) = 18.20, p < .001. This suggests that adding Extraversion to the test
# battery for identifying those that perform well on the job improves prediction
# by roughly 15%.

# Try this with the other two personality factors, Agreeableness and Neuroticism


