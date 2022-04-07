# These data are based on a study investigating the effects of emotional
# intelligence on wisdom. Emotional intelligence is a general ability to
# perceive others emotions, express emotion to effectively communicate empathy to
# others, and manage and control one's own emotions. Wisdom is the general
# ability to integrate knowledge and experience and tolerate uncertainty in
# order to better guide decision making. Our theory suggests that those with
# greater emotional intelligence should have a better understanding of the
# emotional states, experiences, and reasoning of others. This deeper level of
# understanding others should therefore lead to making wise decisions when
# interacting with others.

# We collected data on 200 individuals measuring both emotional intelligence and
# wisdom. Create a regression model to investigate whether the relationship
# between emotional intelligence and wisdom in this sample is consistent with
# the theory.

dat <- read.csv("data/emotional_intelligence.csv")

summary(dat) # check for missing data

# Let's visualize the bi-variate (2-variables) relationship as a scatter plot.
# I'll put my predictor on the x-axis and my outcome on the y-axis.

plot(
  x = dat$emotional_intelligence,
  y = dat$wisdom,
  xlab = "Emotional Intelligence",
  ylab = "Wisdom"
)

# The purpose of a linear regression model is to try and fit a line that best
# describes our data. We could try and do this

# Fit Regression Model ####

# We'll use the lm() function which stands for 'linear regression model'. We use
# it the same way we use the t.test() and cor.test() functions. We'll specify a
# formula with our dependent/outcome variable on the left side of the formula
# and our predictor on the right side. We'll also want to tell the lm() function
# where to get those variables from, so we'll use the `data = dat` argument.
# Finally, we'll save this to an object. I'll call it `fit`, because we are
# fitting a line to our data.

fit <- lm(wisdom ~ emotional_intelligence, data = dat)

# We can then look at the results with the `summary()` function.

summary(fit)

# We can see that the parameter for the relationship between EI and W is b =
# 0.02. This suggests that a one-point increase in EI scores is associated with
# a 0.02-point increase in W scores. These are self-report scales, so the
# unstandardized parameter may not be very meaningful and in this case can make
# the relationship look very weak. Let's convert the standardized parameter to
# the standardized parameter. We'll first get the parameter with the `coef()`
# function. 

coef(fit)

b_ei <- coef(fit)["emotional_intelligence"]

# next we need the standard deviations for EI and W

sd_ei <- sd(dat$emotional_intelligence)
sd_w <- sd(dat$wisdom)

# Finally, we'll use this formula to convert from unstandardized to
# standardized: B_xy (beta) = (b_xy / sd_y) * sd_x

(b_ei / sd_w) * sd_ei

B_ei <- (b_ei / sd_w) * sd_ei

# We see now that the relationship is actually fairly moderate. We would say
# that a one-standard deviation increase in EI is associated with a 0.42
# standard deviation increase in W.

# Also, notice that when we have ONE predictor, the standardized parameter is
# the same as the correlation between the two variables.

cor(dat$emotional_intelligence, dat$wisdom)

# We also see that if a person scores a zero on the EI test, we would expect
# their W score to be b = 1.66. This isn't very meaningful because the lowest EI
# score one can get on the test is 60. We typically don't interpret the
# intercept in regression models unless it has some substantial meaning.

# We also see a `Std. Error` column in the summary. This is the standard error
# for the parameters. If we did this study a million times and ran a regression
# model on the different samples, we'd get different parameters each time. In
# fact, we would have a distribution of parameters with a mean and standard
# deviation. This is called a sampling distribution. The standard error from a
# single sample is an estimate of the standard deviation of the theoretical
# parameter sampling distribution. 

# Think about it in terms of measurement. When we measure something (table or
# box) we want those measurements to be reliable, so that every time we measure
# it we get the same result. When estimating a regression parameter, we are
# trying to measure the relationship between the predictor and the outcome. We
# want our measurement of the relationship to be reliable. If it is unreliable,
# how can we know that what we've measured this time will be the same if we
# measure it a second time?. And which one is the true relationship? The
# standard error describes how uncertain we are that we've measured the
# relationship reliably. Put another way, the SE describes whether we can expect
# to get a similar parameter estimate if we did this study again. If the SE is
# large, we're probably not very certain that what we've estimated is close to
# the true relationship. If SE is small, we can have more confidence in the
# validity of our parameter estimate.

# We want to compare the size of the effect (the parameter estimate of the
# relationship) to the amount of uncertainty (SE) we have that we've measured
# the relationship reliably. We do this by dividing the parameter estimate by
# its standard error (b / SE). This gives us our t-value test statistic, which R
# has conveniently calculated for us in the summary output. We see that the EI
# and W relationship has a t-value of 6.49. R also gives us the degrees of
# freedom which is 198. R gives us a p-value for the t statistic. We can see
# that this relationship is statistically significantly different from zero
# because the p-value is less than .05. R gives us more information about the
# regression model we created, but we'll save some of that for a later lecture.

# Confidence Intervals ####

# The SE gives us some idea of how uncertain we are about our measurement of
# the relationship between the variables. We can get a more clear idea of this
# uncertainty with confidence intervals (CI). CI give us an idea of the range of
# parameter estimates our measurement could take if we did this study many many
# more times. If we want 95% confidence, we can look up a z-score in a z-table
# for a 2-tailed test with alpha = .05, which is roughly 1.96. We then multiply
# our SE by 1.96 and subtract it from our parameter estimate to get the lower
# bound of our CI (lower = b - 1.96*SE). To get the upper bound, we simply add
# this to our parameter estimate instead (upper = b + 1.96*SE). Since we're
# using R, we can use the `confint()` function to get the CIs for our parameter
# estimates. We'll specify that we want 95% confidence intervals with the
# "level" argument.

confint(object = fit, level = .95)

# We see that, with 95% confidence, we can expect our estimate of the
# relationship to range between 0.01 and 0.02 if we did this study many more
# times. A common usage of CIs is to look and see if they cross zero. If they
# do, such that the lower bound is negative and the upper bound is positive,
# this indicates that we can't be sure whether the relationship is positive,
# negative, or zero. This is actually the p-value, but in a different form. The
# confidence interval for the EI -> W relationship does not contain zero, so we
# can be 95% confident that the positive relationship we observed was not due to
# random chance.

# Assessing the quality of our model ####

# An important step with any model is checking whether it meets certain
# assumptions and how well it fits our sample data. A common statistic we use is
# R-squared, which represents the percentage of the variance in our outcome that
# our predictors account for. The summary output tells us that our R^2 value is
# roughly 0.18, which means we're explaining roughly 18% of the variance (or
# differences) in wisdom scores with our EI predictor. This is pretty good, but
# also means that 82% of the variance in wisdom scores is still unexplained or
# unaccounted for by EI alone.

# An important assumption is that our residuals are normally distributed. We can
# check this by looking at a histogram of the residuals. First, let's get the
# residuals with the `residuals()` function. Then, we'll use the `hist()`
# function

# wisdom - wisdom_pred

fit_resid <- residuals(fit)
hist(fit_resid, 20)

# In this case, the residuals look approximately normally distributed. Not
# perfect, but it's not far off enough to make us think our model is invalid.
# Another useful inspection is to plot the residuals and the predicted values of
# our outcome (wisdom) in a scatterplot. We can get the predicted values with
# the `predict()` function. We'll then use the `plot()` function with our
# predicted values on the x-axis and our residuals on the y-axis

fit_pred <- predict(fit) # y-hat from the slides
plot(
  x = fit_pred,
  y = fit_resid,
  xlab = "Predicted",
  ylab = "Residual"
)

# What we want to see is a relatively random scattering of the points. If we see
# patterns or lines or groupings, this may indicate that our model is
# misspecified or is missing an important predictor. 

# Another similar inspection involves plotting our predicted values of wisdom
# against our observed values.

plot(
  x = dat$wisdom,
  y = fit_pred,
  xlab = "Observed Wisdom",
  ylab = "Predicted Wisdom"
)

# Ideally this would be a straight line of points, which would indicate that
# we've perfectly predicted our widom data with our model. In reality this never
# happens. But we should see a moderate to strongly positive relationship. In
# our case, we see a modest positive relationship between what our model
# predicts wisdom scores should be, and what they actually are. This is somewhat
# adequate, but it indicates that there are other aspects of wisdom that
# emotional intelligence can't account for on its own. We'll need to bring in
# more predictors to better understand or predict wisdom, but that will be for
# another day.

# Graphing our results ####

# One last thing we should do is create a plot for our readers that shows the
# relationship between W and EI. An easy way to do this is to first create fake
# or simulated EI scores. Since EI scores range from roughly 62 to 133, we'll
# just create a range of scores.

range(dat$emotional_intelligence)
ei_range <- 62:134 # We could also use the `seq()` function

# Now we'll feed the `predict()` function these hypothetical EI scores with the
# "newdata" argument. The `predict()` function needs a data.frame, so we'll put
# the ei_range object in a data.frame(). It also needs the same column names
# that were used in the data for our regression model (i.e. we need to name the
# fake ei scores "emotional_intelligence" in the data.frame)

ei_df <- data.frame(
  "emotional_intelligence" = ei_range
)

# Now we'll feed this dataframe to the predict function and assign the output to
# a new variable in our ei_df dataframe

ei_df$wisdom_predict <- predict(object = fit, newdata = ei_df)

# Finally, we'll use the `plot()` function to visualize the linear relationship
# between EI and W that our model suggests. We'll use the type = "l" argument to
# plot a line, and the lwd = 2 to adjust the width of the line.

plot(formula = wisdom_predict ~ emotional_intelligence, 
     data = ei_df,
     type = "l",
     lwd = 2)

# Let's also see how this looks with our observed data. We'll use the `points()`
# function with our original data.

points(
  formula = wisdom ~ emotional_intelligence, 
  data = dat)

# We now have a graphical display of what our regression model predicts wisdom
# scores should be across the levels of EI and what they actually are in the
# real data.

# Let's wrap this up by writing a short description of the results.

# 1. what did you do why did you do it
# 2. what are the stats?
# 3. what does it mean, or how to interpret it?


# I conducted a linear regression analysis to investigate the relationship
# between emotional intelligence and wisdom. The results suggest that emotional
# intelligence scores have a modest and statistically significant and positive
# relationship, b = 0.02, SE = 0.00, p < .001, B = 0.42, R^2 = 0.18. This
# suggests that a one-point increase in emotional intelligence scores is
# associated with a 0.02-point increase in wisdom scores.


