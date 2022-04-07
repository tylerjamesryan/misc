
# Many of our predictors are categorical rather than continuous. They have
# discrete groups that may predict or affect the outcome differently. Let's
# start off with a familiar example: the coffee experiment from the beginning of
# the semester. In that experiment, we wanted to know whether drinking a cup of
# coffee affects response times on a math test. The experiment involved randomly
# assigning half of the participants to a treatment group where they drank a cup
# of coffee before taking a math test, whereas the other group was given a hot
# cup of water to drink before taking a math test. The response times were
# recorded for each person, and we used a t-test to test whether there are
# significant differences between the means of the two groups. Let's do that
# again as a refresher.


# read in data

dat <- read.csv("data/caffeine_experiment_1.csv")

# calculate average response time for each person

rt_item_names <- c("rt_1", "rt_2", "rt_3", "rt_4", "rt_5", 
                   "rt_6", "rt_7", "rt_8", "rt_9", "rt_10")

rt_items <- dat[, rt_item_names]

dat$mean_rt <- rowMeans(rt_items, na.rm = T)

# Conduct t-test

tt_1 <- t.test(mean_rt ~ condition, data = dat)

# View results
tt_1

# We see that there was a significant difference between the two groups in their
# mean response times. The treatment significantly reduced the response time,
# such that the control condition (53.71 s) had significantly longer response
# times on average compared to the treatment condition (45.76 s). We can get the
# difference in the means between the groups by subtracting the control
# condition from the treatment condition, which is roughly 7.95 s.

tt_1$estimate[2] - tt_1$estimate[1]

# A t-test tests whether there are group differences. The grouping variable can
# be seen as a categorical predictor of the dependent variable / outcome. In
# this case, we are trying to predict response times with the condition
# predictor. With this in mind, let's see what happens when we conduct a
# regression instead, with mean_rt as the outcome and condition as the
# predictor.

# Conduct the linear regression
reg_1 <- lm(mean_rt ~ condition, data = dat)

# View results
summary(reg_1)

# You'll notice two parameters, the intercept, and the slope which looks like
# "conditiontreatment". Look at the estimate for the intercept. It is 53.71 s,
# which is the mean response time for the control group, identical to what we
# found with our t-test. Now look at the estimate for the first predictor, -7.95
# s. That is identical to the difference in the mean between the control and
# treatment condition that we found with our t-test. Our regression model has
# tested for group differences just like the t-test, but with slightly different
# math (which isn't important for our purposes). The "conditiontreatment"
# predictor indicates the increase or decrease in the outcome for the treatment
# group compared to some reference, in this case it's the control group. The
# intercept in a regression is the expected level of the outcome when all other
# predictors are zero. So the intercept is the average response time for the
# control group.

# When we only have one categorical predictor with two groups, we will
# essentially get the same results with either test. So why use a regression
# model? Because we can add other predictors, something we can't do with a
# t-test.

# Let's say that participants weren't assigned to the two conditions at random.
# We wanted to run the treatment condition in the mornings so people in the
# experiment wouldn't be drinking coffee in the afternoon and have a difficult
# time sleeping at night because they've consumed caffeine too close to their
# bed time. Likewise, the control condition was largely conducted in the
# afternoons and evenings so that any caffeine participants consumed in the
# morning is largely out of their system. This may seem like a smart decision
# for experimental design, not only to be considerate towards the participants
# but also to prevent confounding the results because of caffeine consumption
# prior to the experiment. One possible issue though is that some participants
# may be more inclined to participate in the afternoons and evenings than in the
# mornings. Younger participants may not want to get up as early as older
# participants, so they will sign up for the afternoon/evening experiments more
# often. This means we would have more younger participants in the control
# condition than in the treatment condition. In other words, age affects
# treatment assignment, which then may affect response times. Age may also
# directly affect response times because younger participants tend to be quicker
# at solving math problems than older people in general. The causal model looks
# like this:

# condition -> mean_rt
# condition <- age -> mean_rt

# Age is a fork in this model, confounding our ability to measure the
# relationship between condition and response time. We must control for it. But
# a t-test does not allow for this. We instead need to use linear regression.

reg_2 <- lm(mean_rt ~ condition + age, data = dat)
summary(reg_2)

# By using a regression, we can control for potential confounds to better
# measure the treatment effect. In this case, the intercept is the expected
# response time for someone that hasn't received the treatment and is at age 0.
# Likewise, the "conditiontreatment" effect is the expected increase/decrease in
# response times for someone that has received the treatment compared to someone
# in the control condition, after controlling for the effects of age.

# I can write up the results like so:

# I conducted a multiple linear regression with condition and age predicting
# mean response times. The results suggest that there are significant
# differences in mean response time between the treatment and control
# conditions, b = -7.94, SE = 0.69, p < .001, such that the treatment condition
# had a lower average response time, controlling for age. Age did not have a
# significant relationship with mean response times, controlling for treatment
# condition, b = 0.16, SE = 0.13, p = .213.

# Again, the important thing to remember is that the conditionteatment predictor
# is estimating the difference in response time between the treatment condition
# and some reference group. R automatically chose the control condition as the
# reference group for us. We'll see how to change the reference group below.

# One more thing that might be useful to understand is what this looks like for
# our regress line. I'm going to create some fake data for our model to make
# predictions for. You don't need to know how to do this, it's just for
# demonstration purposes.

fake_data <- expand.grid(
  condition = unique(dat$condition),
  age = 0:70
)

fake_data$predicted_rt <- predict(reg_2, newdata = fake_data)

plot(
  predicted_rt ~ age,
  data = fake_data[which(fake_data$condition == "treatment"),],
  type = "l",
  lty = 1,
  ylim = c(40, 65),
  frame.plot = F,
  xaxs = "i",
  yaxs = "i",
  xlab = "Age",
  ylab = "Predicted Response Time"
)

lines(
  predicted_rt ~ age,
  data = fake_data[which(fake_data$condition == "control"),],
  type = "l",
  lty = 2
)

text(30, 57, "control")
text(30, 49, "treatment")

# Notice that the slope (i.e. the relationship) between age and RT is the same
# for both groups. The treatment group is just lower because on average they
# were faster than someone of similar age in the control group. 


# 3 or more levels ####

# Another advantage of using linear regression is that we can use categorical
# predictors with more than two levels, just like we do with an ANOVA. An ANOVA
# tests whether there are significant differences between groups, typically when
# we have three or more levels in the grouping variable. Using a categorical
# predictor with more than two levels in a regression model does the same thing
# an ANOVA does. Let's say we want to know whether there are significant
# differences between the different ethnic groups in our sample.

# Notice that our ethnicity variable is coded as numeric in the dataset. If we
# plug in the ethnicity variable without recoding ethnicity to a categorical
# variable, R will treat it like a continuous predictor.

summary( lm(mean_rt ~ ethnicity, data = dat) )

# This model suggests that as ethnicity increases, so do response times. That
# doesn't really make any sense, we obviously don't want this. So let's recode
# the ethnicity variable to a categorical level of measurement.

# Want to recode it like this:

# 1 = "American Indian or Native Alaskan"
# 2 = "Asian or Pacific Islander"
# 3 = "Black (not Hispanic)"
# 4 = "Hispanic"
# 5 = "White (not Hispanic)"
# 6 = "Middle-Eastern, Arabic"


# We could create a bunch of tests and indices like we always do, but here's a
# shortcut for recoding this variable. First we'll create a key. The key has
# each ethnicity label in the order that we want the numbers to be recoded as.

ethnicity_key <- c("American Indian or Native Alaskan",
                   "Asian or Pacific Islander",
                   "Black (not Hispanic)",
                   "Hispanic",
                   "White (not Hispanic)",
                   "Middle-Eastern, Arabic")

# Now we can use the ethnicity variable in our dataset, which is just numbers
# 1-6, as an index of this ethnicity_key, like this:

dat$ethnicity

ethnicity_key[dat$ethnicity]

dat$ethnicity <- ethnicity_key[dat$ethnicity]

# Now we can use this predictor as a categorical variable in our regression
# model.

reg_3 <- lm(formula = mean_rt ~ ethnicity, data = dat)

summary(reg_3)

# Notice that the regression model treats the ethnicity predictor similar to how
# it treated the condition predictor. R sets one of the groups as the reference,
# and then compares all other groups to that reference. In this case, R set the
# "American Indian or Native Alaskan" category as the reference, so each
# predictor is the increase or decrease in response time compared to that
# reference category. For example, "ethnicityAsian or Pacific Islander" indicates
# that Asians and Pacific Islanders had 1.69 s longer response times compared to
# American Indians or Native Alaskans on average. "ethnicityBlack (not
# Hispanic)" indicates that non-Hispanic Black participants had shorter response
# times by 0.64 s compared to American Indians or Native Alaskans. We can also
# see by the p-values that none of the other ethnic groups are significantly
# different in response times from American Indians or Native Alaskans. 

# These parameters will tell us whether one group is different from the reference
# group, but won't really tell us whether there are significant differences
# between the groups as a whole like we would get if we ran an ANOVA. Luckily we
# can use the `anova()` function on a single model to get an F-test and test
# whether there are differences between the groups in general.

anova(object = reg_3)

# We can see that the F-test is non-significant, suggesting that ethnic groups
# do not significantly differ from one another in mean response times.

# factors ####

# Factors are a class in R that is a mix between numeric and character classes,
# made specifically for categorical data. When you use a categorical variable
# that is as character string, like we did above with ethnicity, R automatically
# converts it to a factor class. We can see what this looks like by using the
# `as.factor()` function on the ethnicity variable.

dat$ethnicity <- as.factor(x = dat$ethnicity)
dat$ethnicity

# notice that the values don't have quotation marks around them like we would
# find with character data. You'll also notice that R tells you in the output
# how many levels (categories) are in the factor variable. We can also access
# the levels with the `levels()` function

levels(x = dat$ethnicity)

# When creating a factor variable with `as.factor()`, R will automatically sort
# the levels in alphabetical order and set the first level as the reference. That
# is why our all of our categories are compared to "American Indian or Native
# Alaskan" in the regression model, it's the first category when sorted in
# alphabetical order. Perhaps we instead want to compare each category to
# another reference group (e.g. Hispanic). We can use the `relevel()` function
# like so:

dat$ethnicity <- relevel(x = dat$ethnicity, ref = "Hispanic")
levels(x = dat$ethnicity)

# Notice now that Hispanic has been moved to the front of the factor levels. If
# we conduct the linear regression again, it will now be the reference to compare
# all other categories.

summary( lm(mean_rt ~ ethnicity, data = dat) )



