# Inferential Statistics - t-tests and bivariate correlations

# Random number generators and setting the seed ####

# Let's return to our caffeine experiment (again, we're not done yet). Remember
# in the distributions and graphs script we created our own data with a normal
# distribution random number generator `rnorm()`. Every time that's used, it
# samples new numbers at random. So the numbers you see are not the same as what
# I see. We can fix this by using the `set.seed()` function. Without going into
# unnecessary details, this function tells R to generate random numbers from a
# certain starting point. Let's me show you.

# I'll pick a "seed". In this case 100, but the number is arbitrary
set.seed(100)

# Now I'll sample 5 random numbers from a standard normal distribution
rnorm(n = 5, mean = 0, sd = 1)

# The numbers should be 
# -0.50219235  0.13153117 -0.07891709  0.88678481  0.11697127

# If we run rnorm again, it will give us new numbers
rnorm(n = 5, mean = 0, sd = 1)

# But if we "set the seed" prior to sampling the random numbers, we'll get the
# same ones again

set.seed(100) 

rnorm(n = 5, mean = 0, sd = 1)

# Now what you see should be the same thing that I see. Again, if you run the
# rnorm function again, it will not give the same 5 numbers anymore. Setting the
# seed just determines a starting point for R. Now when we simulate data, we can
# use set.seed() to get the same answers.

# Simulate Data ####

set.seed(436) # choose some seed, again this is more or less arbitrary

# specify sample sizes
treatment_n <- 300
control_n <- 300
total_n <- treatment_n + control_n

# create math scores
math_score_mu <- 5
math_score_sigma <- 1
math_scores <- rnorm(n = total_n, mean = math_score_mu, sd = math_score_sigma)

# create condition variable

treatment_cond_id <- rep("treatment", times = treatment_n)
control_cond_id <- rep("control", times = control_n)

condition <- c(treatment_cond_id, control_cond_id)

# Let's put our math scores and condition variables into a data.frame

dat <- data.frame(
  "math_score" = math_scores,
  "condition" = condition
)

# specify effects for treatment and math score

caffeine_effect <- -10
math_ability_effect <- -5

# let's also specify the reaction time overall average and the standard
# deviation.

average_rt <- 60
rt_sigma <- 1

# Now, we could create indices to select participants from each group to specify
# the population means in separate steps. Let's try something easier. I'm going
# to create a test for being in the treatment group.

treatment_test <- dat$condition == "treatment"

# Now I'm going to convert the logical TRUE and FALSE values in the
# treatment_test to 1s and 0s, with TRUE = 1 and FALSE = 0. I can just use the
# `as.numeric()` function.

as.numeric(treatment_test) # take a look at what it does

# I'm going to create a new variable called "condition_dummy". "Dummy" means
# we've turned a variable that identifies two groups into a set of zeros and
# ones. We'll see this a lot when we talk about regression and categorical
# variables.

dat$condition_dummy <- as.numeric(treatment_test)

# We can now create our population means. Instead of creating it in two steps,
# we're going to use the "condition dummy" variable like this.

rt_mu <- average_rt + math_ability_effect * dat$math_score + caffeine_effect * dat$condition_dummy

# Try running just the "caffeine_effect * dat$condition_dummy" portion. Notice
# that participants that were in the treatment group and that we dummy coded
# with a 1 receive the caffeine effect (-10) because we multiplied the caffeine
# effect by 1. Those that were in the control group and were dummy coded with a
# 0 receive no caffeine effect because we multiplied the caffeine effect by 0.


# Now we can simulate our response times for all participants and save it as a
# variable in our dat object.

dat$rt <- rnorm(n = total_n, mean = rt_mu, sd = rt_sigma)

# t-tests ####

# Let's test for differences between the two groups. If there are significant
# differences between our two groups, we have evidence to say that our treatment
# had an effect. We'll use an independent samples t-test. Again, without getting
# too far into the weeds, an independent samples t-test takes the difference
# between two group means and puts that difference on the scale of a
# t-distribution, called a "t-score". A t-distribution is like a normal
# distribution, but has "heavy-tails", such that values can have a wider range
# than a standard normal distribution. It has one parameter, the degrees of
# freedom. Here's a histogram of sample drawn from a t-distribution with 2
# degrees of freedom compared to a normal distribution.

set.seed(-103)
hist(rnorm(n = 10000, mean = 0, sd = 1), breaks = 50, col = "red")
hist(rt(n = 10000, df = 2), breaks = 1000, add = T, col = "lightblue")

hist(rt(n = 10000, df = 2), 100, xlim = c(-20, 20))
hist(rnorm(n = 10000, 0, 1), 10, xlim = c(-10, 10))

# Notice that most of the observations in the t-distribution are around zero,
# and that the further we get from zero, the fewer observations there are. When
# we get a t-score, that represents the difference between our groups if it were
# converted to the t-distribution scale. The standard t-distribution, like the
# one plotted, shows us how likely we are to observe any given t-score if there
# truly were no differences. If there are truly no differences between the
# groups at the population level, we're most likely to observe t-scores close to
# zero. We are not very likely to observe t-scores far from zero, as the
# histogram shows.

# If the group difference is small, the t-score will be close to zero. If the
# difference is large, positively or negatively, the t-score will be further
# from zero. The further from zero, the more evidence we have to say that there
# are truly differences between the groups and the treatment does create and
# effect. In other words, the differences are not likely due to random chance.

# This t-score is compared to a t-distribution with degrees of freedom equal to
# df = group_1_n + group_2_n - 2. If we want to be 95% confident that the
# t-score difference was not due to random chance and that our treatment
# actually did something, we would compare our t-score to a critical value for
# that t-distribution at 95% confidence (alpha = .05). This can be converted to
# a p-value, which tells us the probability of observing the differences and
# t-score that we calculate in our sample, under the assumption that there truly
# are no differences in the population. This definition of the p-value has
# nuance and it's important! A p-value is not the probability that something
# will be replicated, nor is it the probability that our treatment caused the
# differences we observed. All it says is that the null hypothesis (that there
# are no differences) is not likely to have produced the data we sampled. This
# is why we say we "reject the null hypothesis" and never say we "accept the
# alternative hypothesis". The alternative hypothesis might still be wrong! The
# p-value cannot tell you otherwise.

# Enough of the rant and technical mumbo jumbo. R will do all that fancy math
# stuff for us. Let's conduct a t-test with the `t.test()` function. We first
# should specify our null and alternative hypotheses.

# H0: There is no effect for caffeine on response times

# H1: Caffeine affects response times

# 2 tailed t-test

# Now let's conduct the test. We'll use the formula method like we've done
# before

tt_1 <- t.test(formula = rt ~ condition, data = dat)

# If we run tt_1 we'll get a summary of the results printed to the console

tt_1

# We can also access our results with the $

tt_1$statistic # t-statistic
tt_1$parameter # t-distribution degrees of freedom
tt_1$p.value # p-value
tt_1$estimate # group means

# The difference is huge statistically. The observed difference is about 10
# seconds (which is what we told R when we created the data above). The critical
# t-value for a distribution with 594.85 degrees of freedom is around 1.65. The
# t-score is 22.84, which is very far from zero. Additionally, our p-value is
# less than 0.05. The difference between the treatment and control condition is
# statistically significant. The difference in group means is ~ 10 seconds.
# Whether this is practically significant is up for interpretation. 10 seconds
# might not sound like a substantial decrease, but as a percentage, it's roughly
# a 28% decrease in response time, which seems practically significant. A
# commonly reported effect size for t-tests is Cohen's d. We can calculate it
# like this.

# First we need to calculate the "pooled standard deviation", or the average
# standard deviation between the two groups. This is just the square-root of the
# average of the two group variances. Remember the variance is just the square
# of the standard deviation. We'll use the `aggregate()` function to get the
# variances for each group.

group_var <- aggregate(rt ~ condition, data = dat, FUN = var)

pooled_variance <- mean(group_var$rt) # aka the average variance
pooled_sd <- sqrt(pooled_variance)

# Now we need to divide the group mean difference (or differences in the group
# sample means) by this pooled standard deviation.

group_mean <- aggregate(rt ~ condition, data = dat, FUN = mean)
rt_difference <- group_mean$rt[1] - group_mean$rt[2] # treatment mean - control mean

cohen_d <- rt_difference / pooled_sd

# Cohen's d ranges between 0 and 2. This effect size is very large. Check out
# this table for interpretation suggests.
# https://en.wikipedia.org/wiki/Effect_size#Cohen's_d

# We can write this up in APA format like so:

# I conducted an independent samples t-test to test whether there are
# significant differences between the treatment and control conditions. The
# results suggest that caffeine consumption prior to the math exam had a
# significant negative effect on the response times compared to drinking hot
# water, t(594.85) = 22.84, p < .001, d = 1.86. On average, participants that
# consumed a cup of coffee prior to the task responded 9.83 seconds faster than
# participants that drank a cup of hot water.



# Correlational tests ####

# t-tests are used to compare the means of some continuous out come between two
# groups. Correlation tests are used when we want to see if two continuous
# variables are related. In this case, we want to know whether math ability is
# related to response times on a problem solving task. We can use the `cor()`
# function to get a correlation between two variables. 

cor(dat$math_score, dat$rt, use = "pairwise")

# We see that the two are strongly negatively correlated, but `cor()` does not
# give us an inferential statistical test. This is fine for descriptive
# statistics and exploring data, but we want to test an hypothesis. We need to
# use `cor.test()`. We can use the formula method, but this time BOTH variables
# must go on the right side of the tilde ~.

# H0: There is no relationship between math ability and response times
# H1: There is a relationship between math ability and response times

ct_1 <- cor.test(~ math_score + rt, data = dat)

# just like `t.test()`, we can view a summary printed to the console, or access
# the individual results with the $. We interpret the correlation in terms of
# standard deviations because it is a standardized effect. In this case, a
# 1-standard deviation increase in math score is associated with a .70 decrease
# in average response time. We might want to convert this to an unstandardized
# effect. We must multiply the correlation by the standard deviation of response
# time divided by the standard deviation of math score.

math_score_sd <- sd(dat$math_score)
rt_sd <- sd(dat$rt)

unstandardized_effect <- (rt_sd / math_score_sd) * ct_1$estimate

10 * unstandardized_effect

math_scores <- 0:10
predicted_rt <- average_rt + math_scores*unstandardized_effect

plot(dat$math_score, dat$rt, type = "p")
lines(
  x = math_scores,
  y = predicted_rt,
  col = "blue"
)

# Notice that this unstandardized effect is approximately equal to the
# population effect of math score that we specified above when we generated the
# data.

# We can write up the results again, like so:

# I performed a bivariate correlation test to investigate the relationship
# between math score and response time. The results suggest that math score and
# response time are strongly, significantly, and negatively correlated, r =
# -.70, t(598) = -24.23, p < .001. For every one-standard-deviation increase in
# math score, response times are associated with a decrease by .70 standard
# deviations.





