# Distributions and graphical analyses

# A distribution describes how a variable is distributed. There are many
# distributions for describing different types of data, and each distribution is
# characterized by parameters. The one that we are most familiar with is the
# normal distribution. The normal distribution is characterized by two
# parameters, a mean we call "mu" (pronounced mew not moo), and a standard
# deviation we call "sigma". 

# R comes with functions that can sample data from a population with a specific
# distribution. These are call "random number generators" because they generate
# random numbers. If we want to sample (or simulate) data from a normal
# distribution, we can use the `rnorm()` function. `rnorm()` takes three
# arguments. The first is the sample size "n". The second is the population
# "mean" that we want to sample from. The third is the population standard
# deviation, "sd", that we want to sample from. Let's create a sample of 10,000
# observations with a population mean of 0 and standard deviation of 1. We'll
# save these samples to a variable we'll call "stress_1".

n <- 10000 # sample size
mu <- 0 # population mean
sigma <- 1 # population standard deviation

rnorm(n = n, mean = mu, sd = sigma)

# take a look at what these random samples 

stress_1

# You'll notice that your samples are different from my samples. And if you run
# the `rnorm()` function again, it will give you new samples. This is what a
# random number generator does, it gives you numbers at random.

# stress_1 is a sample from a larger population. We can get the sample mean of
# stress_1 with the `mean()` function.

mean(stress_1)

# We can also get the sample standard deviation with the `sd()` function.

sd(stress_1)

# Note that the sample mean is close to the population mean but it is not exact.
# Same thing with the sample standard deviation. We have a large sample, so our
# sample mean and sd will be good approximations of the population mean (mu) and
# standard deviation (sigma). Play around with different sample
# sizes to see how sample descriptive statistics change.

# run these two lines several times and see how a small sample size affects our
# estimates of the population mean and sd. 
small_sample <- rnorm(n = 10, mean = 0, sd = 1)
c("mean" = mean(small_sample), "sd" = sd(small_sample))

# compare that to a large sample size
large_sample <- rnorm(n = 10000, mean = 0, sd = 1)
c("mean" = mean(large_sample), "sd" = sd(large_sample))

# The smaller sample size was way off for both mean and sd. notice also that
# each time you create a sample, the sample mean and sd vary a lot compared to
# the large sample size. This will be important when we talk about hypothesis
# testing.

# Let's visualize what this normal distribution for stress_1 looks like. A
# histogram is the go-to way of visualizing distributions. A histogram takes a
# random variable (e.g. our stress_1 sample), breaks up the values into a
# number of bins or categories, and counts up how many observations are in each
# category. It then displays these counts with a bar plot without any gaps
# between the bars. We'll use the `hist()` function.

hist(x = stress_1)

# R does its best to choose an appropriate number of bins. Sometimes we want to
# choose that number ourselves to better visualize the distribution. We can do
# this by adding the "breaks" argument that tells the `hist()` function how many
# different bins to create.

hist(x = stress_1, breaks = 100)

hist(x = stress_1, breaks = 10)

# We see that the distribution decently approximates a normal distribution. The
# center of the distribution is around the population mean.

# If we wanted to save this histogram to our computer, the easiest way is to
# click the "Export" button on the plot window. You can copy the image to your
# clipboard and paste it somewhere else, or you can save the image with "Save as
# Image". 

# In R, we can add a number to a sample from  a normal distribution, just like
# we did with vectors a few lessons ago. This will shift the sample mean. If I
# add a 2 to the extraversion sample, it will increase the sample mean by 2 and
# shift the distribution to the right on the histogram.

hist(x = stress_1)
hist(x = stress_1 + 2)

# The sample mean is now the sample mean of stress_1 plus 2

mean(stress_1)
mean(stress_1) + 2
mean(stress_1 + 2)

# I can do the same thing but this time I'll subtract 10 from extraversion.

hist(x = stress_1 - 10)

# Note that the shape of the distribution does not change. Only its location.
# Adding and subtracting a single number will only shift the distribution
# positively or negatively. The sample mean will change but the standard
# deviation will not change.

sd(stress_1)
sd(stress_1 - 10)

# Remember standard deviation just describes how far our sample observations are
# from the mean. Adding a number to all of the observations, (i.e. changing the
# mean), will not affect how far the observations are from the mean.

# Let's visualize this a little more with a couple other arguments to the
# `hist()` function. We'll use "xlim" to specify the xaxis minimum and maximum.
# We'll also use the "add = TRUE" argument to put multiple histograms on the
# same plot. Finally, we'll use the "col" argument to specify a color for each
# histogram so we can differentiate them.

hist(stress_1, xlim = c(-10, 10), col = "lightblue")
hist(stress_1 + 5, col = "green", add = TRUE)
hist(stress_1 - 7, col = "red", add = TRUE)

# Notice each distribution has a different mean, but they are the exact same
# shape with the exact same standard deviation.

# If we want to affect the standard deviation, or how spread out the
# distribution is we must either multiply or divided the distribution by a
# number. If we want to make the distribution more spread out, which also
# increases the standard deviation, we can multiply it by a number.

hist(stress_1 * 4)
hist(stress_1 * 10)

# Notice that it maintains it's overall normal distribution shape, but width or
# "scale" of the distribution changes and becomes wider. The new standard
# deviation becomes the original standard deviation multiplied by the number.

sd(stress_1)
sd(stress_1) * 4
sd(stress_1 * 4)

# We can also shrink the scale or width of the distribution, and therefore the
# standard deviation, by dividing the variable by a number.

hist(stress_1 / 4)
hist(stress_1 / 10)

sd(stress_1)
sd(stress_1) / 4
sd(stress_1 / 4)

# Let's visualize this again

hist(stress_1 * 3, xlim = c(-20, 20), col = "blue")
hist(stress_1 * 2, col = "green", add = TRUE)
hist(stress_1 * 1, col = "red", add = TRUE)
hist(stress_1 / 2, col = "purple", add = TRUE)



# Boxplots ####

# We can also visualize the distribution of a variable with a boxplot. A boxplot
# graphs the median and min, max, and quantiles of a variable. We can use the
# `boxplot()` fuction in R. We'll also use the "range" argument to tell R to
# draw "whiskers" at the min and max.

boxplot(stress_1, range = 0)

# We can also draw this horizontally
boxplot(stress_1, range = 0, horizontal = T)

# This gives us similar information about the sample median and spread of the
# data, but doesn't really show us how the distribution is shaped.

# Visualizing experimental groups ####

# Let's simulate some data that might represent our caffeine and math
# performance experiment. We'll have two groups, a treatment group and control
# group. Let's assume that response times are normally distributed. The average
# response time, regardless of caffeine consumption is 60 seconds.

average_rt <- 60

# Let's also say that the treatment (caffeine) decreases response times by an
# average of 10 seconds when answering math questions.

caffeine_effect <- -10

# In our experiment, anyone that was given caffeine in the treatment condition
# should then have an average response time equal to the overall average RT
# regardless of treatment plus the effect of the treatment. We would thus expect
# the population mean response time to be mu = 60 seconds + -10 seconds.

treatment_mu <- average_rt + caffeine_effect

# Conversely, the control group that only received hot water to drink should not
# have any additional effect on their reaction time. Their population mean
# should just equal the average response time, mu = 60 seconds.

control_mu <- average_rt

# Finally, let's keep things simple and assume that the standard deviations from
# each group are both equal to 1.

treatment_sigma <- 1
control_sigma <- 1

# Now let's simulate or sample response times for each group. We'll get a sample
# of n = 250 participants for each group. Note that we are sampling from two
# different population distributions because they have different population
# means.

treatment_n <- 250
control_n <- 250

treatment_rt <- rnorm(n = treatment_n, mean = treatment_mu, sd = treatment_sigma)

control_rt <- rnorm(n = control_n, mean = control_mu, sd = control_sigma)

# Now let's get some descriptive sample statistics for each group.

mean(treatment_rt)
mean(control_rt)

sd(treatment_rt)
sd(control_rt)

# Our sample is decently large for each group so they are good approximations of
# the populations that we sampled them from.

# Let's visualize these two groups with a set of histograms

hist(treatment_rt, breaks = 10, xlim = c(45, 65), col = "brown")
hist(control_rt, col = "lightblue", add = T)


# To visualize the results with a boxplot, we'll have to transform the data into
# "long" format and create a variable that designates whether someone was in
# the treatment or control group.

# Let's create the condition variable first. We'll use the `rep()` function,
# which repeats some value or string a number of times. We'll want to repeat the
# character string "treatment" for each person in the treatment condition.

treatment_cond_id <- rep(x = "treatment", times = treatment_n)

# And we'll do the same with the control condition

control_cond_id <- rep(x = "control", times = control_n)

# Now we'll put them together into a "condition" vector with treatment coming
# first.

condition <- c(treatment_cond_id, control_cond_id)

# Now we'll create a vector of the response times for each group with treatment
# coming first.

rt <- c(treatment_rt, control_rt)

# Finally, we'll put both the condition variable and response time variable
# together into a data.frame and call it "dat".

dat <- data.frame(
  "RT" = rt,
  "Condition" = condition
)

head(dat)

# We can do all of the descriptive statistics functions we were using before on
# this dataset if we want to.

table(dat$Condition) # sample size for each condition

aggregate(formula = RT ~ Condition, data = dat, FUN = mean) # group means for RT

# Now we can use the boxplot function. We'll use the formula notation like we do
# with the `aggregate()` function. RT will be on the left (outcome) side, and
# Condition will be on the right (predictor/manipulation) side.

boxplot(formula = RT ~ Condition, data = dat, range = 0)


# The boxes do not overlap, and the group median RTs are quite different. We can
# clearly see that the treatment, caffeine, had a substantial negative effect on
# response times. This shouldn't really surprise us though, because we were the
# ones that created the data this way. We told R we wanted the treatment to have
# a strong negative effect. I encourage you to play around with the caffeine
# effect and see how that changes the group differences. You should also try
# changing the population standard deviations and see what happens.


# Visualizing bivariate relationships ####

# Sometimes we have two continuous variables and we want to visualize their
# relationship. In the caffeine experiment, an obvious confound is someone's
# general mathematical ability. Someone who's better at math will have faster
# response times on average. Let's simulate this situation.

# Let's assume we have a test for math ability that we administer before the
# experiment. The test is scored from 1 to 10, and the scores are normally
# distributed with a population mean of 5 and standard deviation of 1. Our total
# sample size in the experiment should be 250 treatment + 250 control = 500.

math_mu <- 5
math_sigma <- 1
total_n <- treatment_n + control_n
math_scores <- rnorm(n = total_n, mean = math_mu, sd = math_sigma)

hist(math_scores)
mean(math_scores)
sd(math_scores)

# Let's now assume that general math ability, as measured by the test, has a
# negative relationship with reaction times. In other words, the better at math
# someone is, the quicker they are at solving math problems. Let's say that a
# one-point increase in math test score is associated with a 5-second decrease
# in response time. So the effect of math ability should be -5.

math_ability_effect <- -5

# Now, just like we did above, let's generate some response times. Without
# adminstering a treatment of caffeine, the population mean (mu) should be the
# average response time, regardless of math ability, plus the effect of math
# ability times the math test score.

rt_mu <- average_rt + math_ability_effect * math_scores

# Let's stick with a population standard deviation (sigma) of 1 for simplicity.

rt_sigma <- 1

# Now let's create some response times.

rt <- rnorm(n = total_n, mean = rt_mu, sd = rt_sigma)

# Let's say we want to visualize the relationship between math scores and
# response times. We can use the `plot()` function. `plot()` takes an argument
# "x", which specifies what to plot on the x-axis, "y" for the y-axis, and then
# "type" to specify what type of plot. We'll put math scores on the x-axis,
# response times on the y-axis, and specify the type as "p" for points.

plot(x = math_scores, y = rt, type = "p")

# We see a very strong negative relationship between math scores and response
# times, just like we told R.

# Let's add one more step and incorporate our treatment effect. The popluation
# mean for treatment response times should be equal to the average overall
# response time, plus the effect of math ability, plus the effect of treatment.

# Let's select the math scores for just the treatment group. We'll create an
# index for the treatment group in the dat data.frame.

treatment_test <- dat$Condition == "treatment"
treatment_index <- which(treatment_test)

treatment_math_scores <- math_scores[treatment_index]

# Now we'll calculate the treatment population mean again, but with the math
# ability effect included.

treatment_mu <- average_rt + math_ability_effect*treatment_math_scores + caffeine_effect

# Now let's select the control condition math scores by removing the treatment
# index

control_math_scores <- math_scores[-treatment_index]

# Now we can calculate the control group population mean.

control_mu <- average_rt + math_ability_effect*control_math_scores

# specify the standard deviations

treatment_sigma <- 1
control_sigma <- 1

# Finally, let's simulate the response times from each distribution

treatment_rt <- rnorm(n = treatment_n, mean = treatment_mu, sd = treatment_sigma)

control_rt <- rnorm(n = control_n, mean = control_mu, sd = control_sigma)

# Now let's look at the scatter plot for the treatment condition between math
# scores and response times. Let's change the x- and y-axis labels with the
# "xlab" and "ylab" arguments. Let's color code it as well. Finally, let's
# specify the x- and y-axis ranges.

plot(x = treatment_math_scores, 
     y = treatment_rt, 
     type = "p", 
     pch = 1,
     xlab = "Math Scores", 
     ylab = "Response Time",
     xlim = c(0, 10),
     ylim = c(10, 60),
     col = "brown",
     main = "Study 1, math scores and response times")

# We can use the `points()` function to add data points to the current plot.
# Let's add the control condition data points.

points(x = control_math_scores, 
       y = control_rt,
       col = "blue",
       pch = 1)

# We can now see the relationship between math scores and response times for
# each experimental condition.

# As an added bonus, let's include a key or legend to show our readers what the
# colors mean. We'll use the `legend()` function.

legend(
  x = 5.5, # where to start left side of legend on plot
  y = 60, # where to start top side of legend on plot
  legend = c("Coffee", "Hot Water"), # labels for each group
  col = c("brown", "blue"), # colors for each group
  pch = 1, # specify the type of point used
  title = "Experimental Group" # give the legend a title
)

# Using graphs to get an idea of what your data looks like can help to tell a
# story about the relationships involved. However, graphical inspection is like
# descriptive statistics. It shows you what's going on in your sample, but can't
# really assure you that this is what's actually occurring in the population,
# nor can it tell you whether we should expect the same results if we were to
# conduct this study again. Keep in mind, we simulated the data for these
# examples, so we know what the true population relationships are because we
# told R what they are. This is not the case when we collect actual data. We
# will instead have to perform tests using inferential statistics to help us
# infer what's going on in the population beyond the small set of data we've
# collected.




