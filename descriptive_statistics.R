# Descriptives

# The first task for most data analyses is to get an idea of what our sample
# looks like. If you remember back to your intro to research methods class, the
# data we collect are typically samples of a larger population. We can't assess
# the whole population because that would take too long or require too many
# resources. We use samples to get an idea of the characteristics of the
# population. But because the sample is not the population, the characteristics
# of the sample will never be the same as the characteristics of the population.
# The best we can hope for is that they are close. Typically, the larger our
# sample size, the more confident we can be that its characteristics are good
# approximations of the population characteristics. 

# When we look at sample descriptives, or ways to describe our data both
# qualitatively and quantitatively, we aim to get an idea of what the population
# may look like, whether we've done a good job of sampling from the population
# at random, or maybe even whether there might be something wrong with a
# variable we've measured.

# Let's use the caffeine experiment as an example. In this example experiment,
# we wanted to test the hypothesis that caffeine consumption will increase
# performance on solving math problems by making subjects solve them quicker.
# This dataset contains data from an experiment where one group was given a hot
# cup of coffee to drink (treatment) and the other group was given a hot cup of
# filtered water to drink (control) prior to the math problem solving task.

dat <- read.csv("data/caffeine_experiment_1_cleaned.csv")

head(dat)

# Let's get the sample size, that may be useful later. If our dataset is
# formatted so that each row is a single person or case, the `nrow()` function
# should give us the sample size. 

n_total <- nrow(dat)

# Let's also get sample sizes for each condition. We can do this a couple of
# ways. One we've already learned, where we index which rows are in the control
# condition.

control_test <- dat$condition == "control"
control_index <- which(control_test)

# We can then take the length of this control index to get the number of
# subjects in the control condition

n_control <- length(control_index)

# We can subtract this from our total sample size, n_total, to get the treatment
# condition sample size.

n_treatment <- n_total - n_control

# We could instead use the `table()` function. `table()` takes a vector, or
# multiple vectors, and counts how many cases there are of each unique value.
# For the condition variable, we have two unique values (control and treatment).

table(dat$condition)
n_by_condition <- table(dat$condition)

# We could also get a percentage of cases in each condition by dividing the
# condition sample sizes by the total sample size.

n_by_condition / n_total

# We often want descriptives of the sample demographics. Gender, Race/Ethnicity,
# and Age are typical.

table(dat$gender)

table(dat$gender) / n_total

# Note that there is a 6 with nothing above it. These are missing values when we
# are using `table()` on a character string variable.

table(dat$ethnicity)

table(dat$ethnicity) / n_total

# R often defaults to printing around 8 decimal places. This can make it
# difficulty to read results. We can use the `round()` to to make it look nicer.

round(x = table(dat$ethnicity) / n_total, digits = 2)

# Finally, let's look at age. `table()` works for categorical or ordinal
# variables, but isn't very informative for contiuous or interval/ratio
# variables. We'll want to use measures of central tendency and spread/variance.
# There are missing values in the age variable, so we'll want to account for
# that.

mean(dat$age, na.rm = T)
median(dat$age, na.rm = T)

sd(dat$age, na.rm = T)
range(dat$age, na.rm = T)

# A great way to investigate the distribution of a continuous variable is to
# visualize the distribution with a histogram. We can use the `hist()` function.

hist(x = dat$age, breaks = 10)

# It looks like our age variable is highly skewed (skew right). This sample was
# likely take from a college campus where most individuals in the population are
# between ages 18 and 30. Most of the individuals in this sample are around ages
# 19 to 22. The mean and median are somewhat misleading because this isn't a
# normal distribution. Luckily, we aren't using age as an outcome, so it doesn't
# need to be normally distributed. All we're looking for is whether these ages
# make sense given the population we drew this sample from, which it does.


# We might also want to look at the distribution of gender, ethnicity, and age
# in each condition to make sure things aren't too unbalanced. We can look at
# the cross-classification of multiple categorical variables. This is also
# called a contingency table.

table(dat$gender, dat$condition)
table(dat$ethnicity, dat$condition)

# We can use the `aggregate()` function to get means of a continuous variable
# for different groups. In this case we compare ages across conditions. The
# first argument is a formula. A formula is made using the tilde (~). It should
# be to left of the one key on your keyboard and you'll need to use the shift
# key as well. A formula is made by putting an outcome variable on the left side
# of the tilde and any predictors or grouping factors on the right side. The
# names of the variables do not need to go in quotation marks for a formula.

age_by_cond_form <- age ~ condition # We'll see these more later this semester.

# The other arguments are the data argument, were we'll enter the name of our
# data.frame object, and the FUN argument, where we'll tell are which function
# we want to use on each group's age.

aggregate(formula = age_by_cond_form, data = dat, FUN = mean)

# We can use multiple grouping factors

aggregate(formula = age ~ condition + gender, data = dat, FUN = mean)

# Although there are differences in the distributions between conditions, we
# don't really have a good reason to believe that these differences will affect
# math problem response times or the effects of caffeine.

# Perhaps we want descriptives of each individual math problem response time.
# That would probably have been good to do before calculating average response
# times. Let's look at a few descriptives to make sure none of the items are
# exhibiting anything unusual. First we'll get an indexing vector for the
# individual response time variables. We can use the `grep()` function again.

rt_index <- grep(pattern = "rt_", x = colnames(dat))

dat[, rt_index]

# We can now use the `apply()` function. `apply()` takes a matrix or data.frame,
# and applies some function to either the rows or the columns. The first
# argument, X, is the matrix or data.frame. The second, MARGIN, tells R whether
# we want to apply a function to the rows or the columns. 1 = rows, 2 = columns.
# The third argument, FUN, gives the function we want to apply. 

# Columns means

rt_columns <- dat[,rt_index]

apply(X = rt_columns, MARGIN = 2, FUN = mean)

# We have NAs and forgot to use the na.rm argument. We can add a fourth argument
# after the FUN argument that passes information to the `mean()` function.

apply(X = rt_columns, MARGIN = 2, FUN = mean, na.rm = T)

# same as
colMeans(x = rt_columns, na.rm = T)

# We could do the same thing to calculate row means with MARGIN = 1

apply(X = dat[,rt_index], MARGIN = 1, FUN = mean, na.rm = T)

# same as
rowMeans(x = dat[,rt_index], na.rm = T)


# We can also pass along other functions to apply to rows or columns

# median of cols
apply(X = dat[,rt_index], MARGIN = 2, FUN = median, na.rm = T)

# standard deviation of cols
apply(X = dat[,rt_index], MARGIN = 2, FUN = sd, na.rm = T)

# range
apply(X = dat[,rt_index], MARGIN = 2, FUN = range, na.rm = T)

# Nothing looks too out of sorts. 

# Let's look at our outcome variables, response times. Let's create a mean score
# variable for out response times and call it "mean_rt"

dat$mean_rt <- rowMeans(dat[, rt_index], na.rm = T)

# Now we can look as descriptive statistics for the mean RT

mean(dat$mean_rt, na.rm = T)
median(dat$mean_rt, na.rm = T)
sd(dat$mean_rt, na.rm = T)

hist(dat$mean_rt, 15)

# The histogram shows two distinct distributions in our data that are relatively
# normal (wonder what's causing those differences).

# One more thing we might want to check is the correlation between our response
# time items. We'll go over correlations in more detail in a few weeks, but we
# can use correlations now as a descriptive statistic to get an idea of how
# related our variables are in the sample. We can use the `cor()` function.
# Because we have missing data, we'll want to also use the use = "pairwise"
# function, so that R removes missing values when calculating the correlation
# between pairs of variables. I'll also wrap the `round()` function around the
# `cor()` function to make things look nice.

cor(x = dat[, rt_index], use = "pairwise")

cor(x = dat$mean_rt, y = dat$age, use = "pairwise")

round(x = cor(dat[, rt_index], use = "pairwise"), digits = 2)

# If we want an overall mean score for response times to be an adequate
# representation of someones average math problem solving speed, we probably
# want our individual items to be fairly strongly correlated (r > .1). Weak or
# negative correlations between items would indicate that those that respond
# quickly to one item, tend to respond substantially faster or slower to another
# item, which may indicate that something else about the items is affecting
# response times and the mean probably isn't a good approximation of one's math
# problem solving speed. These correlations look great, so we should be fine
# taking the row mean for each person.



