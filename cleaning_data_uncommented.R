# Cleaning data

# Data that we collect is often not in a format that ready to begin analyzing.
# The first step in data analysis is often cleaning and formatting the data.

# In this example experiment, we wanted to test the hypothesis that caffeine
# consumption will increase performance on solving math problems by making
# subjects solve them quicker. This dataset contains data from an experiment
# where one group was given a hot cup of coffee to drink (treatment) and the
# other group was given a hot cup of filtered water to drink (control) prior to
# the math problem solving task.

# Read in the caffeine_experiment_1.csv dataset. 

dat <- read.csv("data/caffeine_experiment_1.csv")

# Take a look at the dataset with the `head()` function

head(dat)


# Notice that there's a few missing values (NA). Missing data is common,
# especially with survey data. Sometimes this isn't a huge issue if there aren't
# a lot of missing values. For example, the missing values for the gender
# variable is not a huge deal for this experiment. We don't have a good reason
# to believe that gender significantly impacts math ability or the affect
# caffeine would have. However, if the missing value is for a variable that is
# important to our study, such a condition, we would likely have to remove that
# subject from our sample.

# Look for NAs in the condition variable

dat$condition

# We can create an index of the NAs by creating a test with the `is.na()`
# function inside the `which()` function

condition_missing_index <- which(is.na(dat$condition))

# We now have an index that shows us which cases (subjects) have an NA for the
# condition variable. We can now remove these cases and update our "dat" object.

dat <- dat[-condition_missing_index, ]

# We can now confirm that these cases have been removed

dat$condition

# There's also subject names in the dataset. Experimenters need to de-identify
# data so that the data can't be connected back to the individual that
# partcipated. To create an index of the first and last name variables, we'll
# create a test that checks if a column name is equal to "first_name" OR if the
# column name is equal to "last_name".

subj_names_test <- colnames(dat) == "first_name" | colnames(dat) == "last_name"

subj_names_index <- which(subj_names_test)

# Now we can update our dataset without the subject names

dat <- dat[,-subj_names_index]

# Some variables were assigned numbers for categories that aren't informative
# (e.g. gender, ethnicity, condtion). We'll want to change those.

# We'll start with gender. In our dataset zeros represent males, and ones
# represent females. We can create an index for each to modify dat.

male_index <- which(dat$gender == 0)
female_index <- which(dat$gender == 1)

dat[male_index, "gender"] <- "male"
dat[female_index, "gender"] <- "female"

# Next, we'll do the same thing with the condition variable. In this case, ones
# represent the control condition and twos represent the treatment condition.

control_index <- which(dat$condition == 1)
treatment_index <- which(dat$condition == 2)

dat[control_index, "condition"] <- "control"
dat[treatment_index, "condition"] <- "treatment"

# Next we'll change the ethnicities. Now we could do the same thing as before
# where we create an index for each ethnicity. There are 6 different ethnicities
# in this dataset, numbered 1 through 6. To speed things up, we can create a
# vector of the character strings for each ethnicity.

ethnicities <- c("American Indian or Native Alaskan",
                 "Asian or Pacific Islander",
                 "Black (not Hispanic)",
                 "Hispanic",
                 "White (not Hispanic)",
                 "Middle-Eastern, Arabic")

# Now we can use the dat$ethnicity vector as an index to select each
# label from the character vector of ethnicities.

ethnicities[dat$ethnicity]

dat$ethnicity <- ethnicities[dat$ethnicity]

# Finally, subjects were given 10 math problems. We need to calculate the mean
# response time for each subject. We can use the `rowMeans()` function to
# calculate a response time mean for each subject.

# First, we'll create an a vector with the response time variable names

rt_var_names <- c("rt_1", "rt_2", "rt_3", "rt_4", "rt_5",
                  "rt_6", "rt_7", "rt_8", "rt_9", "rt_10")

# This is perfectly fine to just type out the names and select columns in dat
# with this vector. 

dat[,rt_var_names]

# Sometimes that can be cumbersome to type all that out. A handy trick is to use
# the `grep()` function. `grep()` takes some character string and tries to find
# matches in a vector of character strings. It then return an index of matches.
# The first argument (pattern) is the character string pattern we want to match.
# In this case, all the response time variables start with "rt_" and then some
# number. We'll search for "rt_" in the column names of dat. The second argument
# (x) is the vector of charcter strings we want to search through, in this case
# it is the column names of dat.

rt_index <- grep(pattern = "rt_", x = colnames(dat))

dat[, rt_index]

# This is especially useful when we have, for example, survey data where there
# are many items for many tests and we need to calculate scores for each.

# Now that we have an index of all the response time trials, we'll calculate a
# mean response time score for every subject with the rowMeans argument. One
# thing we'll have to account for is missing data in the reaction times. R
# cannot calculate a mean (or sum, difference, variance, etc) with missing data
# unless we tell it to remove the NAs when it does its calculation.

# For example, if I have a vector like the one below, with some numbers and a
# missing value, and I want to calculate the mean, I have to include the "na.rm
# = TRUE" argument.

a <- c(1, 5, -23, .0001, NA, 75, 12)
mean(a)
mean(a, na.rm = T)

# We'll need to do the same thing with the `rowMeans()` function

dat$mean_rt <- rowMeans(dat[,rt_index], na.rm = T)


# We've cleaned our dataset sufficiently to start analyzing it. We'll want to
# save it now with a name indicating that it was "cleaned".

write.csv(x = dat[,-ncol(dat)], file = "data/caffeine_experiment_1_cleaned.csv",
          na = "", row.names = FALSE)











