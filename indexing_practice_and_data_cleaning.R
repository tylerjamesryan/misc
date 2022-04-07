# More practice with indexing with a data cleaning example

# I would recommend removing everything from your environment and starting
# fresh. You can do this by clicking on the broom in your environment panel. An
# R command you can also use is rm(list = ls()), which does the same thing.

# Scenario ####

# Past research has found that math ability declines with age. A group of
# researchers wanted to replicate this finding. They performed a pilot study on
# the Amazon's survey platform "Mechanical Turk". They administered a math test
# with 5 questions to a sample of 100 Adults.

# Our task as data cleaners is to prepare the data for analysis. There are
# several issues with this dataset. 

# One concern when using an online survey is that participants will not pay
# attention and just answer randomly, sometimes called "careless responding".
# The survey the researchers used records response times for each participant on
# each item. Each item should have taken at least 5 seconds or more to answer.
# If the participant answered the items too quickly (average RT < 5 sec), they
# were marked as being careless. We need to remove those participants from the
# dataset.

# The math test score also needs to be calculated for each person. 

# The survey also included a question asking if the participant was currently
# enrolled in post-secondary school as a student. This question is irrelevant to
# the analysis the researchers will conduct, so it should be removed.

# In addition to these tasks, we need to make any other adjustments to the
# dataset that would help the researchers perform their analyses.

# We will have to use indexing to accomplish these tasks.

# Data Cleaning ####

# Import the dataset math_age_mturk_pilot_study.csv with the `read.csv()`
# function. 

data_file_path <- "data/math_age_mturk_pilot_study.csv"

df <- read.csv(file = data_file_path)

# check out the dimensions of this data.frame()

nrow(df) # number of rows / participants
ncol(df) # number of columns / variables

# Let's look at the first few rows to get an idea of what the dataset looks like

head(df)

# Let's also look at the classes for each variable in the dataset

str(df)

# Uh oh, the age variable was imported as a character string vector. Let's look
# at bit closer to see why. Select the age column for all rows.

df[ , "age"]

# It looks like the survey had participants enter their age with a free response
# type question format. Some entered their age with words rather than
# numbers. Don't ever do this when you create a survey! We'll want to change
# these responses to numbers.

# We can see that the participant in row 61 entered their age as "Sixty". 

df[61, "age"]

# Let's fix that.

df[61, "age"] <- 60

# Next, participant 72 entered "Fifty-Seven" for their age

df[72, "age"]

# Let's fix that too

df[72, "age"] <- 57


# Let's confirm that all of the ages are now entered as numbers

df[ , "age"]

# Notice that the age variable is still a character vector. It still has
# quotations marks around all of the numbers. The last thing we have to do is
# convert this age column to a numeric class. We'll use the `as.numeric()`
# function.

as.numeric(df[ , "age"]) # look and see what it does

df[, "age"] <- as.numeric(df[, "age"])

str(df)

# Remove Participant with no test data ####

# Looking good. The dataset isn't too large, so it might be useful to scroll
# through the whole thing and take a look to see if anything else is out of
# order.

df


# It looks like the responses to the math test items for participant 51 were not
# recorded for some reason. We'll want to remove that participant.


# We know that participant 51 is in row 51. We can index row 51 and remove it
# from our data.frame().

# Select row 51 to see their data and confirm it is the one with not math test
# reponses.

df[51, ] 

# No remove that participant by putting a minus sign in front of the index

df[-51, ]

df <- df[-51, ]

# Remove Careless Respondents ####

# Now we'll want to remove the careless participants. We'll need
# to create an index that selects the careless respondents. 

# Careless respondents are identified with a "1" in the "careless" column and
# non-careless respondents are identified with a "0".

df[, "careless"]

# Let's start by creating a logical test. We want to know whether someone has a
# 1 or a 0 in the careless column. In other words, we want a logical vector of
# TRUE and FALSE values where TRUE indicates that someone was careless (has a 1)
# and FALSE indicates that they were not careless (has a 0). Use the "==" test

df[, "careless"] == 1

careless_test <- df[, "careless"] == 1

# We now have a logical vector indicating whether someone was identified as
# being careless or not. We now need a vector of indices for just the careless
# participants (i.e. those the careless_test vector resulted in TRUE). We can
# use the `which()` function to identify which participants were careless.

which(careless_test)

careless_index <- which(careless_test)

# We can now use our careless_index vector to select the rows with respondents
# that were identified as being careless. Select the careless respondents and
# all of the columns to look at their data.

df[careless_index,  ]

# We want these participants removed from the data, so we can put a minus sign
# in front of the careless_index.

df[-careless_index, ]

# We want to update our data.frame() with these careless respondents
# removed.

df <- df[-careless_index, ]

# Check out the number of rows of the new data.frame() to make sure participants
# were removed

nrow(df)

# Also check the careless variable again to ensure that there are no more
# careless participants.

df[, "careless"]

# Calculate Math Test Scores ####

# Now we'll want to create a math test score for each participant by summing up
# their scores to each item. One common issue with research is the presence of
# missing data. Sometimes participants skip questions. Sometimes the computers
# we use to collect the data mess up and fail to record responses. Let's check
# our dataset for missing values with the summary() function.

summary(df)

# Notice that item_3 and item_4 have missing data (NAs). R doesn't know what to
# do with missing data when we try to do math. Anything we add to the NA values
# will result in NA as well. So participants with missing data will have NA for
# their test score. We'll learn to deal with this issue in another class. For
# now, we'll calculate test scores anyway.

# A "brute force" way of doing this is selecting the column of each item
# individually and adding those vectors together.

# Select item_1 for all participants in the data.frame() and save it to
# its own object.

df_clean <- df

df_clean[ , "item_1"]

item_1 <- df_clean[, "item_1"]

# Do the same thing with the other items

item_2 <- df_clean[, "item_2"]
item_3 <- df_clean[, "item_3"]
item_4 <- df_clean[, "item_4"]
item_5 <- df_clean[, "item_5"]

# If you remember a couple classes ago, we saw that vectors of the same length
# can be added together. We'll do that with all the items to get a math test
# score.

item_1 + item_2 + item_3 + item_4 + item_5

math_test_score <- item_1 + item_2 + item_3 + item_4 + item_5

# Now we want to create a new variable in the data.frame and fill it with the
# test scores. We can use the `$` to create the column since this is a
# data.frame(). I'll call my new variable "math_score".

df_clean$math_score <- math_test_score

# Remove Student Variable ####

# We need to remove the student variable because it isn't important to
# the research at hand. We must create an index that selects the student column
# using a combination of the `colnames()` and `which()` functions.

# Create a logical vector using a test

colnames(df)

colnames(df) == "student"

student_column_test <- colnames(df) == "student"

# Now wrap that logical vector in the `which()` function

which(student_column_test)

student_column_index <- which(student_column_test)

# Remove the student column by putting a minus sign in front of the index. Make
# sure you put the index on the column slot of the square brackets

df_clean[, student_column_index]

df_clean[, -student_column_index]

df_clean <- df_clean[, -student_column_index]

# Write dataset to .csv file ####

# Finally, we'll want to save this dataset as something new to our data folder
# with the `write.csv()` function.

# Create a file path and name the dataset something new
clean_data_file_path <- "/Users/tylerryan/OneDrive - Wright State University/School/Class/Spring 2022/psy_4020_adv_dsgn_exp/data/math_age_mturk_pilot_study_cleaned.csv"

# Write the dataset to a .csv file.

write.csv(x = df_clean, file = clean_data_file_path, 
          na = "", row.names = FALSE)

# Look in your data folder and verify that it was saved

