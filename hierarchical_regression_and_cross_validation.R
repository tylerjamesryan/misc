# We've talked about using R2 as a measure of how well our model accounts for
# variance in the dataset we used to train our model. The hope is that if our
# model can account for a large amount of variance in the outcome for this
# dataset, it should account for a similar amount in a new dataset that the
# model was not trained on. One concern is that our model is too tailored to the
# dataset we trained it on. We call this "overfitting" the model to the data.
# When this happens, out model will acount for a large amount of variance in the
# outcome for the training dataset, but will not account for as much in a new
# dataset the model was not trained on. When we create a regression model for
# prediction, our goal is typically focused on predicting new data. If our model
# is suffering from overfitting, it won't be as useful when we try to use it to
# predict new data.

# One method to check whether overfitting is an issue for our model is
# cross-validation (CV). CV entails "training" a regression model on one set of
# data and then "testing" how well that model predicts the outcome in a new
# dataset. If the model is overfitting, R2 for the testing dataset will be
# substantially lower than R2 in the training dataset. This might then give us
# a better idea of how well our model would predict other outcome in the future.


# Let's see how we might go about this. I'll use the backetball.csv dataset as
# an example. This dataset has aggregated statistics for college basketball
# teams for seasons 2016-2017 and 2017-2018. I want to predict how many wins a
# team will earn in a season. I want to use the average number of points scored
# per game by the team (pts), the average number of points scored against the
# team per game (opp_pts). Obviously the more points a team scores on average,
# and the fewer points scored against the team, will lead to more wins during a
# season. But I also want to know what other aspects of the game are important
# for determining wins. Maybe I want to provide recommendations to the Wright
# State baskedball teams on what they should improve upon in order to achieve
# more wins. There are 3 other variables I want to investigate and see which is
# most predictive. They are the average percentage of field goals made (fg_per),
# the average percentage of 3-point shots made per game (X3p_per), and the
# average percentage of free throws made per game (ft_per). I also want to make
# sure that I am not overfitting my model because I want to be able to predict
# how many wins a team can expect in the future. I want to look at both R2 and
# ΔR2 in the training dataset and R2 in the testing dataset.

dat <- read.csv("data/basketball.csv")

# Let's start by splitting our data into training and testing datasets. The data
# contain two different seasons, 2016-2017 and 2017-2018. Let's use the
# 2016-2017 seaons as our training dataset and the 2017-2018 dataset as our
# testing dataset.

logical_test_17 <- dat$season == "2016-2017"
index_17 <- which(logical_test_17)
dat_train <- dat[index_17,]
dat_test <- dat[-index_17,]

# Now let's "train" our initial regression model. I want to use average number
# of points scored and average number of points scored against as my primary
# predictors.

fit_init <- lm(wins ~ pts + opp_pts, data = dat_train)
summary_init <- summary(fit_init)

# .87 or 87% of the variance explained, which is a lot
train_R2_init <- summary_init$r.squared 

# That's a lot of variance explained by our two predictors. There is some
# concern that using average points scored for and against a team in a
# particular year is really only predictive of wins for that year and may not
# give us a model that's useful for other years. Let's use cross-validation to
# see how much our model might be overfitted to the 2016-2017 season data. We'll
# do this by calculating "out-of-sample" R2 by making predictions for the
# 2017-2018 season (test data) using the 2016-2017 model that was trained on the
# "training" data.

# R doesn't have a convenient way of calculating out-of-sample R2, so we'll have
# to use the formula for R2 and calculate it ourselves. Here is the formula:

# SSR = sum( (y_obs - y_pred)^2 )
# SST = sum( (y_obs - y_mean)^2 )
# R2 = 1 - (SSR / SST)

# SSR is the sum-of-squares for our regression model with our predictors. y_obs
# is the observed values of our outcome, in this case it's the wins in our test
# dataset. 

test_wins_obs <- dat_test$wins

# y_pred is the number of wins our model predicts for the test data. We'll use
# the `predict()` function and use the "newdata" argument to make predictions
# for the test data.

test_pred_init <- predict(object = fit_init, newdata = dat_test)

# Now let's plug those into the SSR equation to get SSR for the initial model

test_SSR_init <- sum( (test_wins_obs - test_pred_init)^2 )

# For SST, we need the average (mean) number of wins for the test data.

test_wins_mean <- mean(dat_test$wins)

# Now we can use the SST formula.

test_SST <- sum( (test_wins_obs - test_wins_mean)^2 )

# Finally, we'll take the SSR and SST values and plug them into the R2 formula

test_R2_init <- 1 - (test_SSR_init / test_SST)

# We see that R2 in the training and testing dataset are pretty close. This
# gives us some reassurance that we aren't overfitting the model because it can
# predict outcomes for data it was not trained on.

c("Initial Model - Train" = train_R2_init, 
  "Initial Model - Test" = test_R2_init)

# Now, it seems obvious that the more points a team scores and the fewer points
# scored against them leads to more wins. Telling a basketball team that the
# secret to getting more wins is to score more points will get you laughed at.
# Let's try a few other predictors and see if either provide us with some better
# insights. I'll use assists (ast). Assists are when a player passess the ball
# to a teammate in a way that sets up that teammate to score.

# First, I'll train a new model with the training data, which now includes the
# ast variable as a predictor.

fit_ast <- lm(wins ~ pts + opp_pts + ast, data = dat_train)

# I can now use a hierarchical regression analysis, comparing my initial model
# with my model with an additional predictor, to see whether adding assists as a
# predictor is helping me account for more variance in the outcome. We'll use
# the `anova()` function, which compares the two models with an F-test. We'll
# also get the ΔR2 by extracting R2 from the new model and subtracting the
# initial model R2.

anova(fit_init, fit_ast)

summary_ast <- summary(fit_ast)
train_R2_ast <- summary_ast$r.squared
R2_change_ast <- train_R2_ast - train_R2_init

# Looks like adding assists doesn't increase R2 by a significant amount, ΔR2 =
# 0.00, F(1,347) = 0.65, p = .419. Assists only accounts for 0.03% more variance
# over-and-above the other predictors.

# Let's also assess the out-of-sample R2 for this new model so that we can
# compare it to the out-of-sample R2 for the initial model. We'll do the exact
# same thing we did above, but we'll use this new model instead of the initial
# model.

# Get model predictions for the new data using our model with assists
test_pred_ast <- predict(fit_ast, newdata = dat_test)

# Get SSR with the assists model
test_SSR_ast <- sum( (test_wins_obs - test_pred_ast)^2 )

# Get out-of-sample R2 for the assists model. We use the test_SST that we
# already calculated above.
test_R2_ast  <- 1 - (test_SSR_ast / test_SST)

c("Assists Model - Train" = train_R2_ast, 
  "Assists Model - Test" = test_R2_ast)

# Let's do this all one more time for blocks. Blocks are when a defensive player
# blocks the shot of an offensive player, preventing them from scoring. We'll do
# the exact same thing we did with assists above.

fit_blk <- lm(wins ~ pts + opp_pts + blk, data = dat_train)
anova(fit_init, fit_blk)
summary_blk <- summary(fit_blk)
train_R2_blk <- summary_blk$r.squared
R2_change_blk <- train_R2_blk - train_R2_init

test_pred_blk <- predict(fit_blk, newdata = dat_test)
test_SSR_blk <- sum( (test_wins_obs - test_pred_blk)^2 )
test_R2_blk  <- 1 - (test_SSR_blk / test_SST)

c("Assists Model - Train" = train_R2_blk, 
  "Assists Model - Test" = test_R2_blk)

# Again, we're not really seeing much improvement adding the new predictor
# "blocks". The F-test is not significant, ΔR2 = 0.00, F(1,347) = 2.84, p =
# .093.


# Bonus content ####

# Using a regression for prediction framework can be useful in some contexts,
# but often times it won't reveal why our models aren't improving. Let's think
# causally why the assists and blocks predictors don't really help us predict
# wins. Here's one potential verbal theory to explain the relationships between
# these variables.

# Assists causes points and points causes wins 

# ast -> pts -> wins

# Blocks causes fewer opponent points, which in turn casuses more wins

# blk -> opp_pts -> wins

# In both cases we have a mediator (pts or opp_pts). One arrow is going into
# pts/opp_pts and the other arrow is leaving pts/opp_pts. Theoretically, assists
# doesn't directly cause wins. Assists indirectly causes wins because it leads
# directly to more points which in turn directly causes wins. Same with blocks.
# Blocks do not directly lead to wins. Blocks prevents opponent points directly,
# which in turn causes wins. By including pts and opp_pts in the regression
# models, we've now controlled for these mediators (closed the back doors). We
# then see in our model that the true direct relationship between assists and
# wins, or blocks and wins, is nearly non-existent.



