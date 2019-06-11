# Let's try to stick with just a couple of packages! ####
library(tidyverse)

# Initial slides about the point of the workshop - examples of how to teach
# basic stats methods in R and Python - in R I will cover data simulation,
# the basics of data viz, t-tests for determining the difference
# (as measured by one variable) between groups of people (for example), and 
# correlation and regression for determining the relatioship(s) between 
# variables 

# Data simulation - to determine the difference between two groups ####
# Our two groups will be labelled "Group_1" and "Group_2" with our measured
# or dependent variable labelled "DV"
# We will simulate our data such that the difference between our two
# populations is that of a "medium" effect size (Cohen's d = .5)

RNGversion("3.5")
set.seed(1233)
DV <- c(rnorm(24, 1000, 20), rnorm(24, 1010, 20))
Group <- factor((c(rep("Group_1", 24), rep("Group_2", 24))))
our_data <- tibble(Group, DV)

# Let's check our dataframe
str(our_data)

# This looks fine - our first variable (Group) is a factor while our second (DV)
# is a numerica variable

# First let's visualise using ggplot2

our_data %>% 
  ggplot(aes(x = Group, y = DV, colour = Group)) +
  geom_boxplot(alpha = .2, width = .2) +
  geom_jitter(alpha = .5, width = .05) +
  guides(colour = FALSE)

# As we can seem the median for Group_1 looks lower than the median for Group_2
# Let's determine whether this difference is statistically surprising

# t.test to compare the DV for Group_1 vs. Group_2 ####

group_1_data <- filter(our_data, Group == "Group_1")
group_2_data <- filter(our_data, Group == "Group_2")

t.test(group_1_data$DV, group_2_data$DV)

# So, with this dataset we can conclude there is a difference between our
# two groups as the p-value associated with the t-test is < .005

# PETER - NOT SURE IF THIS WOULD BE USEFUL OR NOT BUT I COULD DEMONSTRATE
# WHAT WOULD HAPPEN IF WE SIMULATED THE SAME EXPERIMENT 1000 TIMES
# AND DETERMINED OVER THE 1000 EXPERIMENTS THE NUMBER OF TIMES WE WOULD
# FIND A STATISTICAL DIFFERENCE BETWEEN THE GROUPS - IT'S A NICE WAY TO 
# ILLUSTRATE SAMPLING ERROR

library(broom)

RNGversion("3.5")
all_data <- NULL

for (i in 1:1000) {
  set.seed(1233 + i)
  DV <- c(rnorm(24, 1000, 20), rnorm(24, 1010, 20))
  Group <- factor((c(rep("Group_1", 24), rep("Group_2", 24))))
  our_data <- tibble(Group, DV, i)
  all_data <- rbind(all_data, our_data)
}

all_results <- NULL

for (n in 1:1000) {
  group_1_data <- filter(all_data, Group == "Group_1", i == n)
  group_2_data <- filter(all_data, Group == "Group_2", i == n)
  
  result <- tidy(t.test(group_1_data$DV, group_2_data$DV))
  all_results <- rbind(all_results, result)
}

all_results %>% 
  ggplot(aes(x = p.value)) + geom_histogram(binwidth = .005)

nrow(filter(all_results, p.value > .05))

# so 589 out of our 1000 simulations fail to detect the difference even though
# one is present in the population

# I CAN EASILY DELETE THE ABOVE BIT DEMONSTRATING SAMPLING ERROR IF YOU THINK
# IS TOO MUCH - I LIKE IT THOUGH AS IT'S A NICE WAY TO ILLUSTRATE THE CONCEPT
# VIA SIMULATION

# Now let's look at the relationship between 2 variables ####
# We need to load the MASS package to use the mvrnorm() function - this allows
# to sample from a multivariate distribution

library(MASS)

# Sample size is n
n <- 500

# A vector of means of our two variables
mu <- c(1000, 2000) 

# Covariance of our 2 variables is equal to Pearson's R * SD_var1 * 
# SD_var2. If we know the variance for each of our variables we can 
# calculate the sd. We can then use these values to work out the
# covariance we need for any particular Pearson's r value

# For the below example to give us a Pearson's r of .5 and variance for
# var1 = 100, and variance of var2 = 50 we have covariance = .5 *
# sqrt(100) * sqrt(50) which gives us 35.35534 - let's call is 35
myr <- 35

# The 2 x 2 covariance matrix where we have the variance of variable 1, 
# the covariance of variables 1 and 2, the covariance of variables 1
# and 2 and the variance of variable 2
mysigma <- matrix(c(100, myr, myr, 50), 2, 2) 

RNGversion("3.5")
set.seed(1234)
my_data <- (mvrnorm(n, mu, mysigma, empirical = TRUE))
colnames(my_data) <- c("Var_1", "Var_2")
my_data <- as_tibble(my_data)

write_csv(my_data, "mv_data.csv")

# Let's visualise the data first ####


my_data %>%
  ggplot(aes(x = Var_1, y = Var_2)) + 
  geom_smooth(method = "lm", se = FALSE) +
  geom_point(alpha = .5)

# We can see there is a positive relationship between our two variables
# But how can we determine whether the line we have drawn is a good model for
# our data?

# We can compare the 'fit' of this line (which is a regression line) to the 
# 'fit' of our mean (which is really just another model) to our data.

# We can add the mean as a line to our visualisation

mean(my_data$Var_2)

# Our mean is 2000 so we plot a horizontal line using geom_hline() through the
# point 2000 on the y-axis

my_data %>%
  ggplot(aes(x = Var_1, y = Var_2)) + 
  geom_smooth(method = "lm", se = FALSE) +
  geom_point(alpha = .5) +
  geom_hline(aes(yintercept = 2000), colour = "red")

# Now let's build two models - the first where the mean of Var_1 is the model of 
# Var_2, and the second where we have each point on Var_1 predicting its 
# partner point on Var_2

model_1 <- lm(Var_2 ~ 1 , data = my_data)
model_2 <- lm(Var_2 ~ Var_1, data = my_data)

# We can use the anova() funciton ot compare the two models to each other to 
# determine what one is the better fit to the data

anova(model_1, model_2)

# If the difference in the models' fit is statistically significant, we can 
# see that in the F-ratio and the p-value.  The lower the Residual Sum of 
# Square (RSS), the better the fit to the data.  We can see from the above
# that model_2 is a better fit to the data than just using the mean as a model.

# We can get the output of the linear model below - we see that if Var_1 were
# to equal 0, the regression line would intercept the y-axis at about 165.
# For every incease in Var_1, the increase in Var_2 is .35.
# The R-squared value tells us that about 25% of the variability in Var_2 is
# accounted for by Var_1.

summary(model_2)

# We also need to build some diagnostice plots ####
# Linear models are built on a nunber of assumptions - violation of these
# can weaken what we can conclude from the model.

plot(model_2)

# For the Residuas vs. Fitted plot, we expect to see a scattered distribution
# Non-linear relationships beteen our variables would be evident from this
# plot showing a 'shape'.  One of the assumptions of a linear model is that
# the relationship between the variables we want to model is linear - not
# much good trying to fit a linear model when this isn't the case!

# One of the key assumptions of a linear model is that the residuals (error) is
# normally distributed - the easiest way to explore this is to look at a Q-Q 
# plot - you should see the points fall on the diagonal if the residuals are
# normally distributed.

# The scale-location plot should look like a scattered distribution meaning that 
# the fit of the model doesn't really change for different values -
# ideally, the red line should be horizontal and through 0.

# For the residuals vs leverage plot, we shouldn't see any points outside
# Cook's distance (dashed red line).  If we do, then those points are having
# an unduly highly influence on our model and we may want to consider removing
# them - or fit a different type of regression model. 

# PETER - COULD ALSO ADD AN EXAMPLE WITH MORE THAN TWO VARIABLES TO DEMONSRATE
# MULTIPE LINEAR REGRESSION - LET ME KNOW IF I SHOULD PUT THAT IN - IT'S EASY
# ENOUGH TO DO
