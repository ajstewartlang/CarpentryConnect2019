library(tidyverse)
library(broom)
library(afex)
library(emmeans)

# Main task 1 - Simulate data and run t-tests ####
# Group A from population with mean = 1000, sd = 20
# Group B from population with mean = 1010, sd = 20
# This equates to a Cohen's d of .5 as (1010-1000)/20 so a medium effect size

# First let's plot an approximation of the population for each sample - let's
# sample 1000 people from each group

set.seed(1234)
DV <- c(rnorm(10000, 1000, 20), rnorm(10000, 1010, 20))
condition <- c(rep("Condition_A", 10000), rep("Condition_B", 10000))

my_data <- tibble(condition, DV)

# and plot the data
my_data %>%
  ggplot(aes(x = DV, fill = condition, alpha = .5)) +
  geom_histogram(data = filter(my_data, condition == "Condition_A"), bins = 50) +
  geom_histogram(data = filter(my_data, condition == "Condition_B"), bins = 50) +
  guides(fill = FALSE, alpha = FALSE)
  
# Now let's sample 50 individuals from each population
set.seed(1234)
DV <- c(rnorm(50, 1000, 20), rnorm(50, 1010, 20))
condition <- c(rep("Condition_A", 50), rep("Condition_B", 50))

my_data <- tibble(condition, DV)

# Plot the data
my_data %>%
  ggplot(aes(x = DV, fill = condition)) +
  geom_histogram(data = filter(my_data, condition == "Condition_A"), bins = 10) +
  geom_histogram(data = filter(my_data, condition == "Condition_B"), bins = 10) +
  guides(fill = FALSE, alpha = FALSE)

# Generate summary stats
my_data %>%
  group_by(condition) %>%
  summarise(mean_dv = mean(DV), sd_dv = sd(DV))

# Run a t-test to see if difference Group A vs Group B is significant ####
t.test(my_data[condition == "Condition_A",]$DV, my_data[condition == "Condition_B",]$DV)

# Can we get the output in tidy format? Answer: yes...
tidy(t.test(my_data[condition == "Condition_A",]$DV, 
            my_data[condition == "Condition_B",]$DV))

# What if we ran 1,000 simulations? ####
all_data <- NULL

set.seed(1234)
for (i in 1:1000) {
  DV <- c(rnorm(50, 1000, 20), rnorm(50, 1010, 20))
  condition <- c(rep("Condition_A", 50), rep("Condition_B", 50))
  my_data <- tibble(condition, DV, i)
  all_data <- rbind(all_data, my_data)
}

# Run 1000 t-tests
all_results <- NULL
for (n in 1:1000) {
  my_data_filt <- all_data %>% filter(i == n)
  result <- tidy(t.test(my_data_filt[condition == "Condition_A",]$DV, 
                        my_data_filt[condition == "Condition_B",]$DV))
  all_results <- rbind(result, all_results)
  
}

# How many of those detect a signficant difference?
count(all_results %>% filter(p.value < .05))

# Main task 2 - Linear model with the built-in mtcars dataset ####
# is mpg predicted by hp?
# Build model
model <- lm(mpg ~ hp, data = mtcars)

# Examine model parameters
summary(model)

# Are we violating any assumptions?
plot(model)

# We are violating some assumptions - let's remove the two likely outliers 
# We need to covert the rownams to a column variable in order to filter
renamed_mtcars <- mtcars %>%
  rownames_to_column()

filtered_mtcars <- filter(renamed_mtcars, (rowname != ("Maserati Bora")) & 
                            (rowname != ("Lotus Europa")))

# Create a new linear model based on the filtered dataset - check the model
# parameters and the assumptions.
model <- lm(mpg ~ hp, data = filtered_mtcars)
summary(model)
plot(model)

# Plot the data
filtered_mtcars %>% 
  ggplot(aes(x = hp, y = mpg)) + 
  geom_point() +
  geom_smooth(method = "lm")

# Test new linear model against the mean as a model
model_null <- lm(mpg ~ 1, data = filtered_mtcars)

# Visualise the mean as a model of our data and the linear model as a model of
# our data
mean_intercept <- mean(filtered_mtcars$mpg)

filtered_mtcars %>% 
  ggplot(aes(x = hp, y = mpg)) + 
  geom_point() +
  geom_hline(yintercept = mean_intercept, colour = "red")

filtered_mtcars %>% 
  ggplot(aes(x = hp, y = mpg)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_hline(yintercept = mean_intercept, colour = "red", size = 1)

# Do the two models differ from each other?
anova(model, model_null)

# Answer yes the models differ - the one with the lower RSS (residual sum of
# squares) is the better fit (under the OLS approach) 

# Generate descriptives
filtered_mtcars %>%
  group_by(cyl) %>%
  summarise(mean = mean(mpg), sd = sd(mpg))

filtered_mtcars %>%
  group_by(cyl) %>%
  ggplot(aes(x = cyl, y = mpg, group = cyl, fill = cyl)) +
  geom_boxplot() +
  guides(fill = FALSE)

# Use aov_4() from the afex package to build ANOVA model ####
# Need to convert cyl to a factor first
filtered_mtcars$cyl <- as.factor(filtered_mtcars$cyl) 

model_ANOVA <- aov_4(mpg ~ cyl + (1 | rowname), data = filtered_mtcars)
nice(model_ANOVA)
summary(model_ANOVA)

emmeans(model_ANOVA, pairwise ~ cyl)

# Visualise the data
mtcars_filtered %>%
  ggplot(aes(x = cyl, y = mpg, fill = cyl)) +
  geom_violin() +
  stat_summary(fun.data = "mean_cl_boot") +
  labs(x = "Number of Cylinders", y = "MPG", 
       title = "Plot of MPG against Number of Cylinders", 
       subtitle = "mtcars dataset filtered") +
  guides(fill = FALSE)
