library(tidyverse)
library(broom)
library(afex)
library(emmeans)

set.seed(1234)
DV <- c(round(rnorm(10000, 1000, 20)), round(rnorm(10000, 1010, 20)))
condition <- c(rep("Condition_A", 10000), rep("Condition_B", 10000))

my_data <- tibble(condition, DV)

my_data %>%
  ggplot(aes(x = DV, fill = condition, alpha = .5)) +
  geom_histogram(data = filter(my_data, condition == "Condition_A"), bins = 50) +
  geom_histogram(data = filter(my_data, condition == "Condition_B"), bins = 50) +
  guides(fill = FALSE, alpha = FALSE)
  
set.seed(1234)
DV <- c(round(rnorm(50, 1000, 20)), round(rnorm(50, 1010, 20)))
condition <- c(rep("Condition_A", 50), rep("Condition_B", 50))

my_data <- tibble(condition, DV)

my_data %>%
  ggplot(aes(x = DV, fill = condition)) +
  geom_histogram(data = filter(my_data, condition == "Condition_A"), bins = 10) +
  geom_histogram(data = filter(my_data, condition == "Condition_B"), bins = 10) +
  guides(fill = FALSE, alpha = FALSE)

my_data %>%
  group_by(condition) %>%
  summarise(mean_dv = mean(DV), sd_dv = sd(DV))

t.test(my_data[condition == "Condition_A",]$DV, my_data[condition == "Condition_B",]$DV)

all_data <- NULL

set.seed(1234)
for (i in 1:1000) {
  DV <- c(round(rnorm(50, 1000, 20)), round(rnorm(50, 1010, 20)))
  condition <- c(rep("Condition_A", 50), rep("Condition_B", 50))
  my_data <- tibble(condition, DV, i)
  all_data <- rbind(all_data, my_data)
}

all_results <- NULL
for (n in 1:1000) {
  my_data_filt <- all_data %>% filter(i == n)
  result <- tidy(t.test(my_data_filt[condition == "Condition_A",]$DV, 
                        my_data_filt[condition == "Condition_B",]$DV))
  all_results <- rbind(result, all_results)
  
}

count(all_results %>% filter(p.value < .05))

# use the built in mtcars data set
model <- lm(mpg ~ hp, data = mtcars)
summary(model)

mtcars %>% 
  ggplot(aes(x = hp, y = mpg)) + 
  geom_point() +
  geom_smooth(method = "lm")

model_null <- lm(mpg ~ 1, data = mtcars)

mean_intercept <- mean(mtcars$mpg)

mtcars %>% 
  ggplot(aes(x = hp, y = mpg)) + 
  geom_point() +
  geom_hline(yintercept = mean_intercept, colour = "red")

mtcars %>% 
  ggplot(aes(x = hp, y = mpg)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_hline(yintercept = mean_intercept, colour = "red", size = 1)

anova(model, model_null)

mtcars %>%
  group_by(cyl) %>%
  summarise(mean = mean(mpg), sd = sd(mpg))

mtcars %>%
  group_by(cyl) %>%
  ggplot(aes(x = cyl, y = mpg, fill = cyl)) +
  geom_boxplot() +
  guides(fill = FALSE)

mtcars$cyl <- as.factor(mtcars$cyl) 
mtcars <- mtcars %>% mutate(id = row_number())

model_ANOVA <- aov_4(mpg ~ cyl + (1 | id), data = mtcars)
nice(model_ANOVA)
summary(model_ANOVA)

emmeans(model_ANOVA, pairwise ~ cyl)
