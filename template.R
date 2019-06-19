# Code along template

# Main task 1 - Simulate data and run t-tests ####
# Sample A from population with mean = 1000, sd = 20
# Sample B from population with mean = 1010, sd = 20
# This equates to a Cohen's d of .5 as (1010-1000)/20 so a medium effect size

# First let's plot an approximation of the population for each sample 

# Now let's sample 50 individuals from each population

# Plot data

# Run a t-test to see if difference Sample A vs Sample B is significant ####

# What if we ran 1,000 simulations? ####
# How many of those detect a signficant difference?

# Main task 2 - Linear model with the built-in mtcars dataset ####
# is mpg predicted by hp?

# Test linear model against the mean as a model

# Factorial ANOVA ####
# Set cyl to factor and add ID to determine whether difference in mpg
# differs significantly by cylinder number.

# Plot data

# Use aov_4() from tehthe afex package to build ANOVA model

# Run pairwise comparisons to tell where the difference lies
