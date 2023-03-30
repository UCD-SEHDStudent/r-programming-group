library(car)
library(tidyverse)
library(psych)

#### t- test ####

# Import data, then view it.
intervention <- read.csv(file.choose())
View(intervention)

# I do not like the variable name for group. It has extra characters. Lets fix that. 

intervention <- rename(intervention, group = ï..Group)

# I used the rename function to change the "ï..Group" variable name to "group". 

# First - check for the homogenity of variance using Levene's test #

leveneTest(Score ~as.factor(group), data = intervention)

# Awesome - .066 for Levene's. We can assume variances are equal. 

# Run a t-test 

t.test(Score ~ group, data = intervention, var.equal = TRUE)

# OK - but what if the data wasn't 

t.test(Score ~ group, data = intervention, var.equal = FALSE)

# This changes it to Welch's Two Sample t-test. Our DF and p values have changed. 


# Lets get a list of all the arguments and values for t.test 

?t.test

#### Examine the effect size with Cohen's d ####

# Option 1  (the long way)
# Create groups for experimental and control - this uses the pipes operator. 

experimental <- intervention %>% 
  filter(group == "Experimental")

control <- intervention %>% 
  filter(group == "Control")

# Now describe these to get the SD, Mean, N

describe(experimental)
describe(control)

# Option 2 - the easy way

cohen.d(intervention ~ group)

#### END ####
