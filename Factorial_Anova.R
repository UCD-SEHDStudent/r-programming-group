library(psych)
library(car)

# Import dataset
ch16ds1 <- read.csv(file.choose())


View(ch16ds1)

# Run Descriptive Stats using the psych package
describeBy(ch16ds1$Loss, group = list(ch16ds1$Treatment, ch16ds1$Gender))


# Now we can run the Anova
## Terms described
# m1: the name of the R object that is storing our results
# aov: the function to run ANOVA
# Loss: our dependent variable
# ~: in shorthand, it means explained by or equal to
# Treatment + Gender + Treatment * Gender: we are telling AOV that the independent
# variables are Treatment, Gender, and the interaction between Treatment and Gender
# that we get by multiplying * Treatment by Gender
# data = ch16ds1: use this data object

m1 <- aov(Loss ~ Treatment + Gender + 
            Treatment*Gender, data = ch16ds1)

# Use the function anova()
anova(m1)

# Check assumptions
leveneTest(Loss~ Treatment*Gender, data = ch16ds1)

# Create an interaction plot

interaction.plot(x.factor = ch16ds1$Treatment, 
                 trace.factor = ch16ds1$Gender,
                 response = ch16ds1$Loss)

# Better plot

# fixed = TRUE: list the factors for gender in alpha order
# leg.bty = "b": add a box around the legend
# xlab/ylab: labels for the x and y axis
# trace.label = "Gender": the label for the legend
# ylim = c(60, 85)): range for the y-axis 60 to 85


interaction.plot(x.factor = ch16ds1$Treatment,
                 trace.factor = ch16ds1$Gender,
                 response = ch16ds1$Loss,
                 fixed = TRUE,
                 leg.bty = "b",
                 xlab = "Treatment",
                 ylab = "Weight Loss",
                 trace.label = "Gender",
                 ylim = c(60,85))


# Use the TukeyHSD function to compare groups

TukeyHSD(m1)

# Computing the effect size for factorial anova

# Omega-squared
Omegas <- function(mod){
  aovMod <- mod
  if(!any(class(aovMod) %in% 'aov')) aovMod <- aov(mod)
  sumAov     <- summary(aovMod)[[1]]
  residRow   <- nrow(sumAov)
  msError    <- sumAov[residRow,3]
  dfEffects  <- sumAov[1:{residRow-1},1]
  ssEffects  <- sumAov[1:{residRow-1},2]
  msEffects  <- sumAov[1:{residRow-1},3]
  ssTotal    <- rep(sum(sumAov[1:residRow, 2]), 3)
  Omegas <- abs((ssEffects - dfEffects*msError)/(ssTotal + msError))
  names(Omegas) <- rownames(sumAov)[1:{residRow-1}]
  Omegas
}



# Partial omega-squared
partialOmegas <- function(mod){
  aovMod <- mod
  if(!any(class(aovMod) %in% 'aov')) aovMod <- aov(mod)
  sumAov     <- summary(aovMod)[[1]]
  residRow   <- nrow(sumAov)
  dfError    <- sumAov[residRow,1]
  msError    <- sumAov[residRow,3]
  nTotal     <- nrow(model.frame(aovMod))
  dfEffects  <- sumAov[1:{residRow-1},1]
  ssEffects  <- sumAov[1:{residRow-1},2]
  msEffects  <- sumAov[1:{residRow-1},3]
  partOmegas <- abs((dfEffects*(msEffects-msError)) /
                      (ssEffects + (nTotal -dfEffects)*msError))
  names(partOmegas) <- rownames(sumAov)[1:{residRow-1}]
  partOmegas
}


# Eta-squared
require(lsr)
etaSquared(m1)

# Omega-squared
Omegas(m1)


#### Curriculum Data  ####

# Import dataset
curriculum <- read.csv(file.choose())


View(curriculum)


# Run Descriptive Stats using the psych package
describeBy(curriculum$score, group = list(curriculum$curriculum, curriculum$gender))


# Now we can run the Anova

## Terms described
# curriculumAOV: the name of the R object that is storing our results
# aov: the function to run ANOVA
# score: our dependent variable
# ~: in shorthand, it means explained by or equal to
# curriculum + gender + curriculum*gender we are telling AOV that the independent
# variables are curriculum and gender and the interaction between curriculum and gender
# that we get by multiplying * curriculum by gender
# data = curriculum use this data object

curriculumAOV <- aov(score ~ curriculum + gender + 
            curriculum*gender, data = curriculum)

# Use the function anova()
anova(curriculumAOV)

# Check assumption
leveneTest(score~ curriculum*gender, data = curriculum)

# Create an interaction plot

interaction.plot(x.factor = curriculum$curriculum, 
                 trace.factor = curriculum$gender,
                 response = curriculum$score)


# Use the TukeyHSD function to compare groups

TukeyHSD(curriculumAOV)

# Computing the effect size for factorial anova

# Omega-squared
Omegas <- function(mod){
  aovMod <- mod
  if(!any(class(aovMod) %in% 'aov')) aovMod <- aov(mod)
  sumAov     <- summary(aovMod)[[1]]
  residRow   <- nrow(sumAov)
  msError    <- sumAov[residRow,3]
  dfEffects  <- sumAov[1:{residRow-1},1]
  ssEffects  <- sumAov[1:{residRow-1},2]
  msEffects  <- sumAov[1:{residRow-1},3]
  ssTotal    <- rep(sum(sumAov[1:residRow, 2]), 3)
  Omegas <- abs((ssEffects - dfEffects*msError)/(ssTotal + msError))
  names(Omegas) <- rownames(sumAov)[1:{residRow-1}]
  Omegas
}



# Partial omega-squared
partialOmegas <- function(mod){
  aovMod <- mod
  if(!any(class(aovMod) %in% 'aov')) aovMod <- aov(mod)
  sumAov     <- summary(aovMod)[[1]]
  residRow   <- nrow(sumAov)
  dfError    <- sumAov[residRow,1]
  msError    <- sumAov[residRow,3]
  nTotal     <- nrow(model.frame(aovMod))
  dfEffects  <- sumAov[1:{residRow-1},1]
  ssEffects  <- sumAov[1:{residRow-1},2]
  msEffects  <- sumAov[1:{residRow-1},3]
  partOmegas <- abs((dfEffects*(msEffects-msError)) /
                      (ssEffects + (nTotal -dfEffects)*msError))
  names(partOmegas) <- rownames(sumAov)[1:{residRow-1}]
  partOmegas
}


# Eta-squared
require(lsr)
etaSquared(curriculumAOV)

# Omega-squared
Omegas(curriculumAOV)


