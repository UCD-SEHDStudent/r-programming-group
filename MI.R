# From: https://www.jstatsoft.org/article/view/v045i03
# https://datascienceplus.com/imputing-missing-data-with-r-mice-package/

library(mice)
library(VIM)

#Load in the airquality dataset, available with r.
data <- airquality

# Now we will remove some datapoints. 
data[4:10,3] <- rep(NA,7)
data[1:5,4] <- NA

# Remove categorical variables (5 and 6)
data <- data[-c(5,6)]

# Let's view the summary for data. See the number of NA data points?
summary(data)

# Which column has the most missing data?

### Classification of missing data
# MCAR - missing completely at random; MNAR - not missing at random

# Lets check how much data is missing. We need less than 5%.
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(data,2,pMiss)
apply(data,1,pMiss)

# From the readout, we see that ozone is missing almost 25%, and
# the others are under this. Maybe remove ozone. 

### Using the mice package. 

md.pattern(data) # helps us get an understanding of the pattern of missing data.

# The output tells us that 104 samples are complete, 34 samples miss only the 
# Ozone measurement, 4 samples miss only the Solar.R value and so on.

# Lets try the VIM package for visuals.
library(VIM)
aggr_plot <- aggr(data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

# Another plot
marginplot(data[c(1,2)])

# If data is MCAR, the red and blue should be similar.

# mice() function handles the imputing process
tempData <- mice(data,m=5,maxit=50,meth='pmm',seed=500)

# Now summarize to view:
summary(tempData)

# Notes: m=5 refers to the number of imputed datasets. 5 is default
# meth = 'pmm' refers to the imputation method. methods(mice) will list these.

methods(mice)

# Lets check the imputed data, specifically ozone. 

tempData$imp$Ozone

# The output shows the imputed data for each observation (first column left) 
# within each imputed dataset (first row at the top).
# If you need to check the imputation method used for each variable: 

tempData$meth

# Now get back the completed dataset.
completedData <- complete(tempData,1)

### Inspecting the distribution of the original and imputed data:

xyplot(tempData,Ozone ~ Wind+Temp+Solar.R,pch=18,cex=1)

# Matching blue and pink dots tell us that the imputed values are "plausible values"

# The density of the imputed data for each imputed dataset is showed in magenta 
# while the density of the observed data is showed in blue.

densityplot(tempData)

# stripplot() shows the distributions of the variables as individual points
stripplot(tempData, pch = 20, cex = 1.2)


### Pooling

# Suppose that the next step in our analysis is to fit a linear model to the data. 
# You may ask what imputed dataset to choose. The mice package makes it again 
# very easy to fit a a model to each of the imputed dataset and then pool the results together

modelFit1 <- with(tempData,lm(Temp~ Ozone+Solar.R+Wind))
summary(pool(modelFit1))

# modelfit1 contains the results of the fitting performed over the imputed datasets.
# pool() function pools them all together.

# In addition to the normal lm() columns, model: fmi contains the fraction of 
# missing info while lambda is the proportion of total variance that is attributable
# to the missing data. 

# Remember - we initialized the mice function with a specific seed (5). 
# To reduce this effect, we can impute a higher number of dataset, by changing 
# the default m=5 in the mice() function to:

tempData2 <- mice(data,m=50,seed=245435) # 50 
modelFit2 <- with(tempData2,lm(Temp~ Ozone+Solar.R+Wind))
summary(pool(modelFit2))
