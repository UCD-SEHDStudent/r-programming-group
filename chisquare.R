

#### Chi-Square Goodness of Fit ####

#import dataset
ch19ds1 <- read.csv(file.choose())
View(ch19ds1)

pedata <- read.csv(file.choose())
View(pedata)

#create a table of observed counts.

gfdata <- summary(ch19ds1$Voucher)

gfpe <- summary(pedata$firstTimeDonor)

# View the data

gfdata

gfpe 

# Our variables are "character" instead of factor. Let's fix that.

ch19ds1$Voucher <- as.factor(ch19ds1$Voucher)

pedata$firstTimeDonor <- as.factor(pedata$firstTimeDonor)

gfdata <- summary(ch19ds1$Voucher)

gfpe <- summary(pedata$firstTimeDonor)

# View the data again. 

gfdata

gfpe

# Run the goodness of fit test

chisq.test(gfdata, p = c(1/3, 1/3, 1/3))

chisq.test(gfpe, p =c(1/2, 1/2))

#### Chi-Square Test of Independence ####

# import the data, then view it.
ch19ds2 <- read.csv(file.choose())

View(ch19ds2)

# convert the variables to factors

ch19ds2$Vote <- as.factor(ch19ds2$Vote)
ch19ds2$Sex <- as.factor(ch19ds2$Sex)

# Now, create a table and view it

toidata <- table(ch19ds2$Sex, ch19ds2$Vote)
toidata

# Run the Chi-Square Test of Independence
# We would use correct = TRUE if one of the cells from the table
# was < 5. In our case, none are smaller than 5, so we use "false".

chisq.test(toidata, p = c(1/4, 1/4, 1/4, 1/4), correct = FALSE)


