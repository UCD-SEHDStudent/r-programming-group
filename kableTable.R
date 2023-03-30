# Load libraries
library(knitr)
library(kableExtra)
library(tidyverse)

####Kable takes dataframes from R and turns them into html and pdf formatting
####This is important since it allows for you to put them in r markdowns, documents, ppts or pdfs

####For more information and customization use this website: "https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html"
####Lots of this training is from the KableExtra vignette linked above


##we will be using a built in dataset. This dataset loads in when you call library on tidyverse

?mtcars
### take the first five rows and six columns from the mt cars table since we don't want a huge table
dt <- mtcars[1:5, 1:6]

###if you want to see the most basic table with no styling
dt %>%
  kbl() 

###More of an apa style table
dt %>%
  kbl(caption = "Recreating booktabs style table") %>%
  kable_classic(full_width = F, html_font = "Cambria")

###Other options for apa style
dt %>%
  kbl() %>%
  kable_classic_2(full_width = F)


##add in grouped headings
dt %>%
  kbl(caption = "Table 1: Grouped Headings") %>%
  kable_classic_2(full_width = F) %>%
  add_header_above(c(" " = 1, "Group 1" = 2, "Group 2" = 2, "Group 3" = 2))


##A little bit of html to add bold <b> and italics <i>
dt %>%
  kbl(caption = "<b>Table 1: </b> <i>Grouped Headings</i>") %>%
  kable_classic_2(full_width = F) %>%
  add_header_above(c(" " = 1, "Group 1" = 2, "Group 2" = 2, "Group 3" = 2))


####If the titles need to be changed it is easiest to do directly in your dataframe

names <- c("Miles Per Gallon", "Displacement", "Gross Horspower", "Weight (1000 lbs)", "Rear Axel Ratio", "1/4 Mile Time")
names(dt) <- names
dt %>%
  kbl(caption = "<b>Table 1: </b> <i>Grouped Headings</i>") %>%
  kable_classic_2(full_width = F) %>%
  add_header_above(c(" " = 1, "Group 1" = 2, "Group 2" = 2, "Group 3" = 2))


####custom conditional formatting.
mtcars[1:8, 1:8] %>%
  kbl() %>%
  kable_paper(full_width = F) %>%
  column_spec(4, background = spec_color(mtcars$disp[1:8], begin=0, end=1, na_color = "white", scale_from = c(0, 360)),
              color = "white") 


####Create a correlation matrix and display as a table

dtCorTable <- cor(mtcars[1:6, 1:6], method = c("pearson"))

dtCorTable <- as.data.frame(dtCorTable)

names(dtCorTable) <- names
rownames(dtCorTable) <- names


dtCorTable %>% 
  kbl(caption = "<b>Table 1: </b> <i>Grouped Headings</i>") %>%
  kable_classic_2(full_width = F) %>%
  add_header_above(c(" " = 1, "Group 1" = 2, "Group 2" = 2, "Group 3" = 2))

###Bit hard to read so let's round:
dtCorTable %>% 
  kbl(caption = "<b>Table 1: </b> <i>Correlation Table</i>", digits = 2) %>%
  kable_classic_2(full_width = F) 



###Bonus if we have time. Using a function I found online here: https://stefaneng.github.io/apa_correlation_table/

library(Hmisc)
apaCorr <- function(mat, corrtype = "pearson") {
  matCorr <- mat
  if (!str_detect(class(matCorr)[1], "rcorr")) {
    matCorr <- rcorr(mat, type = corrtype)
  }
  
  # Add one star for each p < 0.05, 0.01, 0.001
  stars <- apply_if(round(matCorr$r, 2), matCorr$P < 0.05, function(x) paste0(x, "*"))
  stars <- apply_if(stars, matCorr$P < 0.01, function(x) paste0(x, "*"))
  stars <- apply_if(stars, matCorr$P < 0.001, function(x) paste0(x, "*"))
  # Put - on diagonal and blank on upper diagonal
  stars[upper.tri(stars, diag = T)] <- "-"
  stars[upper.tri(stars, diag = F)] <- ""
  n <- length(stars[1,])
  colnames(stars) <- 1:n
  # Remove _ and convert to title case
  row.names(stars) <- tools::toTitleCase(sapply(row.names(stars), gsub, pattern="_", replacement = " "))
  # Add index number to row names
  row.names(stars) <- paste(paste0(1:n,"."), row.names(stars))
  stars
}
apply_if <- function(mat, p, f) {
  # Fill NA with FALSE
  p[is.na(p)] <- FALSE
  mat[p] <- f(mat[p])
  mat
}

irisStars <- apaCorr(as.matrix(mtcars), corrtype = "pearson")

irisStars %>% 
  kbl(caption = "<b>Table 1: </b> <i>Correlation Table</i>", digits = 2) %>%
  kable_classic_2(full_width = F) 





