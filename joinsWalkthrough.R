library(tidyverse)

####joins gif explanation links https://twitter.com/grrrck/status/1029584526207582208?lang=bn
###student survye with likert items 
studentSurvey <- tibble::tribble(
                   ~studentID, ~q1, ~q2, ~q3, ~q4, ~q5,
                       12345L,  1L,  1L,  2L,  1L,  2L,
                       12346L,  4L,  NA,  4L,  4L,  3L,
                       12348L,  1L,  3L,  1L,  3L,  NA,
                       12349L,  2L,  4L,  4L,  2L,  3L,
                       12351L,  4L,  3L,  2L,  NA,  3L,
                       12352L,  3L,  2L,  4L,  2L,  1L,
                       12353L,  3L,  3L,  2L,  4L,  4L
                   )


#student course and grade information
studentCourse <- tibble::tribble(
                   ~studentInfo, ~courseName, ~grade, ~semester, ~endYear,
                       12345L,   "Biology",    "A",    "Fall",    2021L,
                       12346L,   "Biology",    "B",    "Fall",    2021L,
                       12347L,   "Biology",    "A",    "Fall",    2019L,
                       12348L,   "Biology",    "C",    "Fall",    2020L,
                       12349L,   "Biology",    "C",    "Fall",    2021L,
                       12350L,   "Biology",    "B",  "Spring",    2021L,
                       12351L,   "Biology",    "B",  "Spring",    2021L,
                       12352L,   "Biology",    "A",  "Spring",    2022L,
                       12353L,   "Biology",    "A",  "Spring",    2022L,
                       12347L, "Biology 2",    "B",  "Spring",    2022L,
                       12348L, "Biology 2",    "C",    "Fall",    2023L,
                       12349L, "Biology 2",    "C",    "Fall",    2023L,
                       12350L, "Biology 2",    "C",  "Spring",    2022L,
                       12351L, "Biology 2",    "D",    "Fall",    2023L
                   )

surveyDictionary <- tibble::tribble(
                      ~questionID,                                                            ~question,         ~construct,
                             "q1",                                                     "Biology is fun",        "Engagment",
                             "q2",                                                    "I like homework",        "Engagment",
                             "q3", "I am willing to work hard on problems even when they are difficult",             "Grit",
                             "q4",   "I plan use the information I learned in the course for my career", "Career Readiness",
                             "q5",                                                 "I am a hard worker",             "Grit"
                      )



studentCourse %>% rename(studentID = studentInfo)

#### we have two tables with information 
#but we'd like to be able to easily compare student scores on the survey with how they did in the class
##in other words we'd like to join these two tables together

studentJoin <- left_join(studentCourse, studentSurvey, by = c("studentInfo" = "studentID"))

#I still have the problem that some information about the survey is not included in the Student Join

# in order to get the data in a structure I'd like more, I'll use a pivot to make the responses longer

studentSurveyLong <- studentSurvey %>% 
  pivot_longer(cols = c(q1:q5), names_to = "questionID", values_to = "response")

####join the new long table to the survey dictionary
studentJoinLong <- left_join(studentSurveyLong, surveyDictionary)

###join in the course information
allData <- left_join(studentJoinLong, studentCourse)


###clean up NA's
allDataClean <- allData %>% 
  drop_na(response)
