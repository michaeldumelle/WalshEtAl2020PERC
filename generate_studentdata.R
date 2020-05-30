#creating a toy data set due to confidentiality concerns
library(tidyverse)
library(stringr)

## number of students
no_students <- 300
studentID <- str_c("Student", 1:no_students, sep = "_")
terms <- as.character(1:3)

student_data <- expand_grid(studentID, terms) %>%
  # this is 0.95 because we subtracted out the possible percentage of 
  # final grade attributable from correct homework problems
  mutate(final_grade_per = runif(n = nrow(.), 0.4, 0.95), 
         quartiles_finished_per = runif(n = nrow(.), 0, 1),
         problems_tried_per = runif(n = nrow(.), 0, 1), 
         problems_correct_per = runif(n = nrow(.), 0, 1))


#writing to a csv
write_csv(student_data, "studentdata.csv")
