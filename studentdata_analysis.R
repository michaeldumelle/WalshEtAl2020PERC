# data analysis
library(tidyverse)
library(nlme)

# reading in data
student_data <- read_csv("studentdata.csv")

# fitting the model
model <- lme(final_grade_per ~ as.factor(terms) + quartiles_finished_per + problems_tried_per + 
               problems_correct_per, random = ~ 1 | studentID, data = student_data)
summary(model)

# checking residuals
X <- unname(model.matrix(~ as.factor(terms) + quartiles_finished_per + problems_tried_per + 
                           problems_correct_per, data = student_data))

Z <- unname(model.matrix(~ as.factor(student_data$studentID) - 1))
            
beta <- summary(model)$coefficients$fixed


resids0 <- student_data$final_grade_per - X %*% beta
all.equal(unname(as.vector(resids0)), unname(as.vector(residuals(model, level = 0))))
raneffs <- ranef(model)$`(Intercept)`
resids1 <- student_data$final_grade_per - X %*% beta - Z %*% raneffs 
all.equal(unname(as.vector(resids1)), unname(as.vector(residuals(model, level = 1))))

## look at blup epsilon
resids1_blup <- (as.vector(residuals(model, level = 1, type = "normalized")))
ggplot(data.frame(x = X %*% beta, y = resids1_blup), mapping = aes(x = x, y = y)) + 
  geom_point() + 
  geom_hline(yintercept = 0)
qqnorm(resids1_blup)
qqline(resids1_blup)

## look at blup random effect - should be normalized for rigor
ggplot(data.frame(obs = 1:300, y = raneffs), mapping = aes(x = obs, y = raneffs)) + 
  geom_point() + geom_hline(yintercept = 0)
qqnorm(raneffs)
qqline(raneffs)

# checking condition numbers
evals <- eigen(t(X) %*% X)
cond_number <- sqrt(max(evals$values) / min(evals$values))
# over 30 indicates some multicollinearity
cond_number
