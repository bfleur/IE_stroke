# IE group project
# matching script (part 2 of code)

getwd()
setwd("/Users/Virág/Documents/CEU/2nd year/2nd trimester/Impact_evaluation/Term pr/IE_stroke")

library(tidyverse)
library(stargazer)
library(ggplot2)
library(cobalt)
library(texreg)

health_cl = read.csv("data/health_clean.csv")

# 1.
# exact matching on gender

health_cl <- health_cl %>%
  na.omit()

#install.packages("MatchIt")
library(MatchIt)
match.1 <- matchit(treatment ~ gender, data = health_cl,
                   method = "exact", replace = FALSE)

#install.packages("cobalt")
library(cobalt)
love.plot(match.1, stars = "std")


health_em <- match.data(match.1)
health_em


#2.
# coarsened matching on gender and age

summary(l1$age)
# between 16 and 55 yrs
hist(l1$age)

summary(l1$educ)
# between 0 and 18 yrs
hist(l1$educ)

install.packages("car")
library(car)
scatterplot(educ ~ age | treat, data = l1,
            xlab="Age", ylab="Education",
            main="Enhanced Scatter Plot")

# for age, larger population at certain age brackets allows us to more easily 
# match subjects, therefore the brackets may be narrower with the same precision
# as for those less narrow but including less observations
# there are more observations in younger age than at the older people

# for education, the most dense category is between 8-12 years (see histogram)

acut <-c(18,20,22,24,28,35,45)
ecut <- c(6,8,10,12,14)
cut=list(education = ecut, age = acut)

summary(l1$married)
summary(l1$race)
# I do not think it would be necessary to coarse these categories further,
# they are quite aggregated anyway
# married is a binary variable, so it would not be feasible 
#(either married or not, both with a significant number of observations)
# race is the same, there is a small chance that treated and untreated subjects 
# from the three race categories could not be matched (perhaps at hispanic as 
# there is a relatively small amount of observations)


library(Matching)
library(rgenoud)


coarsened.l1 <- matchit(treat ~ age + educ + race + married +
                          nodegree + re74 + re75,
                        data = l1, method = "cem",
                        cutpoints = cut)
coarsened.l1

match_coarsened <- match.data(coarsened.l1)

# 3.
# let's see the features of the matched data
# and the estimated effects

