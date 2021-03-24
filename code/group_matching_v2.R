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
match.1 <- matchit(treatment ~ gender,
                   data = health_cl,
                   method = "exact", replace = FALSE)

#install.packages("cobalt")
library(cobalt)
love.plot(match.1, stars = "std")


health_em <- match.data(match.1)
health_em


#2.
# coarsened matching on gender and age

ggplot(health_cl, aes(x=age)) + 
  geom_histogram(color = "grey69",fill = "slategray2", bins = 15) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"))
median(age)
mean(age)

health_cl$treatfactor<- as.factor(health_cl$treatment)
sumtable(health_cl, group="treatfactor", group.test=TRUE)



#install.packages("car")
library(car)
scatterplot(stroke ~ gender | treatment, data = health_cl,
            xlab="Age", ylab="stroke",
            main="Enhanced Scatter Plot")


library(Matching)
library(rgenoud)

agecut <-c(0, 17,25,35,50)
cut=list( age = agecut)


coarsened_match <- matchit(treatment~age, data = health_cl, method = "cem",
                           cutpoints=cut)
coarsened_match
match_dat_c1 <- match.data(coarsened_match)



coarsened_match_2 <- matchit(treatment~age+gender+ever_married, data = health_cl, method = "cem",
                             cutpoints=cut)
coarsened_match_2

love.plot(coarsened_match_2, stars= "std")

match_dat_c2 <- match.data(coarsened_match_2)


coarsened_match_full <- matchit(treatment~age+gender+ever_married+hypertension+heart_disease+avg_glucose_level,
                                data = health_cl, method = "cem",
                                cutpoints=cut)
coarsened_match_full

love.plot(coarsened_match_full, stars= "std")

match_dat_cfull <- match.data(coarsened_match_full)


# 3.
# let's see the features of the matched data
# and the estimated effects


# regressions with data exact matching, coarsened 1-2-3
# comparisons with unmatched data

reg1_raw <- lm(stroke  ~ treatment, data = health_cl)
reg2_raw <- lm(stroke  ~ treatment + gender + age + hypertension + heart_disease + ever_married + avg_glucose_level,
               data = health_cl)


reg1_ematched <- lm(stroke  ~ treatment, data = health_em)
reg2_ematched <- lm(stroke  ~ treatment + gender + age + hypertension + heart_disease + ever_married + avg_glucose_level,
                    data = health_em)

#reg_coars1 <- lm(stroke  ~ treatment, data = health_em)

#reg_coars2 <- lm(re78  ~ treat + age + educ + race + married +
#                          nodegree + re74 + re75, data = l1)

reg1_coars_full <- lm(stroke  ~ treatment, data = match_dat_cfull)
reg2_coars_full <- lm(stroke  ~ treatment + gender + age + hypertension + heart_disease + ever_married + avg_glucose_level,
                      data = match_dat_cfull)



#screenreg(list(mod_la1_matched, mod_la2_matched, mod_la2_unmatched, mod_la2_unmatched))

library(stargazer)
stargazer(reg1_raw, reg2_raw, reg1_ematched, reg2_ematched, reg1_coars_full, reg2_coars_full,
          title="Regression on exact and coarsened matched and unmatched data", type = "text", 
          out='tab2.txt')





