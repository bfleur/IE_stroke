getwd()
setwd("/Users/EffieFiest/Documents/CEU/ImpactEval/groupassignment")

install.packages("expss")
install.packages("remotes")
remotes::install_github("R-CoderDotCom/ggcats@main")
library(ggcats)
library(expss)
library(ggcats)
library(labelled)
library(tidyverse)
library(stargazer)
library(ggplot2)
library(dplyr)
library(kableExtra)
library(vtable)
library(tidyverse)
library(cobalt)


health = read.csv("healthcare-dataset-stroke-data.csv")

# Inspect dataset # 

glimpse(health)

# 5110 observations; 12 variables # 

# id: unique identifier
# gender: "Male", "Female" or "Other"
# age: age of the patient
# hypertension: 0 if the patient doesn't have hypertension, 1 if the patient has hypertension
# heart_disease: 0 if the patient doesn't have any heart diseases, 1 if the patient has a heart disease
# ever_married: "No" or "Yes"
# work_type: "children", "Govt_jov", "Never_worked", "Private" or "Self-employed"
# Residence_type: "Rural" or "Urban"
# avg_glucose_level: average glucose level in blood
# bmi: body mass index
# smoking_status: "formerly smoked", "never smoked", "smokes" or "Unknown"*
# stroke: 1 if the patient had a stroke or 0 if not
# Note: "Unknown" in smoking_status means that the information is unavailable for this patient

summary(health)
attach(health)

### Transform gender into numerical dummy 
summary(health$gender)
table(health$gender, useNA = "always")

# there is one "other" coded; The 1 "other" observation will be coded as 1 (hence female) 
# we should recode it as missing (NA)
health$gender = na_if(gender, "Other")
table(health$gender, useNA = "always")

# No "Other" character left but one missing value, which was previously "NA" #
# Now convert rest into dummy (1 for female and 0 for male)

health <- health %>%
  mutate(gender = ifelse(health$gender == "Male",0,1))
table(health$gender, useNA = "always")

# Gender variable is now a dummy with 0 for male and 1 for female 


### Transform ever_married into numerical dummy 
table(health$ever_married, useNA = "always")
# No missing values
# Recode to dummy variable with 0 for never married and 1 for married
health <- health %>%
  mutate(ever_married = ifelse(ever_married == "Yes",1,0))
table(health$ever_married, useNA = "always")

### Transform residence type into numerical
table(Residence_type, useNA = "always")
# Recode to dummy variable with 0 for Rural and 1 for Urban
health <- health %>%
  mutate(Residence_type = ifelse(Residence_type == "Urban",1,0))
table(Residence_type, useNA = "always")

### Check variable BMI
table(bmi, useNA = "always")
# we have 201 "N/A" observations. let's recode them as really na's by adding new variable 
# "bmi_num. I also want to keep the original "bmi" variable. 
table(bmi, useNA = "always")
glimpse(bmi)


health$bmi = na_if(bmi, "N/A")
health$bmi_numeric = as.numeric(bmi)
table(health$bmi_numeric, useNA ="always")
# Now we see 201 na's in the numerical bmi variable


### Let's create an appropriate treatment variable using the "smoking_status" variable
attach(health)
table(smoking_status, useNA = "always")
# okay, we have 1544 "unknown" answers for this variable, we would have to recode them as "NA"
# as otherwise it makes little sense, hence theser observations now being recoded as NA
# will not appear as "N" in the regressions.
health$smoking_status = na_if(health$smoking_status, "Unknown")
table(health$smoking_status, useNA = "always")
# Now we are left with "formerly smoked", "never smoked" and "smokes".
# Treatment (=1) will be if the person either smokes or formerly smoked and 
# not "experience" treatment if the person never smoked. 

health <- health %>%
  mutate(smoking_status = ifelse(smoking_status == "never smoked",0,1))
table(health$smoking_status, useNA = "always")

# We are left with 1544 NA values for the treatment variable, 
# 1892 people never smoked, thus are not captured as treated and 1674 people 
# either formerly smoked or are still smoking. 
# in the last step let's rename the variable
colnames(health)

health =
  health %>% 
  rename(treatment = smoking_status)
colnames(health)
health =
  health %>% 
  rename(bmi_character = bmi)
health =
  health %>% 
  rename(bmi = bmi_numeric)


# Great, BMI and treatment are now adequately renamed. 

### Just checking again for any not yet recoded NA's
table(age, useNA = "always")
table(gender, useNA = "always")
table(hypertension, useNA = "always")
table(heart_disease, useNA = "always")
table(ever_married, useNA = "always")
table(work_type, useNA = "always")
table(Residence_type, useNA = "always")
table(avg_glucose_level, useNA = "always")
summary(avg_glucose_level)
table(bmi, useNA ="always")
table(health$treatment, useNA ="always")

sapply(health, function(x) sum(is.na(x)))

# in total we have 1 NA in gender, 201 NA's for the BMI variable and 1544 NA's for the treatment variable '

### Let's check for duplicates
sum(duplicated(health))
# yay, no perfect duplicates 

### Let's move on to some visualization 
# What's the general age distribution?

ggplot(health, aes(x=age)) + 
  geom_histogram(color = "grey69",fill = "slategray2", bins = 15) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"))
median(age)
mean(age)
# Fairly normally distributed age distriubtion with slight tilt, 
# the median age is 45 and the mean age is 43.23 (thus, fairly normally distributed)

ggplot(health, aes(x=avg_glucose_level)) + 
  geom_histogram(color = "grey69",fill = "slategray2", bins = 15) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"))
# Glucose is not normally distributed. 
# Lets check if people suffering from a stroke graphically also have a higher glucose level.

ggplot(health,aes(y = avg_glucose_level, x = factor(stroke))) +
  geom_boxplot(colour="lightpink3")  + coord_flip() + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"))
# On average, people suffering from a stroke indeed have a much higher glucose level 
# but there are quite some outliers for people not experiencing a stroke yet also 
# having high glucose levels. 
# Let's check with numbers
health %>%
  filter(stroke == 1) %>%
  summarize(mean_avg_glucose_1 = mean(avg_glucose_level))

health %>%
  filter(stroke == 0) %>%
  summarize(mean_avg_glucose_0 = mean(avg_glucose_level))

# Indeed, people who have a stroke have an avg glucose level of 132
# whereas people with no stroke have an avg glucose level of 104. 


# What about smoking? Graphically, do people who smoke experience more strokes? 
ggplot(health,aes(y = factor(treatment), x = factor(stroke))) +
  geom_jitter(colour="lightpink3")  + coord_flip() + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"))
# The graphical correlation now is not so super clear but indeed, the jitter for people
# smoking and having a strong is "more dense" than people not smoking and having a stroke. 


# What about BMI? 

ggplot(health,aes(y = bmi, x = factor(stroke))) +
  geom_boxplot(colour="lightpink3")  + coord_flip() + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"))
#similar to avg. glucose level, people suffering from a stroke indeed have a slightly 
#higher BMI but it is not such a big difference. 
mean(health[health$stroke>0,"bmi"], na.rm = TRUE)
mean(health[health$stroke==0,"bmi"], na.rm = TRUE)

# Indeed, people not recording a stroke have a mean BMI of 28.82 whereas
# people reporting a stroke have a mean BMI of 30.47, thus people
# experiencing a stroke on average have more weight. 

# What about the distribution of gender and having a stroke?

ggplot(health,aes(y = factor(gender), x = factor(stroke))) +
  geom_jitter(colour="lightpink3")  + coord_flip() + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"))
# from a pure graphical look, no clear correlation

corr1 = lm(stroke ~ gender, data = health)
summary(corr1)

# Checking with a simple regression, we also do not see a difference in male or females
# experiencing a stroke (not stat. significant at all)

### Let's check for balance ###

health$treatfactor<- as.factor(health$treatment)
sumtable(health, group="treatfactor", group.test=TRUE)

# Well....looking at the balancing table, 
# the variables "gender", "age", "heart_disease", 
# "ever_married", "residence_type", and "BMI" are statistically
# insignificant on the conditional variable "treatment" which is 
# whether a person smoked/smokes or not. 
# Thus, matching should be really done. 

# Balance can also be checked with an OLS model "manually"

balance = lm(treatment ~ . - bmi_character - treatfactor -id, data = health)
summary(balance)

# Matching with the balancing table results from above we see that the variables
# "gender", "age", "heart disease" are statistically highly significant and "ever_married", 
# "work_type", "residence_type" are stat. significant on a 10% sig. basis.
# Thus, Gender and Age seem to be spec. unbalanced. and we should match those.

reg_raw = lm(stroke ~ treatment + gender + age + ever_married + work_type + Residence_type, data = health)
summary(reg_raw)

write.csv(health,"health_clean.csv", row.names = FALSE)


  


