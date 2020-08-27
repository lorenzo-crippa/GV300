###################################################################################################
# Project:                 GV300 - LABa02 sessions                                                #
#                                                                                                 #
# University:              University of Essex                                                    #
#                                                                                                 #
# Programmer:              Lorenzo Crippa                                                         #
#                                                                                                 #
# Week:                    Week 16 (Monday 13th of January, 2020)                                 #
###################################################################################################

# clear
rm(list=ls())

# Lorenzo Essex
setwd("C:/Users/lc19059/Dropbox/Shared_Essex/GTA/GV300")

# Lorenzo Macbook
#setwd("/Users/Lorenzo/Dropbox/Shared_Essex/GTA/GV300")

#################
library(ggplot2)
library(psych)
library(stargazer)
library(foreign)
#################

##############
# question 4 #
##############

# (a)
RootCause <- rnorm(1000, mean = 0, sd = 1)
OtherThing <- rnorm(1000, mean = 0, sd = 1)
errors <- rnorm(1000, mean = 0, sd = 1)

Outcome <- 1 + RootCause + 3*OtherThing + errors

# ii
model.1 <- lm(Outcome ~ RootCause)

# iii
model.2 <- lm(Outcome ~ RootCause + OtherThing)

# results
stargazer(model.1, model.2, type = "text")
stargazer(model.1, model.2, type = "latex") # for LaTeX users

# (b)
rm(list = ls())

RootCause <- rnorm(1000, mean = 0, sd = 1)
OtherThing <- 2*RootCause + rnorm(1000, mean = 0, sd = 1)
errors <- rnorm(1000, mean = 0, sd = 1)

Outcome <- 1 + RootCause + 3*OtherThing + errors

# ii
model.1 <- lm(Outcome ~ RootCause)

# iii
model.2 <- lm(Outcome ~ RootCause + OtherThing)

# results
stargazer(model.1, model.2, type = "text")
stargazer(model.1, model.2, type = "latex") # for LaTeX users

##############
# question 5 #
##############

rm(list = ls())

data <- read.dta("data/gb_recoded.dta")

# (a)

# summary of many variables together
describe(data.frame(data$e5, data$age, data$turnout05))

# for slides (no skewness, kurtosis, etc...):
describe(data.frame(data$age, data$gender, data$f1, data$e5, data$turnout05),
         skew = F)

# individually:
summary(data$age)
ggplot(data, aes(x = age)) + geom_histogram()
ggplot(data, aes(x = age)) + geom_density()
ggsave("slides/pictures/week_16_density.pdf", device = "pdf")

summary(data$gender)
ggplot(data, aes(x = gender)) + geom_bar()

summary(data$f1)
ggplot(data, aes(x = f1)) + geom_bar() 
  
summary(data$e5)
ggplot(data, aes(x = e5)) + geom_bar() 
ggsave("slides/pictures/week_16_barplot.pdf", device = "pdf", width = 8)

summary(data$turnout05)
ggplot(data, aes(x = turnout05)) + geom_bar(stat = "count")
ggsave("slides/pictures/week_16_barplot2.pdf", device = "pdf")

# but remember you also have bivariate representations:
ggplot(data, aes(y = age, x = f1)) + geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 15))
ggsave("slides/pictures/week_16_bivariate.pdf", device = "pdf", width = 8)

ggplot(data, aes(x = e5)) + geom_bar(aes(fill = gender))

# (b)
# turn the three factor variables into numeric ones
data$e5_num <- as.numeric(data$e5)
data$gender_num <- as.numeric(data$gender) # 1 is male, 2 is female
data$f1_num <- as.numeric(data$f1)

# see what happens if you do not include age and gender (confounders) in your model:
model.conf <- lm(e5_num ~ f1_num, data = data)
summary(model.conf)

# this is the model with only the dependent variable as non-factor (gender and f1 are still factors)
model.f <- lm(e5_num ~ age + gender + f1, data = data)
summary(model.f)

# this is the model with all factor variables turned into non-factor
model.n <-lm(e5_num ~ age + gender_num + f1_num, data = data)
summary(model.n)

# compare model with and without confounders
stargazer(model.conf, model.n, type = "text")

stargazer(model.f, model.n, type = "text")
stargazer(model.f, model.n, type = "latex") # for LaTeX users

# (c)
stargazer(model.n, type = "latex")

# t statistic:
se <- sqrt(diag(vcov(model.n)))
t.stat <- model.n$coefficients[2] / se[2]
t.stat #it's -8.56. DF are 1730

# probability of drawing such an extreme t-stat from a t-student distribution:
pt(t.stat, df = 1730) # 1.225248e-17

# probability of drawing such an extreme t-stat from a z distribution (approximation):
pnorm(t.stat) # 5.643376e-18

###############
### THE END ###
###############