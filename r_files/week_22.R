###################################################################################################
# Project:                 GV300 - LABa02 sessions                                                #
#                                                                                                 #
# University:              University of Essex                                                    #
#                                                                                                 #
# Programmer:              Lorenzo Crippa                                                         #
#                                                                                                 #
# Week:                    Week 22 (Monday 24th of February, 2020)                                #
###################################################################################################

# clear
rm(list=ls())

# Lorenzo Essex
setwd("C:/Users/lc19059/Dropbox/Shared_Essex/GTA/GV300")

# Lorenzo Macbook
#setwd("/Users/Lorenzo/Dropbox/Shared_Essex/GTA/GV300")

#################
library(tidyverse)
library(texreg)
library(sandwich)
library(AER) # this is needed to have ivreg() !!!
library(rdd) # for reg discontinuity
#################

##############
# Question 1 #
##############

set.seed(123)

X <- rpois(1000, lambda = 3)
Z <- rbinom(1000, size = 8, prob = 0.4)
T <- 2 + 3*Z - 2*X + rnorm(1000)
Y = 1 + 2*T -3*X + rnorm(1000)

# let's look at the models we get if we do not instrument
model.bias <- lm(Y ~ T)
summary(model.bias) 
# we estimate a slope of 2.59 whereas the true value is 2. The intercept is also super biased (-11, it should be 1)

# if we control for the confounder X instead:
model <- lm(Y ~ T + X)
summary(model) # we get unbiased estimates: intercept is around 1, slope of T is around 2 and slope of X is around -3

# often, though, we can't control for our confounders! For instance if we don't observe them or if we don't have data.
# thus, we can instrument:
model.iv <- ivreg(Y ~ T | Z)
summary(model.iv) 
# notice that the slope is now unbiased, but the intercept is still biased: the causal effect estimated is that OF T ONLY.

# display the results:
screenreg(list(model.bias, model, model.iv), include.rmse = F, stars = c(.01, .05, .1))

# now generate a bad instrument
zNot <- runif(1000)
# zNot can be a bad instrument for two reasons:

# 1)
T <- 2 + 3*Z - 2*X + 0*zNot + rnorm(1000) # zNot is not a cause of T
Y = 1 + 2*T -3*X + rnorm(1000)

model.iv.bad <- ivreg(Y ~ T | zNot)
summary(model.iv.bad) # slope is very biased: we used a bad instrument!

# 2)
X <- 1 + 2*zNot + rnorm(1000)
T <- 2 + 3*Z - 2*X + 5*zNot + rnorm(1000) # zNot IS a cause of T but also of X 
Y = 1 + 2*T -3*X + rnorm(1000)

# (exclusion restriction is violated unless we control for X too)

model.iv.bad2 <- ivreg(Y ~ T | zNot)
summary(model.iv.bad2) # slope is very biased: we used a bad instrument!

# if we controlled for the X (confounder effected by zNot) we would get unbiased results:
model.iv.ok <- ivreg(Y ~ T + X | zNot + X)
summary(model.iv.ok) # this way we get unbiased estimates

# print results
screenreg(list(model.iv.bad, model.iv.bad2, model.iv.ok), include.rmse = F, stars = c(.01, .05, .1))

# for LaTeX
texreg(list(model, model.iv, model.iv.bad, model.iv.bad2), include.rmse = F,
       custom.model.names = c("Linear (controls)", "Good IV", "Weak IV", "Exclusion Restriction"),
       stars = c(.01, .05, .1))


##############
# question 2 #
##############

rm(list = ls())

set.seed(123)
Instrument <- rbinom(50000, size = 1, prob = 0.4)
ObservableThing <- rnorm(50000)
UnobservableThing <- rnorm(50000)

VariableOfInterest <-ifelse(ObservableThing + UnobservableThing + Instrument >= 2.5, 1, 0)
table(VariableOfInterest)                            

OutcomeVariable = UnobservableThing + 1 * VariableOfInterest + rnorm(50000)

# a) effect is 1 (see equation of the DGP)
# notice that in order to estimate this unbiased effect we don't even need to control for ObservableThing,
# because this is not a confounder in the DGP of OutcomeVariable: we only need to control for UnobservableThing,
# which is a confounder:
model <- lm(OutcomeVariable ~ VariableOfInterest + UnobservableThing)
summary(model) # indeed all estimates are unbiased (intercept included: it's 0)

# b)

model1 <- lm(OutcomeVariable ~ VariableOfInterest)
summary(model1) # 2.31 (biased)

# c)

# 2SLS manually:

# 1st stage
first.st <- lm(VariableOfInterest ~ Instrument)
fitted.var <- first.st$fitted.values # save the fitted values!

# 2nd stage
second.st <- lm(OutcomeVariable ~ fitted.var)
summary(second.st) # 1.08 (unbiased)

# ivreg
model.iv <- ivreg(OutcomeVariable ~ VariableOfInterest | Instrument)
summary(model.iv)

# show that results are the same:
screenreg(list(second.st, model.iv), include.rmse = F, stars = c(.01, .05, .1),
          custom.model.names = c("Manually", "ivreg"))

texreg(list(second.st, model.iv), include.rmse = F, stars = c(.01, .05, .1),
       custom.model.names = c("Manually", "ivreg"))

# d)
Instrument2 <- rbinom(50000, size = 1, prob = 0.02)

cor(Instrument, Instrument2) # notice the correlation of 0.0015: it's very very low!
cor(VariableOfInterest, Instrument2) # 0.0053: very low too

model.iv.weak <- ivreg(OutcomeVariable ~ VariableOfInterest | Instrument2)
summary(model.iv.weak) # biased estimate

# e)
VariableOfInterest = ObservableThing + UnobservableThing + 0.05*Instrument

model.iv2 <- ivreg(OutcomeVariable ~ VariableOfInterest | Instrument)
summary(model.iv2) # biased estimate: weak effect of the instrument on treatment variable!

# f)
OutcomeVariable = UnobservableThing + VariableOfInterest + 0.05*Instrument + rnorm(50000)

model.iv3 <- ivreg(OutcomeVariable ~ VariableOfInterest | Instrument)
summary(model.iv3) 
# biased: the instrument has an effect on the outcome variable(excl. restr. not met), even if weak it biases stuff !

screenreg(list(model1, model.iv, model.iv.weak, model.iv2, model.iv3), include.rmse = F,
          custom.model.names = c("(b)", "(c)", "(d)", "(e)", "(f)"), stars = c(.01, .05, .1))

texreg(list(model1, model.iv, model.iv.weak, model.iv2, model.iv3), include.rmse = F,
       custom.model.names = c("(b)", "(c)", "(d)", "(e)", "(f)"), stars = c(.01, .05, .1))

##############
# question 3 #
##############

rm(list=ls())

data <- read.csv("data/USStateLegislature.csv")

data %>% ggplot(aes(y = turnout, x = votemargin)) + geom_point() + geom_vline(xintercept = 0)

data$indicator[data$votemargin >= 0] <- 1
data$indicator[data$votemargin < 0] <- 0

data$indicator <- as.factor(data$indicator)
data %>% ggplot(aes(y = turnout, x = votemargin)) + geom_point(aes(colour = indicator)) + 
  geom_vline(xintercept = 0) + theme(legend.position = "none") # this way we get rid of the legend
ggsave("slides/pictures/week_22_3scatt.pdf", device = "pdf")

# first save plot:
p <- data %>% ggplot(aes(y = turnout, x = votemargin)) + geom_point(aes(colour = indicator)) + 
  geom_vline(xintercept = 0) + theme(legend.position = "none")

# automatic smooth model:
p + geom_smooth(aes(group = indicator))
ggsave("slides/pictures/week_22_3locfit.pdf", device = "pdf")

# linear model:
p + geom_smooth(aes(group = indicator), method = "lm")
ggsave("slides/pictures/week_22_3linfit.pdf", device = "pdf")

# quadratic model:
p + geom_smooth(aes(group = indicator), method = "lm", formula = y ~ x + I(x^2))
ggsave("slides/pictures/week_22_3sqrfit.pdf", device = "pdf")

# loess function:
p + geom_smooth(aes(group = indicator), method = "loess")

# models:
model <- RDestimate(turnout ~ votemargin, data = data, cutpoint = 0, se.type = "HC1")
summary(model) 
# since we didn't specify a bandwidth (bw) the program automatically estimates the model calculating an optimal
# bandwidth, and using that bandwith, half that bandwith and twice that bandwith and reports its size and observations

# otherwise we can specify it:
model2 <- RDestimate(turnout ~ votemargin, data = data, cutpoint = 0, bw = c(0.10), se.type = "HC1")
summary(model2)

model3 <- RDestimate(turnout ~ votemargin, data = data, cutpoint = 0, bw = c(1.5), se.type = "HC1")
summary(model3) # with a large bandwith you're using all available observations! (except NAs)


# (d) 
# Testing assumption 2: discontinuity only occurs at cut-off = 0
summary(RDestimate(turnout~votemargin,cutpoint=-.1,data=data)) # not significant
summary(RDestimate(turnout~votemargin,cutpoint=.1,data=data)) # not significant

# Testing assumption 3: re-run the RDD analysis using other covariates (not the outcome variable) as dep. var:

# scatterplot
data %>% ggplot(aes(y=democraticvoteshare_president,x=votemargin)) + geom_point(aes(col = indicator)) +
  geom_vline(xintercept=0) + theme(legend.position = "none")
ggsave("slides/pictures/week_22_ass3.pdf", device = "pdf")

ass.3 <- RDestimate(democraticvoteshare_president ~ votemargin, cutpoint = 0, data = data, se.type = "HC1")
summary(ass.3) # not significant

# There is no discontinuity in the relationship of x and any other covariate (at least when considering
# the covariates for which we have observation, i.e. democraticvoteshare_president)

# Assumption 1 in principle is not testable. Yet, we can get some evidence that could speak to it.
# The assumption tells us that manipulation/sorting into T and C is not possible: evaluate by density plot. 
# If there is no increased density around the cut-off, we probably do not observe manipulation. 

DCdensity(data$votemargin,0, verbose=T, plot=T)
dev.copy("slides/pictures/week_22_ass1.pdf", device = pdf)
dev.off()

# This command estimates the difference in heights of the 
# density plot over the forcing variable x at the cut-off 
# We should not be able to reject the null that there is no
# difference. In this example, we indeed cannot reject the null.
# The p-value is .17
# The command also provides a plot of the density of x for eyeballing.


###############
### THE END ###
###############