###################################################################################################
# Project:                 GV300 - LABa02 sessions                                                #
#                                                                                                 #
# University:              University of Essex                                                    #
#                                                                                                 #
# Programmer:              Lorenzo Crippa                                                         #
#                                                                                                 #
# Week:                    Week 21 (Monday 17th of February, 2020)                                #
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
library(texreg)
library(haven)
library(tidyverse)
library(AER)
library(sandwich)
#################

#############################################
# illustration of the power of a test

# suppose you have a random variable X normally distributed, with variance = 3:
# X ~ N(mu, sigma = 3)
# Suppose you want to test the hypothesis that mu (mean of X) is smaller or equal to 100.
# Let's represent the test:
X <- seq(80, 120, by = 0.001) # create a vector of possible values of X
# let's save the densities of each of these values under the assumption that the mean is 100
Y <- dnorm(X, mean = 100, sd = 3)
# plot the distribution under the assumption that the null-hypothesis is true:
plot(X, Y, type = "l", ylab = "density")

# now, our test is two-tailed (we test the null that the mean is <= 100). Suppose our level of significance is alpha = 0.05
alpha <- .05

# what critical value in our distribution leaves 95% of the observations to its right in this distribution? 
# Who knows! Let's ask R using the quantile function:
cval <- qnorm(0.95, mean = 100, sd = 3, lower.tail = T) # it's 104.9346 

# let's add it to our previous plot:
abline(v = cval, col = "red")
# if our test statistics end up on the left of the red line we WILL NOT REJECT the null hypothesis. 
# If it ends up on the right of the red line we WILL REJECT the null hypothesis

# let's identify clearly these regions on the plot with some text labels:
text(102, -0.002, "Retain null", col = "blue")
text(107, -0.002, "Reject null", col = "red")

# now, suppose the true value of the mean of this distribution was actually 106. 
# How will this random variable be distributed? Let's plot it! 
# We add to the previous plot the distribution of the random variable under the TRUE value of the mean, that is 106

# first, save the densities in a separate vector, then plot it:
Y2 <- dnorm(X, mean = 106, sd = 3)
lines(X, Y2, type = "l", col = "blue")

# now, let's give some colors to some of the areas of interest for us in this plot here 
# (we use the polygon() function to color areas, but there is no reason for you to learn how to use it)
# first save two nice colors with some transparency (the "alpha" value below)
mycolred <- rgb(255, 0, 0, max = 255, alpha = 125, names = "red50") # this is just the 1st color we want
mycolblue <- rgb(0, 0, 255, max = 255, alpha = 125, names = "blue50") # this is just the 2nd color we want

polygon(c(X[X<=cval], cval), c(Y2[X<cval], 0), col = mycolred)
polygon(c(X[X>cval], cval), c(Y2[X>cval], 0), col = mycolblue)

# what's the red area in our test? It's the probability of NOT REJECTING the null (we're in the non-rejection region) when
# the null-hypothesis is false (we have built the alternative distribution which is exactly built under the assumption that
# the null-hypothesis is false, because the mean is 106). So, we DON'T want our test to have a large red area!
# We call this area, this probability, beta. We can ask R to tell us what value it has precisely here:
beta <- pnorm(cval, mean = 106, sd = 3) # notice that the mean is 106: we are under the assumption that our null was false!

# we have a beta of 0.36, the probability of not rejecting the null when it's false is 0.36 IF THE MEAN IS PRECISELY 106.

# Then, what is the blue area? It's the "power of the test" (or 1-beta), thus the probability of rejecting 
# the null (we are in the rejection region) when it is false! It is precisely what we want our test to do.
# In this case, IF THE MEAN IS PRECISELY 106, the power of the test is:
1-beta # 0.63876

# notice that beta and 1-beta (power of the test) depend on the ACTUAL TRUE VALUE 
# of the parameter we're testing on (in this example, the mean). For instance, say the true mean is 110:
beta2 <- pnorm(cval, mean = 110, sd = 3)
1-beta2
# in this case beta, the probability of not rejecting the null when the null hypothesis is false 
# (and true mean is 110) is 0.05, whereas the power of a test is 0.95 

# This is why the power of a test is way more difficult to define than the level of significance (which is
# the probability of rejecting the null when it is true): there is only one way the null-hypothesis can be true, but
# there are plenty of possible ways (infinite, in fact) that the null-hypothesis can be false. The power of a test will
# depend on the actual true value of the parameter we want to test on. If you remember the power analysis we did last time,
# we were IMPOSING, indeed, a true effect of 0.5 and all the analysis of the necessary sample size for that effect to be
# detected was based on that.

# now, repeat the procedure to get and save the plot (for my slides):
plot(X, Y, type = "l", ylab = "density", xlim = c(90, 115)) # this latter argument "limits" the plot to these x values
text(102, -0.0025, "Retain null", col = "blue")
text(107, -0.003, "Reject null", col = "red")
lines(X, Y2, type = "l", col = "blue")
polygon(c(X[X<=cval], cval), c(Y2[X<cval], 0), col = mycolred)
polygon(c(X[X>cval], cval), c(Y2[X>cval], 0), col = mycolblue)
abline(v = cval, col = "red")

# save it:
dev.copy("slides/pictures/week_21_power.pdf", device = pdf, width = )
dev.off()

#############################################################################

# clear
rm(list=ls())

# load dataset
card <- read_dta("data/Card_data.dta")
head(card) # visualize the first 10 entries

# turn some categorical variables into integers (they are recognized as continuous by R)
card$nearc2 <- as.factor(card$nearc2)
card$nearc4 <- as.factor(card$nearc4)
card$black <- as.factor(card$black)
card$smsa <- as.factor(card$smsa)
card$south <- as.factor(card$south)
card$enroll <- as.factor(card$enroll)

### start from descriptive statistics
describe(card)

# we can get plots now. 
# The %>% operator which I use below is called a "pipe" operator, and is very useful whenever you need to apply
# many functions to some R object. The logic behind it is that it "channels" everything that is on its left-hand side 
# to the function(s) on its right-hand side. It is part of the tidyverse package, which contains packages you've been 
# using over and over and over like dplyr and ggplot2. The pipe operator helps you to avoid doing something like: 
# function1(function2(function3(argument))) when you need to apply many functions. This example would become, instead: 
# argument %>% function3() %>% function2 %>% function1, where you apply these functions in this order. It is a bit 
# confusing at first because it goes from left to right, whereas in R we are used to go from right to left since we 
# use so much the <- operator, but after a while it becomes clear and it is definitely very useful. Try to use it!

# Univariate plots (histograms, densities, boxplots...)
card %>% ggplot(aes(x = educ)) + geom_histogram()
card %>% ggplot(aes(x = wage)) + geom_density() # very skewed, maybe better to log it?
card %>% ggplot(aes(x = log(wage))) + geom_density() # better!

# bivariate plots (scatterplot)
card %>% ggplot(aes(x = educ, y = log(wage))) + geom_point()

# multivariate plots (scatterplot where there's a mapping variable - here discrete, could be continuous)
card %>% ggplot(aes(x = educ, y = log(wage))) + 
  geom_point(aes(colour = nearc4, alpha = I(.5))) + # alpha is used to change the transparency of the dots
  xlab("years of schooling") + ylab("log of wage") +
  scale_color_discrete("4-years\ncollege?", # this function changes the legend of the plot (for the mapping var)
                       breaks = c(0,1),
                       labels = c("no", "yes"))
ggsave("slides/pictures/week_21_scatterplot.pdf", device = "pdf")

# Ok, now let's move to the models, and play around a bit

####### start from OLS estimation
model.ols <- lm(log(wage) ~ educ + exper + black + south + married + smsa, data = card)

# we need robust standard errors. Our t-statistics and pvalues will change:
robustVC.ols <- vcovHC(model.ols, type = "HC1") # this is a robust vcov matrix: the squared root of its diagonal will give us the robust Standard Errors!
coeftest(model.ols, vcov. = robustVC.ols) # show the results with robust SE: t-tests change of course!

# let's also save the robust standard errors from the robust vcov matrix and the pvalues from the t-test
# performed with them in two separate vectors (we'll need them below when we'll print the results using screenreg)
robust.ols <- sqrt(diag(robustVC.ols)) # save the robust standard errors

# to save the pvalues we need to prepare a small loop, where we iterate the t-test for each coefficient estimated
pvalues.ols <- rep(NA, length(robust.ols)) # prepare an empty vector to store the pvalues: always do it OUT of a loop!
for (i in 1:length(pvalues.ols)) { # this is our loop
  tstat <- abs(model.ols$coefficients[i]/robust.ols[i]) # save individual tstat for each coefficient. Use absolute value! 
  names(tstat) <- NULL # remove the name associated with the tstat (the name of the corresponding variable)
  pvalues.ols[i] <- (1-pt(tstat, df = 3002))*2 # perform the 2-tailed t-test (thus, we multiply times 2)
}

#######  move to instrumental variable: 2SLS estimation
model.iv <- ivreg(formula = log(wage) ~ educ + exper + black + south + married + smsa | 
                    nearc4 + exper + black + south + married + smsa, data = card) 

# this function does the 2SLS procedure for you. Doing that manually would give you the same results. We can show it:
first.stage <- lm(educ ~ nearc4 + exper + black + south + married + smsa, data = card)

# in order to move to the second stage, we first create a data frame with the variables we have used in the first only,
# and only with the observations where there is full information (no NAs). Otherwise we couldn't use the fitted values
# from the first stage, for the instrumented variable, as independent variable for the second stage
iv.data <- filter(card,
                  !is.na(card$educ) & !is.na(card$nearc4) & !is.na(card$exper) & !is.na(card$black) &
                    !is.na(card$south) & !is.na(card$married)& !is.na(card$smsa) & !is.na(log(card$wage)))
# now store the fitted values from the first stage
iv.data$educ.fitted <- first.stage$fitted.values

second.stage <- lm(log(wage) ~ educ.fitted + exper + black + south + married + smsa, data = iv.data)

# check that parameters are the same when done manually and when using ivreg() (SEs are not gonna be the same):
summary(second.stage)
summary(model.iv)
# ok, they are, cool!

# compute robust standard errors:
robustVC.iv <- vcovHC(model.iv, type = "HC1") # same as before: robust variance-covariance matrix: the squared root of its diagonal will be the robust standard errors
coeftest(model.iv, vcov. = robustVC.iv)

# let's also save the robust standard errors and pvalues (we'll need them below using screenreg)
robust.iv <- sqrt(diag(robustVC.iv))

# same procedure as before to save pvalues: a small loop
pvalues.iv <- rep(NA, length(robust.iv)) 
for (i in 1:length(pvalues.iv)) {
  tstat <- abs(model.iv$coefficients[i]/robust.iv[i]) 
  names(tstat) <- NULL 
  pvalues.iv[i] <- (1-pt(tstat, df = 3002))*2
}


# table with results: print robust SE and related pvalues we have stored in the vectors
screenreg(list(model.ols, model.iv), 
          include.rmse = F,
          override.se = list(robust.ols, robust.iv), # we need to tell screenreg to print the ROBUST standard errors!
          override.pvalues = list(pvalues.ols, pvalues.iv), # we need to change the pvalues too
          stars = c(0.01, 0.05, 0.10)
)

# print it for LaTeX users (slides):
texreg(list(model.ols, model.iv), 
       include.rmse = F,
       override.se = list(robust.ols, robust.iv),
       override.pvalues = list(pvalues.ols, pvalues.iv),
       stars = c(0.01, 0.05, 0.10)
)

######## How good is the instrument?
# let's first look at this plot:
card %>% ggplot(aes(y = educ, x = nearc4)) + geom_boxplot() + 
  xlab("4-years college in the county?") + ylab("years of schooling") + 
  scale_x_discrete(breaks = c(0,1), labels = c("no", "yes"))
ggsave("slides/pictures/week_21_boxplot2.pdf", device = "pdf")
# it does not look like the presence of a 4-years college in the county affects years of schooling: 
# distributions are basically identical, I honestly can't distinguish the two boxplots. Of course, it might
# be that the effect of the instrument on the IV emerges after controlling for our included
# control variables, but this already is not a very good sign per se.

# let's move to more analytical tools. Let's print diagnostics from our IV model:
summary(model.iv, diagnostics = TRUE) # this provides some elements to evaluate it (see "diagnostic tests" in the table)

# we can also check what's the F statistics of the first stage of our 2SLS model. Let's obtain it
summary(first.stage) # the F statistic is large, thus the instrument is arguably strong.

# Still, does it meet the exclusion restriction assumption? Arguably not.

###############
### THE END ###
###############