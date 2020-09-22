###################################################################################################
# Project:                 GV300 - LABa02 sessions - Problem Set 4 answers                        #
#                                                                                                 #
# University:              University of Essex                                                    #
#                                                                                                 #
# Programmer:              Lorenzo Crippa                                                         #
#                                                                                                 #
###################################################################################################

library(psych)
library(dplyr)
library(ggplot2)
library(stargazer)
library(reshape2)

# Lorenzo Essex
setwd("C:/Users/lc19059/Dropbox/Shared_Essex/GTA/GV300")

# Lorenzo Mac
#setwd("/Users/Lorenzo/Dropbox/Shared_Essex/GTA/GV300")

# clear all
rm(list=ls())

##############
# exercise 1 #
##############

set.seed(1234)

df <- data.frame(
  x1 = rnorm(2000, mean = 0, sd = 1),
  x2 = rnorm(2000, mean = 0, sd = 1),
  x3 = rnorm(2000, mean = 0, sd = 1)
)

describe(df$x1) 
# mean is -0.01 and sd is 0.99 
# 1) 68% of the observations should be in [-1;0.98]
# 2) 95% of the observations should be in [-1.99;1.97]
# 3) 99% of the observations should be in [-2.98;2.96]

# represent:
hist(df$x1, freq = F)
abline(v = -1, col = "red")
abline(v = 0.98, col = "red")
abline(v = -1.99, col = "blue")
abline(v = 1.97, col = "blue")
abline(v = -2.98, col = "green")
abline(v = 2.96, col = "green")

# with ggplot2:
ggplot(df, aes(x = x1)) + geom_histogram(aes(y = ((..count..)/sum(..count..))*100)) +
  ylab("percentages") +
  geom_vline(xintercept = mean(df$x1), col = "white") +
  geom_vline(xintercept = c(-1, 0.98), col = "red") + 
  geom_vline(xintercept = c(-1.99, 1.97), col = "blue") +
  geom_vline(xintercept = c(-2.98, 2.96), col = "green")
ggsave("slides/pictures/problemset4_hist.pdf", device = "pdf")

# percentiles
quantile(df$x1, probs = c(.005, .025, .16, .84, .975, .99))

# chi squares
df$chi2 <- df$x1^2

ggplot(df, aes(x = chi2)) + geom_histogram(aes(y = ((..count..)/sum(..count..))*100)) +
  ylab("percentages") + geom_vline(xintercept = 3.84, col = "red")
ggsave("slides/pictures/problemset4_hist_chi2.pdf", device = "pdf")

# percentiles
quantile(df$chi2, probs = .95)

# F 
df$F <- (df$x2^2)/(df$x3^2)

ggplot(df, aes(x = F)) + geom_histogram(aes(y = ((..count..)/sum(..count..))*100)) +
  ylab("percentages") #+ geom_vline(xintercept = 161.4, col = "red")
ggsave("slides/pictures/problemset4_hist_F.pdf", device = "pdf")

# percentiles
quantile(df$F, probs = .95)

# t
df$t <- df$x2 / sqrt(df$chi2)

ggplot(df, aes(x = t)) + geom_histogram(aes(y = ((..count..)/sum(..count..))*100)) +
  ylab("percentages")
ggsave("slides/pictures/problemset4_hist_T.pdf", device = "pdf")

# percentiles
quantile(df$t, probs = .95)


##############
# exercise 2 #
##############

rm(list=ls())

data <- read.csv("data/baseball.csv")

# how many cases?
unique(data$weightpounds) # 89 cases

# mean by groups:
aggregate(data$heightinches, by = list(data$weightpounds), FUN = mean)

# save as data frame
df <- aggregate(data$heightinches, by = list(data$weightpounds), FUN = mean)
df<- rename(.data = df,
  weightpounds = Group.1,
  expected.height = x)

# represent 1
ggplot(df, aes(x = weightpounds, y = expected.height)) + geom_point() + 
  xlab("weight") + ylab("E(height)") + geom_smooth(method = "lm")
ggsave("slides/pictures/problemset4_scatter1.pdf", device = "pdf")

# represent 2
ggplot(data, aes(x = weightpounds, y = heightinches)) + geom_point() + 
  xlab("weight") + ylab("height") + geom_smooth(method = "lm")
ggsave("slides/pictures/problemset4_scatter2.pdf", device = "pdf")

# two models
mod1 <- lm(data = df, expected.height ~ weightpounds)
mod2 <- lm(data = data, heightinches ~ weightpounds)

# results
stargazer(mod1, mod2,keep.stat = c("n", "adj.rsq", "f"), type = "text")

# LaTeX
stargazer(mod1, mod2,keep.stat = c("n", "adj.rsq", "f"))

##############
# exercise 3 #
##############

rm(list=ls())

# input data
data <- data.frame(
  distric = seq(1,6),
  incumbent = c("Matt Salmon", "Ed Pastor", "Jim Kolbe", "Bob Stump", "John Shadegg", "J.D. Hayworth"),
  money = c(362, 418, 712, 346, 426, 1839),
  vote.share = c(65, 68, 52, 65, 69, 53)
)

# model
mod <- lm(data = data, vote.share ~ money)
summary(mod)

stargazer(mod,keep.stat = c("n", "adj.rsq", "f"), type = "text")

# LaTeX
stargazer(mod,keep.stat = c("n", "adj.rsq", "f"))

data$predicted <- mod$fitted.values

# plot
ggplot(data, aes(y = vote.share, x = money)) +
  geom_smooth(method= "lm", se=FALSE, color = "lightgrey") +  
  geom_segment(aes(xend = money, yend = predicted), color = "red") +  
  geom_point() +
  geom_point(aes(y = predicted), shape = 1) +
  ylab("vote share")
ggsave("slides/pictures/problemset4_scatterfit.pdf", device = "pdf")

# regress on the intercept only
mod2 <- lm(data = data, vote.share ~ vote.share)
summary(mod2)
mean(data$vote.share)

# ordinal variable
data$m.low <- ifelse(data$money < 500, 1, 0)

# model
mod3 <- lm(data = data, vote.share ~ m.low)
summary(mod3)

# group-wise mean
aggregate(data$vote.share, by = list(data$m.low), FUN = mean)

##############
# exercise 5 #
##############

# (a)
rm(list=ls())

df <- data.frame(
  university = c(rep(0, 1000), rep(1, 1000)),
  noise = rnorm(2000)
)

df$income <- 15000 + 5000*df$university + 1000*df$noise

mod <- lm(data = df, income ~ university)
summary(mod)

stargazer(mod, keep.stat = c("n", "adj.rsq", "f"), type = "text")

stargazer(mod, keep.stat = c("n", "adj.rsq", "f"))

# (b)
rm(list=ls())
df <- data.frame(
  intelligence = runif(2000),
  luck = runif(2000),
  noise = rnorm(2000)
)

df$university <- ifelse(df$intelligence+df$luck>1, 1, 0)
df$income <- 15000 + 10000*df$intelligence + 1000*df$noise

mod <- lm(data = df, income ~ university)
summary(mod)

stargazer(mod, keep.stat = c("n", "adj.rsq", "f"), type = "text")

stargazer(mod, keep.stat = c("n", "adj.rsq", "f"))

###############
### THE END ###
###############