###################################################################################################
# Project:                 GV300 - LABa02 sessions                                                #
#                                                                                                 #
# University:              University of Essex                                                    #
#                                                                                                 #
# Programmer:              Lorenzo Crippa                                                         #
#                                                                                                 #
# Week:                    Week 8 (Monday 18th of November, 2019)                                 #
###################################################################################################

# Lorenzo Essex
setwd("C:/Users/lc19059/Dropbox/Shared_Essex/GTA/GV300")

# Lorenzo Macbook
#setwd("/Users/Lorenzo/Dropbox/Shared_Essex/GTA/GV300")

################
library(ggplot2)
library(dplyr)
library(gridExtra)
################

# import data from http://people.stern.nyu.edu/wgreene/Text/econometricanalysis.htm
data <- read.csv("data/Greene_data.csv")

# Price = Sale Price in $ (million),
# Height = Height (inches),
# Width = Width (inches),
# Signed = Dummy variable = 1 if signed, 0 if not,
# Picture = ID number (identifies repeat sales),
# House = Code for auction house where sale took place.

# summarize variables
summary(data)

# describe with histogram and / or densities
ggplot(data, aes(x = PRICE)) + geom_histogram(aes(y = ..density..)) + 
  geom_density(aes(alpha = I(.3)), fill = "aquamarine4") + 
  geom_vline(xintercept = mean(data$PRICE), col = "red") +
  geom_vline(xintercept = median(data$PRICE), col = "blue")

ggplot(data, aes(y = PRICE)) + geom_boxplot() + facet_wrap(data$SIGNED)
ggsave("output/week_8_R_box.pdf", device = "pdf")

ggplot(data, aes(x = HEIGHT)) + geom_histogram(aes(y = ..density..)) + 
  geom_density(aes(alpha = I(.3)), fill = "aquamarine4") + 
  geom_vline(xintercept = mean(data$HEIGHT), col = "red") +
  geom_vline(xintercept = median(data$HEIGHT), col = "blue")

ggplot(data, aes(x = WIDTH)) + geom_histogram(aes(y = ..density..)) + 
  geom_density(aes(alpha = I(.3)), fill = "aquamarine4") + 
  geom_vline(xintercept = mean(data$WIDTH), col = "red") +
  geom_vline(xintercept = median(data$WIDTH), col = "blue")

ggplot(data, aes(x = SIGNED)) + geom_histogram(aes(y = ..density..)) + 
  geom_density(aes(alpha = I(.3)), fill = "aquamarine4")

ggplot(data, aes(x = HOUSE)) + geom_histogram(aes(y = ..density..)) + 
  geom_density(aes(alpha = I(.3)), fill = "aquamarine4")

# Question for the class: why are the two latter graphs conceptually wrong?

data$SIGNED <- as.factor(data$SIGNED)
data$HOUSE <- as.factor(data$HOUSE)
ggplot(data, aes(x = PRICE)) + geom_histogram(aes(y = ..density.., fill = HOUSE))

ggplot(data, aes(x = PRICE)) + geom_histogram(aes(y = ..density..)) + 
  geom_density(aes(alpha = I(.3)), fill = "aquamarine4") + facet_wrap(.~SIGNED)

#####################################
# 1) test the hypothesis that the mean price of a paint equals 4 million dollars.

# 2) test the hypothesis that the mean price of a paint sold in the 
# first house is different from that of a paint sold in the second house

# 3) test the hypothesis that the mean price of a signed paint is higher from the mean price of a non-signed one

# 1)
t.test(data$PRICE, mu = 4, conf.level = .95, alternative = "two.sided")
# rejected
t.test(data$PRICE, mu = 4, conf.level = .95, alternative = "less")
# rejected
t.test(data$PRICE, mu = 4, conf.level = .95, alternative = "greater")
# you never reject the null that the mean is smaller than 4

# 2)
mean(data$PRICE[data$HOUSE == 1])
mean(data$PRICE[data$HOUSE == 2])
mean(data$PRICE[data$HOUSE == 3])

# 2)
data.1.2 <- filter(data, data$HOUSE != 3)

t.test(data.1.2$PRICE ~ data.1.2$HOUSE, conf.level = .95)
# not rejected

# 3)
t.test(data$PRICE~data$SIGNED, conf.level = .95, alternative = "greater")
# you never ever reject the null

########################################
# Draw 30 observations from each of 50 variables normally 
# distributed with mean 0 and standard deviation 1

mu = rep(NA,50) # sets the empty vector which will be filled
for(i in 1:50){
  x = rnorm(n = 30,mean = 0,sd = 1)
  mu[i] = mean(x)
}

# Plot the distribution of the averages
qplot(mu, fill = I("aquamarine3"), binwidth = 0.08,
      xlab = "means", ylab = "count",
      main = "Histogram of means from 50 variables")

# obtain a more general function
draw.fun <- function(n, x, mu = 0, sd = 1, bw = 0.08) {
  library(ggplot2)
  mean = rep(NA, x)
  for(i in 1:x){
    y = rnorm(n = n, mean = mu, sd = sd)
    mean[i] = mean(y)
  }
  plot(qplot(mean, xlab = "means", ylab = "count", 
             main = paste("Histogram of means from", x, "variables\nSamples =", n, "observations"),
             fill = I("aquamarine3"), binwidth = bw, geom = "histogram"))
  #return(mean)
  }

# see how it works:
draw.fun(30, 50)
draw.fun(30, 100)

p1 <- draw.fun(30, 50)
p2 <- draw.fun(30, 100)
p3 <- draw.fun(30, 1000)
p4 <- draw.fun(30, 10000)

grid.arrange(p1, p2, p3, p4, nrow = 2)

###############
### THE END ###
###############