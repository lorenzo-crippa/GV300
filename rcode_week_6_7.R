###################################################################################################
# Project:                 GV300 - LABa02 sessions                                                #
#                                                                                                 #
# University:              University of Essex                                                    #
#                                                                                                 #
# Programmer:              Lorenzo Crippa                                                         #
#                                                                                                 #
# Week:                    Week 6 (Monday 4th of November, 2019)                                  #
###################################################################################################

# Lorenzo Essex
setwd("C:/Users/lc19059/Dropbox/Shared_Essex/GTA/GV300")

# Lorenzo Macbook
#setwd("/Users/Lorenzo/Dropbox/Shared_Essex/GTA/GV300")

################
library(ggplot2)
library(dplyr)
################

#############################################
# Functions, loops, conditions and programs #
#############################################

# First some basic math functions

x <- runif(1000, 0,100)

# change a random value into a 0 (say the number 23)
x[23] <- 0

x <- sort(x)

# generate a linear function of x 
lin.y <-  3*x -4

# plot twoway
plot(x, lin.y, type = "l", ylab = "y")

# generate a quadratic function of x
qua.y <- -1*x^(2) + 3*(x) -4

# plot twoway
plot(x, qua.y, type = "l", ylab = "y")

# generate a cubic function of x
cub.y <- x^3 -1*x^2 + 3*x -4

# plot twoway
plot(x, cub.y, type = "l", ylab = "y")

# generate the ln of x
ln.y = log(x)

# plot twoway
plot(x, ln.y, type = "l", ylab = "y")

# generate the exp of x
exp.y <- exp(x)

# plot twoway
plot(x, exp.y, type = "l", ylab = "y")

# notice the zoom:
x.sub <- x[x>= 10 & x <= 20]
y.sub <- exp(x.sub)
plot(x.sub, y.sub, type = "l", ylab = "y", xlab = "x")

# generate the sin of x
sin.y = sin(x) + 1.3

# plot twoway
plot(x, sin.y, type = "l", ylab = "y")

###############
# derivatives #
###############
  
# linear
d.lin.y <- rep(3, length(x))

# plot twoway
plot(x, lin.y, type = "l", ylab = "y")
lines(x, d.lin.y, type = "l", ylab = "y", col = "red")

# quadratic
d.qua.y <- -2*x + 3

# plot twoway
plot(x, qua.y, type = "l", ylab = "y")
lines(x, d.qua.y, type = "l", ylab = "y", col = "red")

# cubic
d.cub.y <- 3*x^2 -2*x + 3

# plot twoway
plot(x, cub.y, type = "l", ylab = "y")
lines(x, d.cub.y, type = "l", ylab = "y", col = "red")

# logarithm
d.ln.y <- 1/x

# plot twoway
plot(x, ln.y, type = "l", ylab = "y")
lines(x, d.ln.y, type = "l", ylab = "y", col = "red")

# exponential
d.exp.y <- exp(x)

# plot twoway
plot(x, exp.y, type = "l", ylab = "y")
lines(x, d.exp.y, type = "l", ylab = "y", col = "red")

# trigonometric
d.sin.y <- cos(x)

# plot twoway
plot(x, sin.y, type = "l", ylab = "y")
lines(x, d.sin.y, type = "l", ylab = "y", col = "red")
############################################
# Matrix algebra
rm(list = ls())

mat1 <- matrix(c(2, 0, -4, 1, 3, 5, -3, 1, 4, 1, 2, 2), byrow = T, nrow = 4, ncol = 3)
mat2 <- matrix(c(1, -4, -1, 2, 3, 5), byrow = T, nrow = 3, ncol = 2)

matprod <- mat1 %*% mat2
matprod

######################################################################
# ! THE SECOND MATRIX MULTIPLICATION CANNOT BE COMPUTED AS NUMBER OF #
# ROWS FOR THE FIRST MATRIX IS != FROM THE NUMBER OF COLUMNS         #
# FOR THE SECOND MATRIX !                                            #
######################################################################

mat1 <- matrix(c(2, 0, -4, 1, 3, 5, -3, 1, 4), byrow = T, nrow = 3, ncol = 3)
mat2 <- matrix(c(2, 1, -3, 0, 3, 1, -4, 5, 4), byrow = T, nrow = 3, ncol = 3)

matprod <- mat1 %*% mat2
matprod

############################################
# learn how to program a function

# generate 100 random numbers from a random binomial distribution
set.seed(123456)
v <- rbinom(100, size = 4, prob = 0.4)

# program a function that gives us the mean of a vector
mean2 <- function(x) {
  m <- sum(x) / length(x)
  return(m)
}

mean(v)
mean2(v)

# let's program a function that gives us the mean of a vector, 
# this time we do it without using the function "sum()"

# we will need loops and conditions
mean2 <- function(x) {
  summation <- rep(NA, length(x))
  for (i in 1:length(x)) {
    if (i == 1) {
      summation[i] <- x[i]
    }
    else {
      summation[i] <- x[i] + summation[i-1]
    }
  }
  mean <- summation[length(x)] / length(x)
  return(mean)
}

mean(v)
mean2(v)

####################################
# Simulation of normal distribution
# Using a loop
set.seed(010101)
par(mfrow=c(4,1))
for (s in c(5, 50, 500, 5000)) {
  nSims = s
  mu = rep(NA,nSims) # sets the vector to be filled
  nSample=15
  for(i in 1:nSims){
    x = rnorm(n=nSample,mean=0,sd=1)
    mu[i] = mean(x)
  }
  hist(mu,main=paste("Histogram of mu for S =",nSims),col="aquamarine3",cex.main=1.5)
}

#################################################################################
# Exercise 1: a) re-program a function for median and population standard       #
#                deviation and call them "median2 and "sd2" respectively.       #
#             b) generate two vectors (of even and uneven length)               #
#                and apply the new functions to them.                           #
#             c) compare the results with those obtained by applying base R's   #
#                functions to them. Do you observe differences? Why?            #
#                                                                               #
# Exercise 2: imagine you're tossing a coin n times. you random variable        #
#             of interest is X: number of heads.                                #
# a) Write a function that returns a data frame with:                           #
#             1. all possible number of heads                                   #
#             2. all possible ways to get each number of heads                  #
#             3. probability to get a certain number of heads                   #
#                                                                               #
# b) Apply the function you programmed to the count of heads obtained tossing   #
#    a fair coin 10 times and save the results. Obtain a twoway plot that       #
#    reports on the x axis the possible number of heads and on the y axis their #
#    probability.                                                               #
#                                                                               #
# c) Then draw 1000 observations relative to the number of heads actually       #
#    obtained by tossing 10 times a fair coin. Obtain a histogram reporting     #
#    the results and compare it with the twoway plot obtained in point b)       #
#                                                                               #
# c+) Draw 5 observations relative to the number of heads obtained by tossing   #
#    10 times a fair coin. Obtain a histogram reporting the results again and   #
#    compare it with what obtained above. Do you observe differences?           #
#################################################################################

###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################

#############
# Solutions #
#############

# Exercise 1
median2 <- function(x) {
  x <- sort(x)
  if (length(x) %% 2 != 0) {
    med <- x[(length(x)+1)/2]
  } else if (length(x) %% 2 == 0) {
    med <- (x[length(x)/2] + x[(length(x)/2)+1])/2
  }
  return(med)
}

v <- rbinom(51, size = 10, prob = 0.1) # uneven vector
median(v)
median2(v)

v <- rbinom(50, size = 5, prob = 0.5) # even vector
median(v)
median2(v)

sd2 <- function(x) {
  m <- mean(x)
  sq.diff <- rep(NA,length(x))
  for (i in 1:length(x)) {
    sq.diff[i] <- (x[i]-m)^2
  }
  sd <- sqrt(sum(sq.diff) / length(x))
  return(sd)
}

sd(v)
sd2(v)

# the two differ because base R's sd() function computes the sample standard deviation
# (which has n-1 at the denominator) while the one we programmed computes the population
# standard deviation (which has n at the denominator). You could also program the sd2()
# function so as to let the user choose what standard deviation is desired:

sd2 <- function(x, type = "pop") {
  if (type != "pop" & type != "sample") {
    stop("Choose either 'sample' or 'pop'!")
  } else {
    m <- mean(x)
    d <- rep(NA, length(x))
    for (i in 1:length(x)) {
      d[i] <- (x[i]- m)^2}
      if (type == "pop") {
        sd <- sqrt(sum(d) / length(x))
      } else {
        sd <- sqrt(sum(d) / (length(x) - 1))
        }
  return(sd)}
}

sd(v)
sd2(v, "sample") # they are the same now
sd2(v, "pop")    # pop sd
sd2(v, "hello")

# Exercise 2
binomial.fun <- function(n, p) {
  outcomes <- 0:n
  combinations <- rep(NA, n)
  probability <- rep(NA, n)
  m <- n+1
  for (i in 1 : m) {
    j = i-1
    combinations[i] <- factorial(n) / (factorial(j) * factorial(n-j))
    probability[i] <- combinations[i]*(p^(j))*((1-p)^(n-j))
  }
return(data.frame(outcomes, combinations, probability))
}

binomial.fun(2, 0.5)
binomial.fun(10, 0.5)

df <- binomial.fun(10, 0.5)
y <- rbinom(10000, 10, 0.5)

par(mfrow=c(1,2))
plot(df$outcomes, df$probability, xlab = "outcomes", ylab = "probability", frame = F)
hist(y, xlim = range(0:10))

df <- binomial.fun(10, 0.5)
y <- rbinom(5, 10, 0.5)

par(mfrow=c(1,2))
plot(df$outcomes, df$probability, xlab = "outcomes", ylab = "probability", frame = F)
hist(y)

###############
### THE END ###
###############