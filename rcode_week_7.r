###################################################################################################
# Project:                 GV300 - LABa02 sessions - Problem Set 2 answers                        #
#                                                                                                 #
# University:              University of Essex                                                    #
#                                                                                                 #
# Programmer:              Lorenzo Crippa                                                         #
#                                                                                                 #
###################################################################################################

# exercise 6
y <- 0:7 # the number of successful applications in 7 countries can go from 0 to 7
mass.function <- dbinom(y, size = 7, prob = 0.3) # probabilities associated with each value of x

# check:
sum(mass.function) # 1, ok

df <- data.frame(y, prob=mass.function) # turn the values into a data frame

plot(df$y, df$prob, type = "l", frame = F, ylab = "probability", xlab = "y", 
     main = "Probability mass function")

# Expected value is the mean: n*p:
mean <- 7*0.3 # 2.1

# variance is n*p*(1-p):
var <- 7*0.3*0.7 # 1.47

############
# GV300: Problem set 2, exercise 7

# draw 1000 random observations from a binomial distribution with n = 7 and p = 0.3

obs <- rbinom(1000, size = 7, p = 0.3)

hist(obs, probability = T)
lines(df$y, df$prob, col = "red")

mean(obs) # 2.205, very close to 2.1
var(obs) # 1.49, very close to 1.47
############