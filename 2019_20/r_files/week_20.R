###################################################################################################
# File-Name:       	gv300_problemSet4.r
# Date:            	19/11/15
# Author:          	DD	
# Machine:         	DD's MacBook Pro
###################################################################################################
###################################################################################################

rm(list=ls())

library(dplyr)
library(lattice)
library(foreign)
library(tidyverse)
library(ri)

# Lorenzo Crippa MacBook Pro project folder
#setwd("/Users/Lorenzo/Dropbox/Shared_Essex/GTA/GV300")

# Lorenzo Crippa Essex PC project folder
setwd("C:/Users/lc19059/Dropbox/Shared_Essex/GTA/GV300")

###################################################################################################
# Statistical power evaluation
###################################################################################################
# Check out EGAP: http://egap.org/content/power-analysis-simulations-r
# Let's generate two variables and run a two-sample t-test on differences
# in their mean

# First, we define the set of sample sizes we look at: N=5 to N=500
sample.size <- seq(from=5, to=500, by=5)

# Then we generate a new variable in which we will write the power of a test as function 
# of sample size. Remember what power of a test is: prob(reject null|Null is false)
power <- rep(NA, length(sample.size))

# We set the level of significance to alpha = .05
alpha <- 0.05

# We are simulating data, so we need to define how many simulations we will run, let's say 500
sims <- 500

# And, finally, we state an expected effect size; the power analysis we are running
#  will tell us how many observations we need to detect this effect size. 
H_a <- .5

# two nested loops:
for (j in 1:length(sample.size)){ # this first loop runs on the possible sample sizes defined above
  N <- sample.size[j] # each different iteration, it saves the sample size in N
  significant.experiments <- rep(NA, sims) # generates an empty vector, which we'll fill in the loop below 

  for (i in 1:sims){
    # This is the core, here we run a t.test on a variable that is normally
    # distributed with mean zero and standard deviation 1 and has N/2 observations 
    # The other variable is also normally distributed but its mean is shifted by H_a=.5. 
    # This is the effect we want to detect!
    # For this second variable we create the other N/2 observations so we got N observations
    # in total.
    p.value <- t.test(rnorm(N/2),rnorm(N/2,H_a))$p.value # save the p-value of the t-test
    # Now we record for every iteration of our simulation whether we find a signficant
    # difference between the two variables
    significant.experiments[i] <- (p.value <= alpha) # this will be a Boolean vector: T or F!
    # record "TRUE" if we found that p-value of the t-test is smaller than our level of significance, F otherwise
  }

  # Now we summarise for how many iterations of our simulation we got a significant result
  power[j] <- mean(significant.experiments) # the mean will be the fraction of "TRUE" we get out of 500 iterations
}

# Let's put the statistial power we determined for each sample size into a data frame to plot it
data <- as.data.frame(cbind(sample.size,power))

# We add a horizontal line at .8 power, that's the standard we
# usually want to be met by a test and sample size in terms of 
# statistical power
data %>% ggplot(aes(y = power, x = sample.size)) + geom_line() +
  geom_hline(aes(yintercept = .8), color='red') +
  scale_y_continuous(breaks=seq(0,1,.2),labels=seq(0,1,.2)) + xlab("Sample size") +
  ylab('Statistical power') + 
  ggtitle('Power analysis for determining an effect size of .5 in a difference-in-means test') +
  theme_bw()

###############
### THE END ###
###############