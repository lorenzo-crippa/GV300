###################################################################################################
# Project:                 GV300 - LABa02 sessions - Problem Set 3 answers                        #
#                                                                                                 #
# University:              University of Essex                                                    #
#                                                                                                 #
# Programmer:              Lorenzo Crippa                                                         #
#                                                                                                 #
###################################################################################################

library(psych)

# Lorenzo Essex
setwd("C:/Users/lc19059/Dropbox/Shared_Essex/GTA/GV300")

# Lorenzo Mac
#setwd("/Users/Lorenzo/Dropbox/Shared_Essex/GTA/GV300")

# clear all
rm(list=ls())

# exercise 2
set.seed(1111)
x <- rbinom(5000, p = .3, size = 12)

# PMF:
pdf("slides/pictures/PMF.pdf")
barplot(height = table(factor(x))/length(x),
        ylab = "frequency",
        xlab = "values",
        main = "PMF of x",
        ylim = c(0, 0.25))
dev.off()

# CDF:
pdf("slides/pictures/CDF.pdf")
barplot(height = cumsum(table(factor(x)))/length(x),
        ylab = "cumulative frequency",
        xlab = "values",
        main = "CDF of x",
        ylim = c(0, 1))
dev.off()

# exercise 4
df <- data.frame(xs = c(11,4,2,10,8,13,8,12,NA,NA),
                 xl = c(14,2,2,6,12,2,4,1,1,7))

describe(df, na.rm = T)
var(df$xs, na.rm = T)
var(df$xl, na.rm = T)

pdf("slides/pictures/problem_set3_boxplot_xs.pdf")
boxplot(df$xs, frame = F, ylab = "xs", main = "boxplot of xs")
dev.off()

pdf("slides/pictures/problem_set3_boxplot_xl.pdf")
boxplot(df$xl, frame = F, ylab = "xl", main = "boxplot of xl")
dev.off()

# t-test
tstat <- abs(mean(df$xl, na.rm = T)-mean(df$xs, na.rm = T)) / 
  (sqrt((((length(df$xl)-2-1)*var(df$xl, na.rm = T)) + ((length(df$xs)-1)*var(df$xs, na.rm = T)))/
  (length(df$xl)-2+length(df$xs)-2))*sqrt((1/(length(df$xl)-2)+(1/length(df$xs)))))

# test
1 - pt(tstat, df = 16)

t.test(x = df$xl, y = df$xs, alternative = "less", conf.level = .95)

x <- c(df$xl,df$xs)
large.city <- c(rep(1,length(df$xl)),rep(0,length(df$xs)))
t.test(x~large.city)
t.test(x~large.city, var.equal=TRUE) #var.equal=TRUE switches of R's default adjustment of 
# the degrees of freedom by taking into account the variance in the two samples. 

# exercise 6
# a
mat <- matrix(rep(NA, 5000), nrow = 50, ncol = 100)
for (i in 1:100) {
  set.seed(i+142) # every iteration a different seed
  mat[,i] <- rchisq(n = 50, df = 50)
}

mean <- array(data = NA, dim = 100)
for (i in 1:100) {
  mean[i] <- mean(mat[,i])
}

pdf("slides/pictures/hist_chisq.pdf")
hist(mean, col = "gray")
dev.off()

# b and c

# let's program a generic function
chi2histogram <- function(n, df = 50, m){
  mean <- rep(NA, m)
  for (i in 1:m) {
    mean[i] <- mean(rchisq(n, df, ncp = 0))
  }
  hist(mean, col = "gray", 
       main = paste("Histogram of mean\n", n, "observations.", m, "variables"))
}


pdf("slides/pictures/obs50var100.pdf")
chi2histogram(n = 50, m = 100)
dev.off()

pdf("slides/pictures/obs50var1000.pdf")
chi2histogram(n = 50, m = 1000)
dev.off()

pdf("slides/pictures/obs100var100.pdf")
chi2histogram(n = 100, m = 100)
dev.off()

pdf("slides/pictures/obs100var1000.pdf")
chi2histogram(n = 100, m = 1000)
dev.off()

pdf("slides/pictures/obs1000var100.pdf")
chi2histogram(n = 1000, m = 100)
dev.off()

pdf("slides/pictures/obs1000var1000.pdf")
chi2histogram(n = 1000, m = 1000)
dev.off()

pdf("slides/pictures/obs10000var100.pdf")
chi2histogram(n = 10000, m = 100)
dev.off()

pdf("slides/pictures/obs10000var1000.pdf")
chi2histogram(n = 10000, m = 1000)
dev.off()

###############
### THE END ###
###############