###################################################################################################
# Project:                 GV300 - LABa02 sessions                                                #
#                                                                                                 #
# University:              University of Essex                                                    #
#                                                                                                 #
# Programmer:              Lorenzo Crippa                                                         #
#                                                                                                 #
# Week:                    Week 24 (Monday 9th of March, 2020)                                    #
###################################################################################################

# clear
rm(list=ls())

# Lorenzo Essex
setwd("C:/Users/lc19059/Dropbox/Shared_Essex/GTA/GV300")

# Lorenzo Macbook
#setwd("/Users/Lorenzo/Dropbox/Shared_Essex/GTA/GV300")

#############
library(tidyverse)
library(haven)
library(foreign)
library(plm)
library(sandwich)
library(texreg)
library(stargazer)
library(psych)
library(lmtest)
#############

######################
#### Problem set 7 ###
######################

##############
# Question 1 #
##############

# data import
data <- read.csv("data/indicators.csv")

# (a)
# control and treatment indicator
data$treatment[data$yearJoinEU == 2004] <- 1 # treatment group
data$treatment[data$yearJoinEU != 2004] <- 0 # control group

# add also indicators for pre-post intervention and the interaction with the previous one
data$intervention <- ifelse(data$year >= 2004, 1, 0)

# countries in the treatment group
unique(data$country[data$treatment == 1])

# Czech Republic  Estonia         Hungary         Latvia          Lithuania       Poland          Slovak Republic
# Slovenia    

# countries in the control group
unique(data$country[data$treatment == 0])

# Albania        Armenia        Bulgaria       Croatia        Georgia        Kosovo         Macedonia, FYR Moldova       
# Montenegro     Serbia         Ukraine     

# (b)
# compute means of GDP per capita by group pre and after intervention
GDPcapita.treat.pre <- mean(data$GDPPerCapita[data$treatment == 1 & data$intervention == 0], na.rm = T)
GDPcapita.treat.pos <- mean(data$GDPPerCapita[data$treatment == 1 & data$intervention == 1], na.rm = T)

GDPcapita.contr.pre <- mean(data$GDPPerCapita[data$treatment == 0 & data$intervention == 0], na.rm = T)
GDPcapita.contr.pos <- mean(data$GDPPerCapita[data$treatment == 0 & data$intervention == 1], na.rm = T)

# compute DiD
DiD <- (GDPcapita.treat.pos - GDPcapita.treat.pre) - (GDPcapita.contr.pos - GDPcapita.contr.pre)
DiD

# save them in a dataframe to plot them
df <- data %>% group_by(treatment, intervention) %>% summarise(meanGDPcap = mean(GDPPerCapita, na.rm = T)) %>%
  ungroup() %>% mutate(
    treatment = recode(treatment,`1`="EU member since 2004",`0`="No EU member"),
    intervention = factor(recode(intervention,`0` = "Before accession",`1`="After accession"),
                          levels = c("Before accession", "After accession"))
    )

ggplot(df, aes(x = intervention, y = meanGDPcap, group = treatment)) + 
  geom_path(aes(colour = treatment, size = I(1))) + 
  geom_point(aes(colour = treatment)) + scale_color_discrete("group") +
  ylab("GDP per capita") + theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank(), axis.title.x = element_blank())
ggsave("slides/pictures/week_24_Q1_b.pdf", device = "pdf", width = 617/72, height = 504/72, units = "in", dpi = 72)

# (c)
# means of control and treatment groups over year
df.con <- filter(data, treatment == 0) %>% select(year, treatment, GDPPerCapita)
df.tre <- filter(data, treatment == 1) %>% select(year, treatment, GDPPerCapita)

df.con <- group_by(df.con, year) %>% summarise(
  GDPcapita = mean(GDPPerCapita, na.rm = T),
  data = "control")
df.tre <- group_by(df.tre, year) %>% summarise(
  GDPcapita = mean(GDPPerCapita, na.rm = T),
  data = "treatment")

# now we need to add the counterfactual line.
treGDP2004 = mean(data$GDPPerCapita[data$year==2004 & data$treatment == 1])
conGDP2004 = mean(data$GDPPerCapita[data$year==2004 & data$treatment == 0])

diff.GDP <- treGDP2004 - conGDP2004

# the counterfactual will be precisely the treatment group if it had not received the treatment:
df.count <- df.con
df.count$GDPcapita<- df.count$GDPcapita + diff.GDP
df.count$GDPcapita[df.count$year < 2004] <- NA
df.count$data <- "counterfactual"

# let's merge them all into the same data frame
df.means <- full_join(df.con, df.tre)
df.means <- full_join(df.means, df.count)

# plot them
ggplot(df.means, aes(x = year, y = GDPcapita, group = data)) + geom_point(aes(colour = factor(data))) +
  geom_line(aes(colour = factor(data))) + geom_vline(xintercept = 2004) + theme_minimal() +
  ylab("GDP per capita") + theme(legend.position = "bottom", legend.title = element_blank())
ggsave("slides/pictures/week_24_Q1_c.pdf", device = "pdf", width = 617/72, height = 504/72, units = "in", dpi = 72)

# (d)
d <- lm(GDPPerCapita ~ intervention*treatment, data = data)
summary(d)

# (e)
vcov.cluster <- vcovHC(d, type = "HC1", cluster = "state")
coeftest(d, vcov.cluster)

# we can save the clustered SEs as usual, and the pvalues with a short loop, 
# to then display the results in a common table
cluster.SE <- sqrt(diag(vcov.cluster))
cluster.pval <- rep(NA, length(cluster.SE)) 
for (i in 1:length(cluster.pval)) { 
  tstat <- abs(d$coefficients[i]/cluster.SE[i]) 
  names(tstat) <- NULL 
  cluster.pval[i] <- (1-pt(tstat, df = nobs(d)-1))*2
}

# (f)
# for instance we might want to include exports as a share of GDP, which might confound 
# both the probability of joining the EU (receiving the treatment) and the GDP per capita
f1 <- lm(GDPPerCapita ~ intervention*treatment + exportsShareGDP, data = data)
summary(f1)
coeftest(f1, vcovHC(f1, type = "HC1", cluster = "state"))

# save SEs and pvalues again:
cluster.SE.f1 <- vcovHC(f1, type = "HC1", cluster = "state") %>% diag() %>% sqrt()
cluster.pval.f1 <- rep(NA, length(cluster.SE.f1)) 
for (i in 1:length(cluster.pval.f1)) { 
  tstat <- abs(f1$coefficients[i]/cluster.SE.f1[i]) 
  names(tstat) <- NULL 
  cluster.pval.f1[i] <- (1-pt(tstat, df = nobs(f1)-1))*2
}

# or we might want to control for joining the EU after 2004:
f2 <- lm(GDPPerCapita ~ intervention*treatment + yearJoinEU, data = data)
summary(f2)
coeftest(f2, vcovHC(f2, type = "HC1", cluster = "state"))

# save SEs and pvalues again:
cluster.SE.f2 <- vcovHC(f2, type = "HC1", cluster = "state") %>% diag() %>% sqrt()
cluster.pval.f2 <- rep(NA, length(cluster.SE.f2)) 
for (i in 1:length(cluster.pval.f2)) { 
  tstat <- abs(f2$coefficients[i]/cluster.SE.f2[i]) 
  names(tstat) <- NULL 
  cluster.pval.f2[i] <- (1-pt(tstat, df = nobs(f2)-1))*2
}

# print results for d, e, f:
screenreg(list(d, d, f1, f2), custom.model.names = c("(d)", "(e)", "(f)", "(f)"), stars = c(.01, .05, .1),
          override.se = list(rep(NA, length(d$coefficients)), cluster.SE, cluster.SE.f1, cluster.SE.f2),
          override.pvalues = list(rep(NA, length(d$coefficients)), cluster.pval, cluster.pval.f1, cluster.pval.f2))

# and LaTeX
texreg(list(d, d, f1, f2), custom.model.names = c("(d)", "(e)", "(f)", "(f)"), stars = c(.01, .05, .1),
       override.se = list(rep(NA, length(d$coefficients)), cluster.SE, cluster.SE.f1, cluster.SE.f2),
       override.pvalues = list(rep(NA, length(d$coefficients)), cluster.pval, cluster.pval.f1, cluster.pval.f2))

##############
# Question 2 #
##############

# clear
rm(list = ls())

# import data
pd <- read.dta("data/anesByState.dta")

# (a)
# summary statistics from psych package
describe(pd)
# or:
cbind(pd$FTM, pd$white, pd$poor, pd$turnout, pd$voteDem, pd$dem) %>% stargazer(summary = T, type = "text")

# and for LaTeX
cbind(pd$FTM, pd$white, pd$poor, pd$turnout, pd$voteDem, pd$dem) %>% stargazer(summary = T, type = "latex")

# plots
# just usual distributions:
pd %>% ggplot(aes(x = FTM)) + geom_histogram(aes(y = ..density..), fill = "gray86", colour = "black") + 
  geom_density(aes(alpha = I(.3)), fill = "deepskyblue3") + theme_minimal() + xlab("feeling thermometer")

# or you can show different distributions by states
pd %>% ggplot(aes(x = state, y = white, group = state)) + geom_boxplot(aes(fill = I("gray86"))) + 
  theme_minimal() + ylab("share of white people")
ggsave("slides/pictures/week_24_Q2_a1.pdf", device = "pdf", width = 617/72, height = 504/72, units = "in", dpi = 72)

# or you can think about weirder things such as the distribution of FTM conditional on "poor" grouped by state:
pd %>% ggplot(aes(x = poor, y = FTM, group = state)) + geom_boxplot(aes(fill = state, alpha = I(.6))) + 
  theme_minimal() + xlab("share of population under poverty line") + ylab("feeling thermometer") +
  theme(legend.position = "bottom", legend.title = element_blank())

# FTM by state over time:
pd %>% filter(year >= 1968) %>% # let's not plot observations before 1968 (they are NAs)
  ggplot(aes(x = year, y = FTM, group = state)) + geom_line(aes(col = state)) + 
  theme_minimal() + ylab("feeling thermometer") + theme(legend.position = "bottom", legend.title = element_blank())

# or alternatively we can have it in different panels and add the overall mean over time:
p <- pd %>% filter(year >= 1968) %>% group_by(year) %>% mutate(overallMean = mean(FTM, na.rm = T))
p %>% ggplot(aes(x = year, y = FTM, group = state)) + geom_line() + facet_wrap(~state) +
  geom_line(aes(x = year, y = overallMean), color = "red") +
  theme_minimal() + ylab("feeling thermometer")
ggsave("slides/pictures/week_24_Q2_a2.pdf", device = "pdf", width = 617/72, height = 504/72, units = "in", dpi = 72)

# some variation between units but similar to the overall mean over time

# (b)
overall.means <- pd %>% summarise_all(funs(mean), na.rm = TRUE)
group.means <- pd %>% group_by(state) %>% summarise_all(funs(g.mean = mean),
                                                          na.rm = TRUE)
d.group.means <- left_join(pd,group.means,by ='state')

within.variation <- d.group.means %>%
  mutate(
    var.year = year - year_g.mean + overall.means$year,
    var.FTM = FTM - FTM_g.mean + overall.means$FTM,
    var.white = white - white_g.mean + overall.means$white,
    var.poor = poor - poor_g.mean + overall.means$poor,
    var.turnout = turnout - turnout_g.mean + overall.means$turnout,
    var.voteDem = voteDem - voteDem_g.mean + overall.means$voteDem,
    var.dem = dem - dem_g.mean + overall.means$dem
  ) %>%
  select(var.year:var.dem) %>%
  summarise_all(funs(sd), na.rm = TRUE)

overall.variation <- pd %>% select(year,FTM:dem) %>% summarise_all(funs(sd),
                                                                     na.rm = TRUE)
between.variation <- group.means %>% select(year_g.mean:dem_g.mean) %>%
  summarise_all(funs(sd), na.rm = TRUE)

# print results
overall.variation
within.variation
between.variation

# (c)
# then run the model
model.c <- plm(FTM ~ white + poor + dem + turnout, data = pd, model = "pooling", index = c("state", "year"))
summary(model.c)
# betas could be biased cause we are not controlling for between-unit variations (fixed or random effect),
# moreover we are not clustering the SEs, which means they might be biased due to serial correlation

# (d)
coeftest(model.c, vcovHC(model.c, type = "HC1", cluster = "group"))
# get SEs and pvalues
SE.d <- vcovHC(model.c, type = "HC1", cluster = "group") %>% diag() %>% sqrt()
pval.d <- rep(NA, length(SE.d)) 
for (i in 1:length(pval.d)) { 
  tstat <- abs(model.c$coefficients[i]/SE.d[i]) 
  names(tstat) <- NULL 
  pval.d[i] <- (1-pt(tstat, df = nobs(model.c)-1))*2
}

# (e)
# first generate the factor version of "state"
pd$state.fac <- factor(pd$state)

# we can introduce the dummies manually in a pooled OLS model:
model.e <- lm(FTM ~ voteDem + dem + poor + white + state.fac, data = pd)
summary(model.e) # notice that this model has non-robust SEs, we should estimate clustered ones!

# standard errors and pvalues, let's save them:
SE.e <- vcovHC(model.e, type = "HC1", cluster = "group") %>% diag() %>% sqrt()
pval.e <- rep(NA, length(SE.e)) 
for (i in 1:length(pval.e)) { 
  tstat <- abs(model.e$coefficients[i]/SE.e[i]) 
  names(tstat) <- NULL 
  pval.e[i] <- (1-pt(tstat, df = nobs(model.e)-1))*2
}

# (f)
# or (better) we use entity demeaned model thanks to plm() function:
model.f <- plm(FTM ~ voteDem + dem + poor + white, data = pd, index = c("state", "year"), model = "within")
summary(model.f)

# and then we can compute and save robust SEs and pvalues:
coeftest(model.f, vcovHC(model.f, type = "HC1", cluster = "group"))
# get SEs and pvalues
SE.f <- vcovHC(model.f, type = "HC1", cluster = "group") %>% diag() %>% sqrt()
pval.f <- rep(NA, length(SE.f)) 
for (i in 1:length(pval.f)) { 
  tstat <- abs(model.f$coefficients[i]/SE.f[i]) 
  names(tstat) <- NULL 
  pval.f[i] <- (1-pt(tstat, df = nobs(model.f)-1))*2
}

# (g)
# introduce random effect
model.g <- plm(FTM ~ voteDem + dem + poor + white, data = pd, index = c("state", "year"), model = "random")
summary(model.g)

# and then we can compute and save robust SEs and pvalues:
coeftest(model.g, vcovHC(model.g, type = "HC1", cluster = "group"))
# get SEs and pvalues
SE.g <- vcovHC(model.g, type = "HC1", cluster = "group") %>% diag() %>% sqrt()
pval.g <- rep(NA, length(SE.g)) 
for (i in 1:length(pval.g)) { 
  tstat <- abs(model.g$coefficients[i]/SE.g[i]) 
  names(tstat) <- NULL 
  pval.g[i] <- (1-pt(tstat, df = nobs(model.g)-1))*2
}

# print regression results
screenreg(list(model.c, model.c, model.e, model.f, model.g),
          custom.model.names = c("(c)", "(d)", "(e)", "(f)", "(g)"),
          override.se = list(rep(NA, length(model.g$coefficients)), SE.d, SE.e, SE.f, SE.g),
          override.pvalues = list(rep(NA, length(model.g$coefficients)), pval.d, pval.e, pval.f, pval.g),
          stars = c(0.01, 0.05, 0.1), include.rmse = F, omit.coef = "state"
          )

# latex:
texreg(list(model.c, model.c, model.e, model.f, model.g),
       custom.model.names = c("(c)", "(d)", "(e)", "(f)", "(g)"),
       override.se = list(rep(NA, length(model.g$coefficients)), SE.d, SE.e, SE.f, SE.g),
       override.pvalues = list(rep(NA, length(model.g$coefficients)), pval.d, pval.e, pval.f, pval.g),
       stars = c(0.01, 0.05, 0.1), include.rmse = F, omit.coef = "state"
)
# (h)
# run a Hausman test for deciding between a FE (model f) and RE (model g)
phtest(model.f, model.g) # you should probably go for a fixed effect here



#############
#### MLE ###
#############

# clear
rm(list=ls())

# Binomial likelihood function with y outcome, n trials, pi probability of success
binomial.loglikelihood <- function(y, n, pi) {
  loglikelihood <- y*log(pi) + (n-y)*log(1-pi)
  return(loglikelihood)
}

binomial.loglikelihood(2,3, 0.1)
binomial.loglikelihood(10, 100, 0.23)

# Use the optim function to optimize the likelihood with respect to a parameter
# given fixed the data (the other two parameters in this case, say y, n)
test <- optim(par = c(.5), # initial value for the parameters to be optimized over (iteration)
              fn = binomial.loglikelihood, # the function to optimize (log-likelihood)
              method = "BFGS", # what method to use in order to optimize
              hessian = T, # return Hessian matrix
              control = list(fnscale = -1), # maximize instead of minimize
              y = 43, n = 100 # the data
)

# look at what we get:
test$par # estimate of the parameter: 
# starting from our data (100 attempts, 43 heads), the best estimate of p is 0.43 of course
test$value # maximized value of the loglikelihood function
test$counts # how often optim called the function and gradient
test$convergence # 0 means successful convergence to a maximum
test$message # any message?
test$hessian # the Hessian matrix (here's only one value since there's only one parameter...
# remember what the Hessian matrix is... var/cov matrix of 2nd derivatives...)

# std error of our parameter? 
# It's the squared root of the diagonal of the inverse of my Hessian matrix
sqrt(diag(solve(-test$hessian)))

# we have only one parameter: we can plot the likelihood function. Up until 2 parameters you can 
# plot them in 3D (2 parameters + (log)likelihood value = 3 dimensions). 3 parameters make
# the likelihood function already impossible to plot (unfortunately we can only visualize 3 dimensions)

ruler <-  seq(0,1, 0.01) # the sequence of 101 possible values of pi (between 0 and 1)
loglikelihood <- binomial.loglikelihood(ruler, y = 43, n = 100) # apply the function 101 times

loglikelihood

plot(ruler, loglikelihood, type = "l", lwd = 2, col = "red", 
     xlab = "probability", ylab = "log-likelihood", ylim = c(-300, -60), 
     # you need these limits as the min and max of loglikelihood are -Inf
     main = "Log-likelihood for binomial model"
)
abline(v = test$par)

ggplot(as.data.frame(ruler, loglikelihood),
       aes(x = ruler, y = loglikelihood)) +
  geom_line(colour = "#CC0000", size = 1) + geom_vline(xintercept = test$par) +
  xlab("probability") + ylab("log-likelihood") + ylim(c(-300, -60)) + theme_minimal()
ggsave("slides/pictures/week_24_ll_binomial.pdf", device = "pdf", width = 617/72, height = 504/72, units = "in", dpi = 72)

# Now try to do the same for a linear model
# suppose your DGP is represented by:
set.seed(123)
x <- runif(1000, min = 0, max = 1)
x <- exp(x) # let's transform x somehow, it can have whatever distribution
y <- 3.2 + 2*x + rnorm(1000, mean = 0, sd = 2) # obtain y from a linear model

stdnorm <- function(z) {
  fi <- (1/sqrt(2*pi))*exp((-1/2)*(z^2))
}

linear.loglikelihood <- function(b,a,x,y) {
  expr <- rep(NA, length(y))
  for (i in 1:length(y)) {
    expr[i] <- log((1/var(y))*stdnorm((y[i]-(a+b*x[i]))/var(y)))
  }
  ll <- sum(expr)
  return(ll)
}

linear.loglikelihood(2, 3.2, x,y)
linear.loglikelihood(4, 3, x,y)
linear.loglikelihood(11, 10, x,y)
linear.loglikelihood(102301231, 123123123141, x,y)

# ok it works
# now optimize it with respect to b (which should be 2)
test <- optim(par = c(.5),
              fn = linear.loglikelihood,
              method = "BFGS",
              hessian = T,
              control = list(fnscale = -1),
              x = x, y = y, a = 3.2)
test$par
test$value
test$counts
test$convergence
test$message
test$hessian

# standard error:
sqrt(diag(solve(-test$hessian)))

# so we can compute our z test:
zstat <- test$par / (sqrt(diag(solve(-test$hessian))))
pval <- (1-pnorm(zstat))/2
pval # 0, we do not reject the null hypothesis

# plot
ruler <-  seq(-100,100, 1) 
loglikelihood <- rep(NA, length(ruler))
for (i in 1:length(ruler)) {
  loglikelihood[i] <- linear.loglikelihood(ruler[i], 3.2, x,y)
}

plot(ruler, loglikelihood, type = "l", lwd = 2, col = "red", 
     xlab = "beta", ylab = "log-likelihood", 
     main = "Log-likelihood for linear model"
)
abline(v = test$par)

# optimize with respect to a (which should be 3.2)
test <- optim(par = c(.5),
              fn = linear.loglikelihood,
              method = "BFGS",
              hessian = T,
              control = list(fnscale = -1),
              x = x, y = y, b = 2)
test$par
test$value
test$counts
test$convergence
test$message
test$hessian

# plot
ruler <-  seq(-100,100, 1) 
loglikelihood <- rep(NA, length(ruler))
for (i in 1:length(ruler)) {
  loglikelihood[i] <- linear.loglikelihood(a = ruler[i], b = 2, x,y)
}

plot(ruler, loglikelihood, type = "l", lwd = 2, col = "red", 
     xlab = "alpha", ylab = "log-likelihood", 
     main = "Log-likelihood for linear model"
)
abline(v = test$par)


################################
# optimize with respect to both a and b
# re-program the function such as:
linear.loglikelihood <- function(p,x,y) {
  a <- p[1]
  b <- p[2]
  expr <- rep(NA, length(y))
  for (i in 1:length(y)) {
    expr[i] <- log((1/var(y))*stdnorm((y[i]-(a+b*x[i]))/var(y)))
  }
  ll <- sum(expr)
  return(ll)
}

linear.loglikelihood(c(1,2),x,y)
linear.loglikelihood(c(3,5),x,y)
linear.loglikelihood(c(1,4),x,y)
linear.loglikelihood(c(3,2),x,y)

test <- optim(par = c(.5, .3),
              fn = linear.loglikelihood,
              method = "BFGS",
              hessian = T,
              control = list(fnscale = -1),
              x = x, y = y)
test$par # fairly close

# standard errors:
SE <- sqrt(diag(solve(-test$hessian)))
SE

# run a z test:
pval <- rep(NA, length(test$par))
for (i in 1:length(pval)) {
  zstat <- test$par[i]/SE[i]
  pval[i] <- (1-pnorm(zstat))/2
}
pval # ok, reject both hypotheses that population parameters = 0

# compare to OLS results:
summary(lm(y ~ x)) # results are very very similar

# in order to plot the result we need to reprogram the loglikelihood function so as to take two different paramters
linear.loglikelihood <- function(a,b,x=x,y=y) {
  expr <- rep(NA, length(y))
  for (i in 1:length(y)) {
    expr[i] <- log((1/var(y))*stdnorm((y[i]-(a+b*x[i]))/var(y)))
  }
  ll <- sum(expr)
  return(ll)
}

linear.loglikelihood(1,2,x,y)
linear.loglikelihood(3,2,x,y)

# ranges of possible values for a and b
a <- seq(-10, 10, by = 1)
b <- seq(-10, 10, by = 1)

# corresponding log-likelihood for each combination of a and b:
z <- matrix(nrow = length(a), ncol = length(b), rep(NA, length(a)*length(b)))

# implement function on sequence of a and b
for (i in 1:length(a)) {
  for (j in 1:length(b)) {
    z[i,j] <- linear.loglikelihood(a[i],b[j],x,y)
  }
}

# prepare data to be plotted:
mat <- z
min <- min(mat, na.rm = T)

# persp() is a function we can use for 3D plots
persp(x = a, y = b, z = z,
      zlim = c((min-5000), test$value),
      theta = 240, phi = 20,
      col = "aquamarine3",
      r = 3,
      d = 10,
      ltheta = 15,
      xlab = "alpha",
      ylab = "beta",
      zlab = "log-likelihood")
dev.copy("slides/pictures/week_24_ll_linear.pdf", device = pdf)
dev.off()

#####################################
# we can also make the 3D plot turn #
#####################################
library(magick)

frames <- 1:360
# generate 360 frames
for(t in frames) {
  # open a PNG plotting device
  png(file = paste0("animated/frame", t, ".png"), height = 7, width = 7, 
      units = "in", res = 500)
  
  # generate the frame:
  persp(x = a, y = b, z = z,
        zlim = c((min-5000), test$value),
        theta = t,
        col = "aquamarine3",
        r = 3,
        d = 10,
        ltheta = 15,
        xlab = "alpha",
        ylab = "beta",
        zlab = "log-likelihood")
  # close plotting device
  dev.off()
}

# and now use magick to merge the frames:
img <- image_read(path = "animated/frame1.png")

# watch out: the following loop runs terribly slow (each iteration adds one more frame to all the previous ones)
for(t in unique(frames)[-1]){
  img0 <- image_read(path = paste0("animated/frame",t,".png"))
  img <- c(img, img0)
  message(t)
}

img1 <- image_scale(image = img, geometry = "720x720")

ani0 <- image_animate(image = img1, fps = 10)
image_write(image = ani0, path = "output/week_24_ll_linear_both_animated.gif")


###############
### THE END ###
###############