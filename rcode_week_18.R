###################################################################################################
# Project:                 GV300 - LABa02 sessions                                                #
#                                                                                                 #
# University:              University of Essex                                                    #
#                                                                                                 #
# Programmer:              Dominik Duell. Edited by Lorenzo Crippa                                #
#                                                                                                 #
# Week:                    Week 18 (Monday 27th of January, 2020)                                 #
###################################################################################################

# clear
rm(list=ls())

# Lorenzo Essex
setwd("C:/Users/lc19059/Dropbox/Shared_Essex/GTA/GV300")

# Lorenzo Macbook
#setwd("/Users/Lorenzo/Dropbox/Shared_Essex/GTA/GV300")

#################
library(dplyr)
library(ggplot2)
library(foreign)
library(MASS)
library(sandwich)
library(lmtest)
library(estimatr)
#################

# Data generation, model set-up
set.seed(01010)
x <- rep(1:100,2) # some values for our independent variable
a <- 0 # the intercept of our linear model
b <- 1 # the slope of our linear model (bivariate)

# Let's create some heteroskedasticity
sigma2 <- x^1.3 # we generate a non-constant variance
error_term <- rnorm(x, mean = 0, sd = sqrt(sigma2)) # variance of the error term changes!
y = a + b*x + error_term # this is our DGP

##############################
# Run the linear model
model <- lm(y ~ x)
residuals <- model$residuals
summary(model)

# we can do the same using matrix algebra, we would get the same estimates. Let's show it.
# first create the Xmat matrix with all our indep variables (a vector of 1 and x, in this case):
Xmat <- matrix(cbind(rep(1, 200), x), ncol = 2, byrow = F)
# then compute OLS estimates:
beta_hat <- solve(t(Xmat) %*% Xmat) %*% (t(Xmat) %*% y) # the %*% symbol is matrix product
beta_hat

# in order to get manually the estimates of the standard errors, we first need to get the variance-cov matrix.

# vcov matrix: first we need the estimate of the error variance, or mean squared error (MSE),
# which we obtain starting from the residuals. Thus we need the predictions of our dependent variable
u <- (Xmat[,1]*beta_hat[1] + Xmat[,2]*beta_hat[2]) - y # the 1st term is the prediction of y, subtract the observed y
sigma_sq <- (t(u) %*% u)/(nrow(Xmat) - ncol(Xmat)) # this is (supposed to be) a number, a constant!
vcovar <- sigma_sq[1,1] * solve(t(Xmat)%*%Xmat)
vcovar # this is the variance-covariance matrix, obtained only with matrix algebra, assuming homoskedasticity. 
# you can check that it's identical to:
vcov(model)

# the squared root of its diagonal will be the estimated standard errors of our parameters:
sqrt(diag(vcovar))
beta_hat

# you can check that it's all identical to what we have obtained using the lm() function for OLS:
summary(model)

# OK, our matrix algebra works fine and gives us what is supposed to (OLS)

# But we know we have a problem of heteroskedasticity! 
# Let's visualise the problem we have created earlier
qplot(x,y) + geom_smooth(method = "lm", se = F, col = I("red"))
ggsave("slides/pictures/week_18_het_1.pdf", device = "pdf")
# look at the variance of your y conditional on x: it increases!

# see also:
qplot(x, residuals) + geom_hline(yintercept = 0, col = I("red")) + ylab("residuals")
ggsave("slides/pictures/week_18_het_2.pdf", device = "pdf")
# the variance of the residuals conditional on x increases too. This is a sign that our errors are heteroskedastic.

# Let's start with OLS and see what results we get assuming homoskedasticity
summary(model)

# Remember the true values of intercept and slope: 0 and 1 respectively. The OLS results we obtained 
# show that slope and intercept are rather correctly estimated, they do not look very biased.

# It is clear from the above regression that the presence of heteroskedascticity doesn't affect consistency 
# OF THE ESTIMATES FOR OUR PARAMETERS (that is, no bias in large samples).

# Remember that the OLS standard error estimates are the squared roots of the diagonal of
# the covariance matrix, if we can assume homoskedasticity. vcov matrix is:
vcov(model)
# or the one we have computed manually:
vcovar

# estimates of the standard errors is the squared root of the diagonal of the vcov matrix:
sqrt(diag(vcov(model)))
# Differently from the estimates of the parameters, S.E. are biased if we have heteroskedasticity

# Robust Standard Error (Heteroskedasticity-consistent standard errors) need to be used.
# The mechanism of HC estimator is hard to summarize. See slides for matrix algebra.
# In stata, there are four different HC estimators, while in R there are 6. 
# But all of these estimators are ways to find an asymptotically 
# consistent analogue of the covariance matrix: X'SigmaX. It is an improvement on the OLS, rather than GLS.

# Let's look at the improved, more conservative standard error estimates using the vcovHC() function:
sqrt(diag(vcovHC(model, type = "HC")))

# you can also directly test your hypotheses on the coefficients using the new VCOV estimators of S.E.:
hc <- vcovHC(model, type = "HC")
coeftest(model, vcov = hc)

# given the definition of White-robust estimators (see slides), we 
# can also compute them manually using matrix algebra. First compute a robust vcov matrix
vcov_rob <- nrow(Xmat) / (nrow(Xmat)- ncol(Xmat)) * solve(t(Xmat) %*% Xmat) %*% 
  t(Xmat) %*% diag(u^2) %*% Xmat %*% solve(t(Xmat) %*% Xmat)

# and the robust estimators for the standard errors will be:
sqrt(diag(vcov_rob))

# Alternatively, look at lm_robust() in the estimatr package:
lm_robust(y ~ x, se_type = "HC0") # this is precisely what we have estimated with vcovHC()

# other types of robust estimators:
lm_robust(y ~ x, se_type = "HC1") # this is precisely what we have estimated with matrix algebra
lm_robust(y ~ x, se_type = "HC2")
lm_robust(y ~ x, se_type = "HC3")

# Detection of heteroskedasticity
# There are two dominant ways to detect the heteroskedasticity. One is to plot the residuals,  
# and the second is to use the popular "White" test. How do these two approaches perform in our case?

# Visualization (see plots above)

# Breusch-Pagan test
bptest(y ~ x)
# we reject the null-hypothesis if we have heteroskedasticity 
# (as in our case, where we strongly reject the null)

####################################################
# Feasible GLS (FGLS): 
# We use fitted values from OLS (which we know is consistent) as weight for creating the variance matrix 
# in stage 1, then fill that into a stage 2 regression. Here, it is enough to use the square of residuals.
fgls <- lm(y ~ x, weights = 1/model$fitted.values^2) # see that we specify the weights

summary(fgls)
sqrt(diag(vcov(fgls)))

# Iterative Feasible GLS (IFGLS): We can iterate through stage 1 and 2 over
# and over again until we converge on a variance and coefficient estimate.

# generate a list of 200 empty vectors
test <- vector('list', 200)
test[[1]] <- list(coef0 = coef(model), res0 = model$residuals) # save coefficients and residuals in the first vector

# program the function that we'll need:
wls_func_lm <- function(obj){
  weight <- obj[['res0']]^2 # save squared residuals 
  lm_reg <- lm(y ~ x, weights = 1/weight)
  lm_reg_res <- lm(lm_reg$residuals^2 ~ x)
  return(list(coef0 = coef(lm_reg), res0 = lm_reg_res$fitted.values)) 
}

for (i in 1:200){
  test[[i+1]] <- wls_func_lm(test[[i]])
  dif <- sum(test[[i+1]][['coef0']] - test[[i]][['coef0']])^2
  cat(i, test[[i]][['coef0']], dif , '\n')
}
# This loop prints the coefficient on intercept and x as well as the residual. 
# Once we see convergence, that's the GLS estimate of our coefficients. 
# In this example, its still quite jumpy even after 200 iterations. 
# We have giant heteroskedasticity here

###############
### THE END ###
###############