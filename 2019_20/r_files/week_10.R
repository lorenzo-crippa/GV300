###################################################################################################
# Project:                 GV300 - LABa02 sessions                                                #
#                                                                                                 #
# University:              University of Essex                                                    #
#                                                                                                 #
# Programmer:              Lorenzo Crippa                                                         #
#                                                                                                 #
# Week:                    Week 10 (Monday 2nd of December, 2019)                                 #
###################################################################################################

# clear
rm(list=ls())

# Lorenzo Essex
setwd("C:/Users/lc19059/Dropbox/Shared_Essex/GTA/GV300")

# Lorenzo Macbook
#setwd("/Users/Lorenzo/Dropbox/Shared_Essex/GTA/GV300")

#################
library(ggplot2)
library(stargazer)
library(sandwich)
library(lmtest)
#################

# import data
Greene <- read.csv("data/Greene_data.csv")

Greene$SIGNED <- as.factor(Greene$SIGNED)
Greene$HOUSE <- as.factor(Greene$HOUSE)

cor(Greene$PRICE, Greene$HEIGHT)
cor(Greene$PRICE, Greene$WIDTH)

cor(x = matrix(c(Greene$PRICE, Greene$HEIGHT, Greene$WIDTH),
               ncol = 3, byrow = F), 
    use = "pairwise.complete.obs")

# bivariate plots:
ggplot(Greene, aes(x = HEIGHT, y = PRICE)) + geom_point() + xlab("height") + ylab("price")
ggsave("slides/pictures/week_10_scatter.pdf", device = "pdf")

ggplot(Greene, aes(x = WIDTH, y = PRICE)) + geom_point() + xlab("width") + ylab("price")
ggsave("slides/pictures/week_10_scatter2.pdf", device = "pdf")

# not only scatterplots!
ggplot(Greene, aes(x = SIGNED, y = PRICE)) + geom_boxplot() + xlab("signed") + ylab("price")
ggsave("slides/pictures/week_10_boxplot.pdf", device = "pdf")

ggplot(Greene, aes(x = HOUSE, y = PRICE)) + geom_boxplot() + xlab("house number") + ylab("price")
ggsave("slides/pictures/week_10_boxplot2.pdf", device = "pdf")

# multivariate plots:
ggplot(Greene, aes(x = WIDTH, y = PRICE, col = SIGNED)) + geom_point() + 
  xlab("width") + ylab("price") + 
  scale_color_discrete("signed",
                       breaks = c(0,1),
                       labels = c("no", "yes"))
ggsave("slides/pictures/week_10_scatter3.pdf", device = "pdf")

ggplot(Greene, aes(x = HEIGHT, y = PRICE, col = HOUSE)) + geom_point() + 
  xlab("height") + ylab("price") + 
  scale_color_discrete("house number")
ggsave("slides/pictures/week_10_scatter4.pdf", device = "pdf")

########
# model
# re-import data (we turned SIGNED and HOUSE into factor variables)
Greene <- read.csv("data/Greene_data.csv")

model <- lm(data = Greene, PRICE ~ HEIGHT + WIDTH + SIGNED + HOUSE)
summary(model)
stargazer(model, type = "text")

# for LaTeX users:
stargazer(model, type = "latex")

# compute OLS using matrix algebra:
Y <- Greene$PRICE

X <- matrix(c(rep(1, length(Greene$PRICE)), 
              Greene$HEIGHT, Greene$WIDTH, Greene$SIGNED, Greene$HOUSE),
            ncol = 5, byrow = F)

OLS <- solve(t(X)%*%X) %*% (t(X) %*% Y)

# compare results:
summary(model)
OLS

# one-tailed t test for our theory.
# Null-hyp: betas are larger than 0

# save the summary of the models' coefficients
X <- summary(model)$coefficients

# t stat for HEIGHT
t.stat.H <- X[2,1] / X[2,2]
1 - pt(t.stat.H, df = 429) # we do not reject the null, for any conventional level of significance

# t stat for WIDTH
t.stat.W <- X[3,1] / X[3,2]
1 - pt(t.stat.W, df = 429) # we do not reject the null, for any conventional level of significance

# post estimation analysis: residuals over fitted values
qplot(x = model$fitted.values, y = model$residuals) + geom_point() + 
  xlab("fitted values") + ylab("residuals") + geom_hline(yintercept = 0, color = c("red"))
ggsave("slides/pictures/week_10_heteroskedasticity.pdf", device = "pdf")
# we clearly have heteroskedasticity

# get robust standard errors using the sandwich and lmtest packages
robust <- vcovHC(model, type = "HC1")
coeftest(model, vcov. = robust)

stargazer(model, model, type = "text",
          se = list(rep(NULL, 5), 
                    c(0.938638, 0.021883, 0.019416, 0.349278, 0.289056)),
          p = list(rep(NULL, 5), 
                   c(8.249e-09, 3.903e-05, 1.680e-08, 1.538e-10, 0.1791)),
          keep.stat = c("n", "adj.rsq", "f"),
          column.labels = c("non-robust", "robust")
          )

# for LaTeX:
stargazer(model, model, type = "latex",
          se = list(rep(NULL, 5), 
                    c(0.938638, 0.021883, 0.019416, 0.349278, 0.289056)),
          p = list(rep(NULL, 5), 
                   c(8.249e-09, 3.903e-05, 1.680e-08, 1.538e-10, 0.1791)),
          keep.stat = c("n", "adj.rsq", "f"),
          column.labels = c("non-robust", "robust")
          )


###############
### THE END ###
###############