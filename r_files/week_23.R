###################################################################################################
# Project:                 GV300 - LABa02 sessions                                                #
#                                                                                                 #
# University:              University of Essex                                                    #
#                                                                                                 #
# Programmer:              Lorenzo Crippa                                                         #
#                                                                                                 #
# Week:                    Week 23 (Monday 2nd of March, 2020)                                    #
###################################################################################################

# clear
rm(list=ls())

# Lorenzo Essex
#setwd("C:/Users/lc19059/Dropbox/Shared_Essex/GTA/GV300")

# Lorenzo Macbook
setwd("/Users/Lorenzo/Dropbox/Shared_Essex/GTA/GV300")

#################
library(haven)
library(estimatr)
library(MASS)
library(AER)
library(psych)
library(tidyverse)
library(summarytools)
library(texreg)
library(stargazer)
library(grid)
library(plm)
library(pastecs)
library(sandwich)
library(gganimate)
library(gifski)
library(transformr)
#################

# import data and turn variables id and t into numeric (id as a factor too, we will need it below)
panel <- read_dta("data/panelModelsData_rFile.dta") %>%
  mutate(
    id = as.numeric(paste(id)),
    t = as.numeric(paste(t)),
    id.fact = factor(id)
  ) %>% filter(id <= 100) # consider only the first 100 workers

######################
# Summary statistics #
######################

# start providing some descriptive statistics. You might be interested in explaining only some variables, perhaps:
cbind(panel$id, panel$t, panel$occ, panel$lwage,  panel$ed, panel$exp, 
      panel$exp2, panel$south, panel$smsa, panel$fem, panel$union) %>% describe()

# or you can export it using stargazer:
cbind(panel$id, panel$t, panel$occ, panel$lwage,  panel$ed, panel$exp, 
      panel$exp2, panel$south, panel$smsa, panel$fem, panel$union) %>% stargazer(summary=TRUE, type = "text")

# (and of course export it in LaTeX if you need):
cbind(panel$id, panel$t, panel$occ, panel$lwage,  panel$ed, panel$exp, 
      panel$exp2, panel$south, panel$smsa, panel$fem, panel$union) %>% stargazer(summary=TRUE, type = "latex")

# using the "summarytools" package you can get frequency tables for individual variables:
freq(panel$exp)

# and it's also a good idea to check two-ways contingency tables. For instance the contingency table of 
# occupation and being from the south in this data frame:
table(panel$exp, panel$occ)
# this gives you the number of respondents per each level of years of 
# experience, dividing them on whether they have a job or not

#########
# plots #
#########

# univariate plots, as usual
# we can obtain histograms, densities, box plots, ...
panel %>% ggplot(aes(x = lwage)) + geom_histogram(bins = 12, colour = "black", fill = "gray84") + 
  xlab("log(wage)") + theme_minimal()

# we can overlay different plots as well, e.g.: histograms and densities
panel %>% ggplot(aes(x = exp)) + geom_histogram(aes(y = ..density..), # we need to use this so as to have densities on the
                                                # y axis, not counts! Otherwise the graph would be terrible
                                                bins = 12, colour = "black", fill = "gray84") +
  geom_density(aes(fill = I("red"), alpha = I(.4))) + xlab("experience") + theme_minimal()
ggsave("slides/pictures/week_23_histdens.pdf", device = "pdf", width = 617/72, height = 504/72, units = "in", dpi = 72)


# bivariate plots
# we can use boxplots to explore bivariate distributions:
panel %>% mutate(
  south = as.integer(south)) %>%
  ggplot(aes(y = lwage, x = south)) + geom_boxplot(aes(group = south), fill = "gray84") +
  xlab("south") + ylab("log(wage)") + theme_minimal()

# or again, densities of log(wage) by different levels of education. Turn levels of education in four levels
panel %>% ggplot(aes(x = lwage, group = ed)) + geom_density(aes(fill = ed, alpha = I(.3))) +
  scale_fill_continuous("years of\neducation") + xlab("log(wage)") + theme_minimal()
ggsave("slides/pictures/week_23_dens.pdf", device = "pdf", width = 617/72, height = 504/72, units = "in", dpi = 72)

# and of course we can have multivariate plots. For instance, scatterplot of log(wage) and experience. 
panel %>% ggplot(aes(x = exp, y = lwage)) + geom_point() + 
  xlab("experience") + ylab("log(wage)") + theme_minimal()

# try to fit some model lines?
panel %>% ggplot(aes(x = exp, y = lwage)) + geom_point() + 
  geom_smooth(aes(color = "Linear"), method = "lm") +
  geom_smooth(aes(color = "Quadratic"), method = "lm", formula = y ~ x + I(x^2)) + # a quadratic fit?...
  xlab("experience") + ylab("log(wage)") +
  scale_color_manual(values = c("Linear" = "red","Quadratic" = "blue")) +
  guides(color = guide_legend(override.aes = list(fill = NA))) +
  theme_minimal() +
  theme(
    legend.position='bottom',
    legend.title=element_blank(),
    legend.spacing.x = unit(.2, 'cm')
  )
ggsave("slides/pictures/week_23_badscatter.pdf", device = "pdf", width = 617/72, height = 504/72, units = "in", dpi = 72)

###################################
# within-between units variations #
###################################

# in this dataframe we have information by different units: we want to highlight these differences now.

panel %>% filter(id<=20) %>% # only the first 20 individuals
  ggplot(aes(y = lwage, x = t, group = id)) + geom_line(aes(colour = id.fact)) + 
  theme_minimal() + theme(legend.position = "none") + xlab("time") + ylab("log(wage)")
# ok there seems to be some difference between units over time, indeed

# Let's highlight different workers in the scatterplot above, using different colours!
panel %>% ggplot(aes(x = exp, y = lwage)) + geom_point(aes(colour = id.fact)) +
  theme_minimal() + theme(legend.position = "none")

# add different fit lines by worker?
panel %>% ggplot(aes(x = exp, y = lwage)) + geom_point(aes(colour = id.fact)) +
  geom_smooth(aes(colour = id.fact), method = "lm", se = F) + theme_minimal() + 
  theme(legend.position = "none") + 
  geom_smooth(method = "lm") +  xlab("experience") + ylab("log(wage)") 
ggsave("slides/pictures/week_23_fixed_effect.pdf", device = "pdf", width = 617/72, height = 504/72, units = "in", dpi = 72)
# each worker seems to have a different intercept, but slopes seem all similar: it seems the case for a unit-fixed effect

# limit to 20 workers to see it more clearly:
panel %>% filter(id<=20) %>%
  ggplot(aes(y = lwage, x = exp)) + geom_point(aes(colour = id.fact)) +
  geom_smooth(aes(colour = id.fact), method = "lm", se = F) + theme_minimal() + 
  theme(legend.position = "none") + xlab("experience") + ylab("log(wage)")
ggsave("slides/pictures/week_23_groupline.pdf", device = "pdf", width = 617/72, height = 504/72, units = "in", dpi = 72)

# is there variation between time points? We can see it by doing:
panel %>% ggplot(aes(x = exp, y = lwage)) + geom_point(aes(colour = as.factor(t))) +
  geom_smooth(aes(colour = as.factor(t)), method = "lm", se = F) + theme_minimal() + theme(legend.position = "none") +
  xlab("experience") + ylab("log(wage)") + geom_smooth(method = "lm")
ggsave("slides/pictures/week_23_yearfe.pdf", device = "pdf", width = 617/72, height = 504/72, units = "in", dpi = 72)
# there does not seem to be difference in slopes over time. Difference is in intercepts

# ggplot2 tip: instead of having all lines in a single panel we can also have them in different ones. E.g.:
panel %>% ggplot(aes(x = exp, y = lwage)) + geom_point() +
  geom_smooth(aes(group = t), method = "lm", se = T) + facet_wrap(~t) +
  theme_minimal() + theme(legend.position = "none") +
  xlab("experience") + ylab("log(wage)")
ggsave("slides/pictures/week_23_timescatter.pdf", device = "pdf", width = 617/72, height = 504/72, units = "in", dpi = 72)

# in case we are feeling extremely nerdy we can also do fancy stuff and animate 
# graphs using the "gganimate" package (requires "gifski" and "transform" packages too)

# first save a plot in an object in r
p <- panel %>% ggplot(aes(x = exp, y = lwage)) + 
  geom_point(aes(colour = id.fact)) + geom_smooth(method = "lm") + theme_minimal() +
  theme(legend.position = "none") + xlab("experience") + ylab("log(wage)")

# then animate it!
p + transition_time(as.integer(t)) + # t is the variable we are animating on
  labs(title = "time: {frame_time}") # title of the gif
# slope is the same over time

anim_save("output/week_23_animated.gif")

# we can also animate the graph we made before about densities:
p <- panel %>% ggplot(aes(x = lwage, group = ed)) + geom_density(aes(fill = ed, alpha = I(.3))) +
  scale_fill_continuous("years of\neducation") + xlab("log(wage)") + theme_minimal()

p + transition_time(ed) + shadow_mark(alpha = 0.3) +
  labs(title = "education: {frame_time}")
anim_save("output/week_23_animated_density.gif")

####################
# nerd time's over #
####################

######################################
# within, between, overall variation #
######################################

# We want to see how our data vary within units, between units and overall. We need to look at the 
# standard deviations of each variable, but the procedure to do it properly is a bit tricky.
# We only consider a few variables in our dataset, the ones we might be interested in

# first get the overall means of our variables, regardless of distinctions between unit (id) and time (t):
overall.means <- panel %>% summarise_all(funs(mean), na.rm = TRUE)

# then get the group means (the means by unit) for each variable, and merge it with the entire dataset we have
group.means <- panel %>% group_by(id) %>% 
  summarise_all(funs(g.mean = mean), na.rm = TRUE)
d.group.means <- left_join(panel, group.means, by ="id") # with this command we merge "panel" and "group.means"

# Now compute the within variation: subtract each individual unit-time observation from the unit means. 
# Remember to add the overall means of each variable for comparability across units.
within.variation <- d.group.means %>%
  mutate(
    var.t = t - t_g.mean + overall.means$t,
    var.lwage = lwage - lwage_g.mean + overall.means$lwage,
    var.ed = ed - ed_g.mean + overall.means$ed,
    var.exp = exp - exp_g.mean + overall.means$exp,
    var.exp2 = exp2 - exp2_g.mean + overall.means$exp2,
    var.wks = wks - wks_g.mean + overall.means$wks,
    var.south = south - south_g.mean + overall.means$south,
    var.tdum1 = tdum1 - tdum1_g.mean + overall.means$tdum1
  ) %>%
  select(var.t:var.tdum1) %>% # select only the variables you want to summarise!
  summarise_all(funs(sd), na.rm = TRUE) # obtain the standard deviation for each variable

# now get the overall and between variations of each variable: the will simply be the standard deviation
# for each variable overall and the standard deviation of the group means for each variable:
overall.variation <- panel %>% select(t,lwage,ed,exp,exp2,wks,south,tdum1) %>% 
  summarise_all(funs(sd), na.rm = TRUE)

between.variation <- group.means %>% 
  select(t_g.mean,lwage_g.mean,ed_g.mean,exp_g.mean,
         exp2_g.mean,wks_g.mean,south_g.mean,tdum1_g.mean) %>% 
  summarise_all(funs(sd), na.rm = TRUE)

# now display all the variations:
within.variation
between.variation
overall.variation

##############
# estimation #
##############

# we can now model wage as function of experience, hours of weeks worked, and education.

# we run a series of different models.

# 1) POOLED MODELS

# Pooled OLS with incorrect default standard errors (non-clustered) first
# Notice that these standard errors will be robust to heteroskedasticity (we are using lm_robust())
# but NOT to serial correlation between units! In order to overcome this problem we will need to cluster them below
ols <- lm_robust(lwage ~ exp + exp2 + wks + ed, data = panel)
summary(ols)

# equivalently, you can run a plm() (panel data) model. Remember to specify the indexes id and t! 

# Pooled OLS
ols.pooled <- lm(lwage ~ exp + exp2 + wks + ed, data = panel) # here we run a pooled OLS (theta = 0)
summary(ols.pooled)
vcov.robust <- vcovHC(ols.pooled, type = 'HC0', adjust = T)
coeftest(ols.pooled, vcov.robust) 

# save the standard errors only (we will need them in the table at the end)
vcov.robust <- sqrt(diag(vcov.robust))

# we also need to save the pvalues. We prepare a small loop, where we iterate the t-test for each coefficient estimated
pvalues.pooled <- rep(NA, length(vcov.robust)) # prepare an empty vector to store the pvalues: always do it OUT of a loop!
for (i in 1:length(pvalues.pooled)) { # this is our loop
  tstat <- abs(ols.pooled$coefficients[i]/vcov.robust[i]) # save individual tstat for each coefficient. Use absolute value! 
  names(tstat) <- NULL # remove the name associated with the tstat (the name of the corresponding variable)
  pvalues.pooled[i] <- (1-pt(tstat, df = nobs(ols.pooled)-1))*2 # perform the 2-tailed t-test (thus, we multiply times 2)
}

# instead of having not-clustered SEs, we should CLUSTER our standard errors over units:

# Pooled OLS with cluster-robust standard errors using lm_robust
ols.cluster <- lm_robust(lwage ~ exp + exp2 + wks + ed, data = panel, clusters = id)
summary(ols.cluster)

# Which is exactly as adding clustered SEs starting from the "pooled OLS" model. We now need to re-estimate it
# using the plm() command and specifying which ones are the units and time indicator:
ols.plm <- plm(lwage ~ exp + exp2 + wks + ed, data = panel,
               index = c('id','t'), model = 'pooling')
summary(ols.plm)
vcov.cluster <- vcovHC(ols.plm, type = 'HC0', cluster = 'group', adjust = T)
coeftest(ols.plm, vcov.cluster)

# save the standard errors only (we will need them in the table at the end)
vcov.cluster <- sqrt(diag(vcov.cluster))

# we also need to save the pvalues. We prepare a small loop again
pvalues.pooled.clustered <- rep(NA, length(vcov.cluster)) 
for (i in 1:length(pvalues.pooled)) { 
  tstat <- abs(ols.plm$coefficients[i]/vcov.cluster[i]) 
  names(tstat) <- NULL 
  pvalues.pooled.clustered[i] <- (1-pt(tstat, df = nobs(ols.plm)-1))*2
}

# (We can also make Feasible Generalised Least squares (FGLS) pooled if we want)
fgls <- pggls(lwage ~ exp + exp2 + wks + ed,data = panel, index = c('id','t'), model="pooling")
summary(fgls)

# 2) INTRODUCE FIXED EFFECT

# Including fixed effects (on units: individual worker) into OLS
ols.dummy <- lm_robust(lwage ~ exp + exp2 + wks + ed + id.fact, # the last one is the fixed effect: the set of dummies!
                       data = panel, clusters = id)
summary(ols.dummy) # notice that R automatically drops one dummy to avoid perfect multicollinearity

# Testing pooled OLS whether country fixed effects matter: F test of joint significance of 
# the 100 worker fixed effects: 
# (1) get the sum of squared residuals of the restricted model 
# (the one where we include the restriction that the fixed effect has jointly parameter = 0)
ols <- lm(lwage ~ exp + exp2 + wks + ed, data = panel)
rss_ols <- ols$residuals^2 %>% sum() # save the sum of squared residuals

# (2) retrive the degrees of freedom for the restricted model
df_ols <- ols$df.residual

# (3) get the sum of squared residuals of the unrestricted model
ols.dummy <- lm(lwage ~ exp + exp2 + wks + ed + id.fact, data = panel)
rss_ols.dummy <- ols.dummy$residuals^2 %>% sum()

# (4) retrive the degrees of freedom for the unrestricted model
df_ols.dummy <- ols.dummy$df.residual

# (5) calculate the number of restrictions.
# the degrees of freedom of the restricted model will be:
# dfr = N - k - r, where N are the observations, k parameters (intercept included) and r restrictions
# the degrees of freedom for the unrestricted model will be:
# dfu = N - k.
# From the first equation we get: r = dfr - (N - k) which is: r = dfr - dfu
r <- df_ols - df_ols.dummy

# (6) calculate the F statistic
F_stat <- ((rss_ols-rss_ols.dummy)/r)/(rss_ols.dummy/df_ols.dummy) 

# (7) retrieve the critical value
F_crit <- qf(.95, r, df_ols)

# (8) Get p-value 
F_stat %>% pf(r, df_ols.dummy, lower.tail = F)

# the Prob>F is < 0.05, so we can reject the null that the coefficientt for all workers are 
# jointly equal to zero: therefore worker fixed-effects are warranted in this model.

# Equivalently to computing the OLS with worker dummies but much more time efficient
fe <- plm(lwage ~ exp + exp2 + wks + ed, data = panel, index = c('id','t'), model = 'within')
summary(fe)
# to obtain clustered standard errors
vcov.cluster.fe <- vcovHC(fe, type = 'HC0', cluster = 'group', adjust = T)
coeftest(fe,vcov.cluster.fe)

# save the standard errors only:
vcov.cluster.fe <- sqrt(diag(vcov.cluster.fe))

# we also need to save the pvalues. We prepare a small loop again
pvalues.fe <- rep(NA, length(vcov.cluster.fe)) 
for (i in 1:length(pvalues.fe)) { 
  tstat <- abs(fe$coefficients[i]/vcov.cluster.fe[i]) 
  names(tstat) <- NULL 
  pvalues.fe[i] <- (1-pt(tstat, df = nobs(fe)-1))*2
}

# 3) INTRODUCE RANDOM EFFECT

# Random effects regression
re <- plm(lwage ~ exp + exp2 + wks + ed, data = panel, index = c('id','t'), model = 'random')
summary(re)
# to obtain clustered standard errors
vcov.cluster.re <- vcovHC(re, type='HC2', cluster='group', adjust=T)
coeftest(re,vcov.cluster.re)

# save the standard errors only:
vcov.cluster.re <- sqrt(diag(vcov.cluster.re))

# we also need to save the pvalues. We prepare a small loop again
pvalues.re <- rep(NA, length(vcov.cluster.re)) 
for (i in 1:length(pvalues.re)) { 
  tstat <- abs(re$coefficients[i]/vcov.cluster.re[i]) 
  names(tstat) <- NULL 
  pvalues.re[i] <- (1-pt(tstat, df = nobs(re)-1))*2
}

# FE or RE? Hausman test
phtest(fe, re)
# The hausman test tells you that coefficient estimates in FE and RE model are significantly different. 
# That means, RE is inconsistent. Accounting for individual-level effects changes our estimate of the coefficent
# on experience so much, we cannot credibly claim that experience is not related to this unobserved individual-level
# effect. 

# Hence, go for the FE here, since we reject the null hypothesis: RE estimators are inconsistent.

# present the results using screenreg: Pooled without clustered SEs, pooled with clustered SEs,
# fixed effect and random effect (both with clustered SEs)
screenreg(list(ols.pooled, ols.plm, fe, re), include.rmse = F, stars = c(.01, .05, .1),
          override.se = list(vcov.robust, vcov.cluster, vcov.cluster.fe, vcov.cluster.re),
          override.pvalues = list(pvalues.pooled, pvalues.pooled.clustered, pvalues.fe, pvalues.re),
          custom.model.names = c("Pooled", "Pooled (clustered)", "Fixed Effect", "Random Effect"))

# export it in latex:
texreg(list(ols.pooled, ols.plm, fe, re), include.rmse = F, stars = c(.01, .05, .1),
       override.se = list(vcov.robust, vcov.cluster, vcov.cluster.fe, vcov.cluster.re),
       override.pvalues = list(pvalues.pooled, pvalues.pooled.clustered, pvalues.fe, pvalues.re),
       custom.model.names = c("Pooled", "Pooled (clustered)", "Fixed Effect", "Random Effect"))

###############
### THE END ###
###############