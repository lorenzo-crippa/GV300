/*************************************************************************************************/
/* Project:                 GV300 - LABa02 sessions                                              */
/*                                                                                               */
/* University:              University of Essex                                                  */
/*                                                                                               */
/* Programmer:              Lorenzo Crippa                                                       */
/*                                                                                               */
/* Week:                    Week 23 (Monday 2nd of March, 2020)                                  */
/*************************************************************************************************/

* Lorenzo Essex
cd "C:/Users/lc19059/Dropbox/Shared_Essex/GTA/GV300"

* Lorenzo Macbook
* cd "/Users/Lorenzo/Dropbox/Shared_Essex/GTA/GV300"

* import data and turn variables id and t into numeric (id as a factor too, we will need it below)
use data/panelModelsData_rFile.dta, clear

drop if id > 100

* with panel data we need to tell Stata which variable represents the 
* unit indicator and which one the time indicator:
xtset id t // id is the unit indicator, t the time
sort id t

**********************
* Summary statistics *
**********************

* start providing some descriptive statistics. You might be interested in 
* explaining only some variables, perhaps:
sum id t occ lwage ed exp exp2 south smsa fem union, separator(0)

* you can get frequency tables for individual variables:
tab exp

* and it's also a good idea to check two-ways contingency tables. For instance the contingency table of 
* occupation and being from the south in this data frame:
tab exp occ

* this gives you the number of respondents per each level of years of 
* experience, dividing them on whether they have a job or not

*********
* plots *
*********

* univariate plots, as usual
* we can obtain histograms, densities, box plots, ...
hist lwage, bin(12) graphregion(color(white)) bcolor(gs9) xtitle("log(wage)")


* we can overlay different plots as well, e.g.: histograms and densities
hist exp, bin(12) graphregion(color(white)) bcolor(gs9) xtitle("experience") kdensity

* bivariate plots
* we can use boxplots to explore bivariate distributions:
graph box lwage, by(south, graphregion(color(white))) ytitle("log(wage)")

* and of course we can have multivariate plots. For instance, scatterplot of log(wage) and experience. 
scatter lwage exp, graphregion(color(white)) xtitle("experience") ytitle("log(wage)")

* try to fit some model lines?
twoway (scatter lwage exp) ///
	(lfitci lwage exp, ciplot(rarea)) (qfitci lwage exp, ciplot(rarea)), ///
	graphregion(color(white)) xtitle("experience") ytitle("log(wage)") ///
	legend(order(3 "linear fit" 4 "quadratic fit"))

***********************************
* within-between units variations *
***********************************

* in this dataframe we have information by different units: we want to highlight these differences now.
xtline lwage if id <= 20, overlay leg(off) ytitle("log(wage)") xtitle("time")
* ok there seems to be some difference between units over time, indeed

* Let's highlight different workers in the scatterplot above, using different colours!
* In Stata things are a bit trickier: we need to isolate wage for each level of the id
separate lwage, by(id) gen(lwage_id)

* then we can plot points, coloring them for each different id (do it for the first 20 workers):
graph twoway (scatter lwage_id1-lwage_id30 exp) ///
	(lfitci lwage exp), leg(off) xtitle("experience") ///
	ytitle("log(wage)") graphregion(color(white))
* the line that passes through each worker's observations seems to have a different intercept and a 
* different slope from the pooled regression line. Moreover, slopes of each unit's line are similar
* although their intercepts differ: it seems the case for a unit-fixed effect

* is there variation between time points? We can see it by doing:
twoway (scatter lwage exp,by(t)) (lfitci lwage exp, by(t)), ///
	xtitle("experience") ytitle("log(wage)") legend(order(3 "linear fit"))

**************************************
* within, between, overall variation *
**************************************

* We want to see how our data vary within units, between units and overall. 
* In Stata this is terribly easy to do:
xtsum t lwage ed exp exp2 wks south tdum1

**************
* estimation *
**************

* we can now model wage as function of experience, hours of weeks worked, and education.

* we run a series of different models.

* 1) POOLED MODELS

* Pooled OLS with incorrect default standard errors (non-clustered) first
* Notice that these standard errors will be robust to heteroskedasticity (we are using the robust option)
* but NOT to serial correlation between units! In order to overcome this problem we will need to cluster them below
reg lwage exp exp2 wks ed, robust
est store ols_pooled

* instead of having not-clustered SEs, we should CLUSTER our standard errors over units:

* Pooled OLS with cluster-robust standard errors using lm_robust
reg lwage exp exp2 wks ed, vce(cluster id)
est store ols_cluster

* (We can also make Feasible Generalised Least squares (FGLS) pooled if we want)
xtgls lwage exp exp2 wks ed

* 2) INTRODUCE FIXED EFFECT

* Including fixed effects (on units: individual worker) into OLS
reg lwage exp exp2 wks ed i.id, vce(cluster id)
* the i.id is the fixed effect! Stata introduces one dummy for each value of id.
* Notice that Stata automatically drops one dummy to avoid perfect multicollinearity

* alternatively you can do:
xtreg lwage exp exp2 wks ed, vce(cluster id) fe // fe is the fixed effect!
est store fe

* Testing pooled OLS whether country fixed effects matter: F test of joint significance of 
* the 100 worker fixed effects: re-run the xtreg model above without robust standard errors.
* Below the table you will find a test about the joint significance of the fixed effect:
xtreg lwage exp exp2 wks ed, fe 
* F test that all u_i=0: F(99, 597) = 87.63 Prob > F = 0.0000, so we can reject the 
* null that the coefficientt for all workers are jointly equal to zero: 
* therefore worker fixed-effects are warranted in this model.

* 3) INTRODUCE RANDOM EFFECT

* Random effects regression
xtreg lwage exp exp2 wks ed, vce(cluster id) re // re is the random effect!
est store re

* FE or RE? Hausman test. In order to do it in Stata we need to re-run the fixed
* effect and random effect models without robust (or clustered) standard errors,
* and store their results:
xtreg lwage exp exp2 wks ed, fe 
est store fe_haus

xtreg lwage exp exp2 wks ed, re 
est store re_haus

hausman fe_haus re_haus
* The hausman test tells you that coefficient estimates in FE and RE model are significantly different. 
* That means, RE is inconsistent. Accounting for individual-level effects changes our estimate of the coefficent
* on experience so much, we cannot credibly claim that experience is not related to this unobserved individual-level
* effect.

* Here Stata tells us that it's having issues in the matrix algebra to perform the Hausman test.
* Indeed what we get is a negative chi2 which makes no sense (chi2 is always positive). Yet in a case
* like this look at the absolute value of the chi2: it's huge and the degrees of freedom are 3.
* You can take a chi2 statistical table and verify that the pvalue for a chi2 of 3805.49 is basically 0.
* Hence, go for the FE here, since we reject the null hypothesis: RE estimators are inconsistent.

* present the results using screenreg: Pooled without clustered SEs, pooled with clustered SEs,
esttab ols_pooled ols_cluster fe re, star(* .1 ** .05 *** .01) scalars(N r2 r2_a F p) se ///
	mtitles("Pooled" "Pooled clustered" "Fixed effect" "Random effect") drop(o.ed)

***************
*** THE END ***
***************
