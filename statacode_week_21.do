/*************************************************************************************************/
/* Project:                 GV300 - LABa02 sessions                                              */
/*                                                                                               */
/* University:              University of Essex                                                  */
/*                                                                                               */
/* Programmer:              Lorenzo Crippa                                                       */
/*                                                                                               */
/* Week:                    Week 21 (Monday 17th of February, 2020)                              */
/*************************************************************************************************/

* Lorenzo Essex
cd "C:/Users/lc19059/Dropbox/Shared_Essex/GTA/GV300"

* Lorenzo Macbook
*cd "/Users/Lorenzo/Dropbox/Shared_Essex/GTA/GV300"

* load dataset
use data/Card_data.dta, replace

sum wage educ nearc4 black married exper, separator(0)

hist educ

kdensity wage // very skewed, maybe better to log it?

gen lwage = log(wage) // definitely better
kdensity lwage

twoway scatter lwage educ

twoway (scatter lwage educ if nearc4 == 0) (scatter lwage educ if nearc4 == 1)

* OLS estimation
reg lwage educ exper black south married smsa, robust
est store model_ols

* 2SLS estimation
ivreg lwage exper black south married smsa (educ = nearc4), robust
est store model_iv

* notice that this is exactly the same as doing the two stages manually
* first stage:
reg educ nearc4 exper black south married smsa
predict educ_fit

* second stage
reg lwage educ_fit exper black south married smsa, robust
est store model_2sls

* show that parameters are identical (standard errors will be different):
esttab model_iv model_2sls, scalar(N r2 r2_a F p) order(educ educ_fit)

* they are identical indeed.
* So, under the hood ivreg does exactly the 2SLS estimation we can do manually.

* Note: ALWAYS REPORT ROBUST STANDARD ERRORS AND T-TEST PERFORMED USING THEM!!

* table for OLS and 2SLS
esttab model_ols model_iv, scalar(N r2 r2_a F p)

* evaluate the IV:
graph box educ, by(nearc4) ytitle("years of schooling")
* it does not look like the presence of a 4-years college in the county affects years of schooling: 
* distributions are basically identical, I can't distinguish the two boxplots. Of course, it might
* be that the effect of the instrument on the IV emerges after controlling for our relevant included
* variables, but this already is not a very good sign.

* let's move to more analytical tools. 
* Let's heck what's the F statistics of the first stage of our 2SLS model. Let's obtain it
reg educ nearc4 exper black south married smsa
* the F statistic is large, thus the instrument is arguably strong.

* Still, does it meet the exclusion restriction assumption?

***************
*** THE END ***
***************
