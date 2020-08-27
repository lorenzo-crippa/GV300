/**************************************************************************************************/
/*  Project:                 GV300 - LABa02 sessions                                              */
/*                                                                                                */
/* University:              University of Essex                                                   */
/*                                                                                                */
/* Programmer:              Lorenzo Crippa                                                        */
/*                                                                                                */
/* Week:                    Week 22 (Monday 24th of February, 2020)                               */
/**************************************************************************************************/

* Lorenzo Essex
*cd "C:/Users/lc19059/Dropbox/Shared_Essex/GTA/GV300"

* Lorenzo Macbook
cd "/Users/Lorenzo/Dropbox/Shared_Essex/GTA/GV300"

clear

**************
* Question 1 *
**************

set seed 123
set obs 1000

gen X = rpoisson(3)
gen Z = rbinomial(8, 0.4)
gen T = 2 + 3*Z - 2*X + rnormal()
gen Y = 1 + 2*T -3*X + rnormal()

* let's look at the models we get if we do not instrument
reg Y T
est store model_bias
* we estimate a slope of 2.59 whereas the true value is 2. 
* The intercept is also super biased (-11, it should be 1)

* if we control for the confounder X instead:
reg Y T X
est store model
* we get unbiased estimates: intercept is around 1, 
* slope of T is around 2 and slope of X is around -3

* often, though, we can't control for our confounders! 
* For instance if we don't observe them or if we don't have data.
* thus, we can instrument:
ivreg Y (T = Z)
est store model_iv
* notice that the slope is now unbiased, but the intercept is still 
* biased: the causal effect estimated is that OF T ONLY.

* display the results:
esttab model_bias model model_iv, scalar(N r2 r2_a F p) star(* .1 ** .05 *** .01)


* now generate a bad instrument
gen zNot = runiform()
* zNot can be a bad instrument for two reasons:

* 1)
drop T Y

gen T = 2 + 3*Z - 2*X + 0*zNot + rnormal() // zNot is not a cause of T
gen Y = 1 + 2*T -3*X + rnormal()

ivreg Y (T = zNot)
est store model_iv_bad // slope is very biased: we used a bad instrument!

* 2)
drop X T Y
gen X = 1 + 2*zNot + rnormal()
gen T = 2 + 3*Z - 2*X + 5*zNot + rnormal() // zNot IS a cause of T but also of X 
gen Y = 1 + 2*T -3*X + rnormal()

* (exclusion restriction is violated unless we control for X too)

ivreg Y (T = zNot)
est store model_iv_bad2 // slope is very biased: we used a bad instrument!

* if we controlled for the X (confounder effected by zNot) we would get unbiased results:
ivreg Y X (T = zNot)
est store model_iv_ok // this way we get unbiased estimates

* print results
esttab model_iv_bad model_iv_bad2 model_iv_ok, scalar(N r2 r2_a F p) star(* .1 ** .05 *** .01)


**************
* Question 2 *
**************

clear

set seed 123
set obs 50000

gen Instrument = rbinomial(1, 0.4)
gen ObservableThing = rnormal()
gen UnobservableThing = rnormal()

gen VariableOfInterest = (ObservableThing + UnobservableThing + Instrument >= 2.5)

tab VariableOfInterest

gen OutcomeVariable = UnobservableThing + 1 * VariableOfInterest + rnormal()

* a) effect is 1
* b)
reg OutcomeVariable VariableOfInterest
est store model1 // 2.34 (biased)

* a) effect is 1 (see equation of the DGP)
* notice that in order to estimate this unbiased effect we don't even need to control for ObservableThing,
* because this is not a confounder in the DGP of OutcomeVariable: we only need to control for UnobservableThing,
* which is a confounder:
reg OutcomeVariable VariableOfInterest UnobservableThing

* c)

* 2SLS manually:

* 1st stage
reg VariableOfInterest Instrument
predict fitted_var // save the fitted values!

* 2nd stage
reg OutcomeVariable fitted_var
est store second_st // 1.04 (unbiased)

* ivreg
ivreg OutcomeVariable (VariableOfInterest = Instrument)
est store model_iv

* show that results are the same:
esttab second_st model_iv, scalar(N r2 r2_a F p) star(* .1 ** .05 *** .01)

* d)
gen Instrument2 = rbinomial(1, 0.02)

corr Instrument Instrument2 // notice the correlation: it's very very low!
corr VariableOfInterest Instrument2 // very low too

ivreg OutcomeVariable (VariableOfInterest = Instrument2)
est store model_iv_weak // biased estimate

* e)
drop VariableOfInterest 
gen VariableOfInterest = ObservableThing + UnobservableThing + 0.05*Instrument

ivreg OutcomeVariable (VariableOfInterest = Instrument)
est store model_iv2 // biased estimate: weak effect of the instrument on treatment variable!

* f)
drop OutcomeVariable
gen OutcomeVariable = UnobservableThing + VariableOfInterest + 0.05*Instrument + rnormal()

ivreg OutcomeVariable (VariableOfInterest = Instrument)
est store model_iv3 
* biased: the instrument has an effect on the outcome variable(excl. restr. not met), even if weak it biases stuff !

* show the results
esttab model_iv model_iv_weak model_iv2 model_iv3, scalar(N r2 r2_a F p) star(* .1 ** .05 *** .01)

**************
* Question 3 *
**************

import delimited "data/USStateLegislature.csv", clear

* we need to turn turnout and votemargin into numeric values (they are strings because
* Stata does not recognize the "NA" as missing values
replace turnout = "." if turnout == "NA"
replace votemargin = "." if votemargin == "NA"

destring turnout votemargin, replace // this turns variables from strings to numeric ones

* scatterplot
twoway (scatter turnout votemargin if votemargin >= 0) ///
	(scatter turnout votemargin if votemargin < 0), xline(0) leg(off)
	
* let's show the two regions as separated and fit a linear model
twoway (scatter turnout votemargin if votemargin >= 0) ///
	(scatter turnout votemargin if votemargin < 0) ///
	(lfit turnout votemargin if votemargin >= 0) ///
	(lfit turnout votemargin if votemargin < 0), xline(0) leg(off)

* quadratic model
twoway (scatter turnout votemargin if votemargin >= 0) ///
	(scatter turnout votemargin if votemargin < 0) ///
	(qfit turnout votemargin if votemargin >= 0) ///
	(qfit turnout votemargin if votemargin < 0), xline(0) leg(off)

* locally smooth:
twoway (scatter turnout votemargin if votemargin >= 0) ///
	(scatter turnout votemargin if votemargin < 0) ///
	(lpoly turnout votemargin if votemargin >= 0) ///
	(lpoly turnout votemargin if votemargin < 0), xline(0) leg(off) xtitle("votemargin")
	
* models:
* notice that you need to install rdrobust from ssc:
ssc install rdrobust

rdrobust turnout votemargin, c(0)  vce(hc1)
est store model
 
* since we didn't specify a bandwidth (bw) the program automatically estimates the model calculating an optimal
* bandwidth, and using that bandwith, half that bandwith and twice that bandwith and reports its size and observations

* otherwise we can specify it:
rdrobust turnout votemargin, c(0) b(-.2 .2)  vce(hc1)
est store model2

rdrobust turnout votemargin, c(0) b(-.3 .3)  vce(hc1)
est store model3

* (d) 
* Testing assumption 2: discontinuity only occurs at cut-off = 0
rdrobust turnout votemargin, c(-.1) vce(hc1) // not significant
rdrobust turnout votemargin, c(.1) vce(hc1) // not significant

* Testing assumption 3: re-run the RDD analysis using other covariates (not the outcome variable) as dep. var:

* scatterplot
twoway (scatter democraticvoteshare_president votemargin if votemargin >= 0) ///
	(scatter democraticvoteshare_president votemargin if votemargin < 0), xline(0) leg(off)

rdrobust democraticvoteshare_president votemargin, c(0)  vce(hc1) // not significant

* There is no discontinuity in the relationship of x and any other covariate (at least when considering
* the covariates for which we have observation, i.e. democraticvoteshare_president)

* Assumption 1 in principle is not testable. Yet, we can get some evidence that could speak to it.
* The assumption tells us that manipulation/sorting into T and C is not possible: evaluate by density plot. 
* If there is no increased density around the cut-off, we probably do not observe manipulation. 
kdensity votemargin, xline(0)


* if we want to obtain confidence intervals, we need to use kdens:

* NOTE: you need to install kdens and moremata for it to work:
ssc install kdens
ssc install moremata

kdens votemargin, ci xline(0, lwidth(thin) lpattern("--")) lcolor(dknavy)

* or else you can distinguish the two subsets like:
kdens votemargin if votemargin >= 0, gen(d1 x1) ci(ci1_1 ci1_2) 
kdens votemargin if votemargin < 0, gen(d0 x0) ci(ci0_1 ci0_2) 

twoway rarea ci0_1 ci0_2 x0, color(dknavy%20) || ///
	rarea ci1_1 ci1_2 x1 , color(dknavy%20) || ///
	line d1 x1, lc(dknavy) || ///
	line d0 x0, lc(dknavy) /// 
	leg(off) xline(0, lwidth(thin) lpattern("--"))

***************
*** THE END ***
***************
