/***************************************************************************************************/
/* Project:                 GV300 - LABa02 sessions - Problem Set 4 answers                        */
/*                                                                                                 */
/* University:              University of Essex                                                    */
/*                                                                                                 */
/* Programmer:              Lorenzo Crippa                                                         */
/*                                                                                                 */
/***************************************************************************************************/

* Lorenzo Essex
cd "C:/Users/lc19059/Dropbox/Shared_Essex/GTA/GV300"

* Lorenzo Mac
*cd "/Users/Lorenzo/Dropbox/Shared_Essex/GTA/GV300"

clear

set obs 2000
set seed 1234

**************
* exercise 1 *
**************

gen x1 = rnormal()
gen x2 = rnormal()
gen x3 = rnormal()

sum x1
* mean is -.02 sd is .98. Intervals will be:
* 68% obs in [-1; .96]
* 95% obs in [-1.98; 1.94]
* 99% obs in [-2.96; 2.92]

hist x1, xline(-1 .96) xline(-1.98 1.94) xline(-2.96 2.92)

centile x1, centile(.5 2.5 16 84 97.5 99)

* chi2
gen chi2 = x1^2

hist chi2, xline(3.84)

centile chi2, centile(95)

* F
gen F = x2^2 / x3^2

hist F

centile F, centile(95)

* t
gen t = x2 / sqrt(chi2)

hist t

centile t, centile(95)

**************
* exercise 2 *
**************

import delimited "data/baseball.csv", clear

tabstat heightinches, by(weightpounds)

egen exp_height = mean(heightinches), by(weightpounds)

* compare models
twoway scatter exp_height weightpounds || lfit exp_height weightpounds

twoway scatter heightinches weightpounds || lfit heightinches weightpounds

* models
sort weightpounds
egen id = group(weightpounds)

reg exp_height weightpounds if id[_n] != id[_n+1]
est store mod1

reg heightinches weightpounds
est store mod2

* compare results
esttab mod1 mod2, star(* .1 ** .05 *** .01) ar2 scalars(F p) se

**************
* exercise 3 *
**************

clear
set obs 6

* manually input data
gen district = _n

gen incumbent = "Matt Salmon"
replace incumbent = "Ed Pastor" if district == 2
replace incumbent = "Jim Kolbe" if district == 3
replace incumbent = "Bob Stump" if district == 4
replace incumbent = "John Shadegg" if district == 5
replace incumbent = "J.D. Hayworth" if district == 6

gen money = 362
replace money = 418 if district == 2
replace money = 712 if district == 3
replace money = 346 if district == 4
replace money = 426 if district == 5
replace money = 1839 if district == 6

gen vote_share = 65
replace vote_share = 68 if district == 2
replace vote_share = 52 if district == 3
replace vote_share = 65 if district == 4
replace vote_share = 69 if district == 5
replace vote_share = 53 if district == 6

reg vote_share money

* plot
twoway scatter vote_share money || lfit vote_share money

* regress on the intercept only
reg vote_share
sum vote_share

* ordinal variable
gen m_low = 1 * (money < 500)

* model
reg vote_share m_low

* group-wise mean
tabstat vote_share, by(m_low)

**************
* exercise 5 *
**************

clear
set obs 2000

gen university = 0 if _n <= 1000
replace university = 1 if _n > 1000

gen noise = rnormal()

gen income = 15000 + 5000 * university + 1000 * noise

reg income university

clear
set obs 2000

gen intelligence = runiform()
gen luck = runiform()
gen noise = rnormal()

gen university = 1 * (intelligence + luck > 1)

gen income = 15000 + 10000 * intelligence + 1000 * noise

reg income university

***************
*** THE END ***
***************
