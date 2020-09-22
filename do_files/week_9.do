/***************************************************************************************************/
/* Project:                 GV300 - LABa02 sessions - Problem Set 3 answers                        */
/*                                                                                                 */
/* University:              University of Essex                                                    */
/*                                                                                                 */
/* Programmer:              Lorenzo Crippa                                                         */
/*                                                                                                 */
/***************************************************************************************************/

* Lorenzo Essex
cd "C:/Users/lc19059/Dropbox/Shared_Essex/GTA/GV300"

* install command
findit ghistcum

clear

* Exercise 2
set obs 5000

set seed 1111
gen x = rbinomial(12, .3)

* PMF
histogram x, fraction ytitle("frequency")
graph export "slides/pictures/PMF_stata.pdf", as(pdf) replace

* CDF
cumul x, generate(y) equal
sort y

scatter y x, ytitle("cumulative frequency")
graph export "slides/pictures/CDF_stata.pdf", as(pdf) replace

* or (ugly):
ghistcum x, bin(10)

* Exercise 4
clear
set obs 10

gen xl = .
replace xl = 11 if _n == 1
replace xl = 4 if _n == 2
replace xl = 2 if _n == 3
replace xl = 10 if _n == 4
replace xl = 8 if _n == 5 | _n == 7
replace xl = 13 if _n == 6
replace xl = 12 if _n == 8

gen xs = .
replace xs = 14 if _n == 1
replace xs = 2 if _n == 2 | _n == 3 | _n == 6
replace xs = 6 if _n == 4
replace xs = 12 if _n == 5 
replace xs = 4 if _n == 7
replace xs = 1 if _n == 8 | _n == 9
replace xs = 7 if _n == 10

sum

graph box xl
graph box xs

* t test
ttest xs == xl

* Exercise 6
* a
clear

program drop _all
program define rchisq, rclass
	syntax [, obs(integer 50)]
	drop _all
	set obs `obs'
	tempvar mu
	g `mu' = rchi2(50)
	sum `mu'
	return scalar mu = r(mean)
end

* see how it works
rchisq
rchisq, obs(10)

* draw 50 observations from 100 random variables and compute their means 
simulate mu=r(mu), reps(100) saving(sim, replace): rchisq

use sim, clear
hist mu, col(gray) name(hist50, replace) ///
	ti("Histogram of mean. 50 observations, 100 variables", col(black))

* b
local nSimsList = "100 1000"
foreach s in `nSimsList' {
	simulate mu=r(mu), reps(`s') saving(sim, replace): rchisq, obs(50)
	use sim, clear
	hist mu, col(gray) name(histobs50var`s', replace) ///
	ti("50 observations, `s' variables", col(black))
}
gr combine histobs50var100 histobs50var1000, rows(1) 

local nSimsList = "100 1000"
foreach s in `nSimsList' {
	simulate mu=r(mu), reps(`s') saving(sim, replace): rchisq, obs(100)
	use sim, clear
	hist mu, col(gray) name(histobs100var`s', replace) ///
	ti("100 observations, `s' variables", col(black))
}
gr combine histobs100var100 histobs100var1000, rows(1) 

local nSimsList = "100 1000"
foreach s in `nSimsList' {
	simulate mu=r(mu), reps(`s') saving(sim, replace): rchisq, obs(1000)
	use sim, clear
	hist mu, col(gray) name(histobs1000var`s', replace) ///
	ti("1000 observations, `s' variables", col(black))
}
gr combine histobs1000var100 histobs1000var1000, rows(1) 

local nSimsList = "100 1000"
foreach s in `nSimsList' {
	simulate mu=r(mu), reps(`s') saving(sim, replace): rchisq, obs(10000)
	use sim, clear
	hist mu, col(gray) name(histobs10000var`s', replace) ///
	ti("10000 observations, `s' variables", col(black))
}
gr combine histobs10000var100 histobs10000var1000, rows(1) 

* c
program drop _all
program chi2histogram
	args m n
    local N = `n'*`m'
    set obs `N'
    g varMean = .
    forvalue i = 1/`m' {
    	tempvar chi2Var
       	g `chi2Var' = rchi2(50)
       	sum `chi2Var'
       	replace varMean = r(mean) in `i'
       	}
	hist varMean, name(hist`m'_`n', replace) title("N=`n', Variables=`m'", col(black)) xsc(off)
	*store plot with name() to later combine them
end


chi2histogram 100 50

	

***************
*** THE END ***
***************
