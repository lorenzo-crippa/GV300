/******************************************************************************/
/* Project:                 GV300 - LABa02 sessions                           */
/*                                                                            */
/* University:              University of Essex                               */
/*                                                                            */
/* Programmer:              Lorenzo Crippa                                    */
/*                                                                            */
/* Week:                    Week 6 (Monday 4th of November, 2019)             */
/******************************************************************************/

* Lorenzo Essex
cd "C:/Users/lc19059/Dropbox/Shared_Essex/GTA/GV300"

* Lorenzo Macbook
*cd "/Users/Lorenzo/Dropbox/Shared_Essex/GTA/GV300"

/*********************************************/
/* Functions, loops, conditions and programs */
/*********************************************/

* Mathematical functions in a twoway space (2 dimensions)
* generate 1000 random values (let's pick them uniformly distributed within 0 and 100)

clear
set obs 1000
 
gen x = runiform(0,100)

* change a random value into a 0 (say the number 23)
replace x = 0 if _n == 23

* generate a linear function of x 
gen lin_y = 3*x -4

* plot twoway
sort x
graph twoway line lin_y x, ytitle("y")

* generate a quadratic function of x
gen qua_y = -1*x^2 + 3*x -4

* plot twoway
sort x
graph twoway line qua_y x, ytitle("y")

* generate a cubic function of x
gen cub_y = x^3 -1*x^2 + 3*x -4

* plot twoway
sort x
graph twoway line cub_y x, ytitle("y")

* generate the ln of x
gen ln_y = ln(x)

* plot twoway
sort x
graph twoway line ln_y x, ytitle("y")

* generate the exp of x
gen exp_y = exp(x)

* plot twoway
sort x
graph twoway line exp_y x, ytitle("y")

* notice the zoom:
sort x
graph twoway line exp_y x if x>= 10 & x <= 20, ytitle("y")

* generate the sin of x
gen sin_y = sin(x) + 1.3

* plot twoway
sort x
graph twoway line sin_y x, ytitle("y")

***************
* derivatives *
***************

* linear
gen d_lin_y = 3

* plot twoway
sort x
graph twoway line lin_y d_lin_y x, ytitle("y") yvarlabel("f(x)" "derivative")

* quadratic
gen d_qua_y = -2*x + 3

* plot twoway
sort x
graph twoway line qua_y d_qua_y x, ytitle("y") yvarlabel("f(x)" "derivative")

* cubic
gen d_cub_y = 3*x^2 -2*x + 3

* plot twoway
sort x
graph twoway line cub_y d_cub_y x, ytitle("y") yvarlabel("f(x)" "derivative")

* logarithm
gen d_ln_y = 1/x

* plot twoway
sort x
graph twoway line ln_y d_ln_y x, ytitle("y") yvarlabel("f(x)" "derivative")

* exponential
gen d_exp_y = exp(x)

* plot twoway
sort x
graph twoway line exp_y d_exp_y x, ytitle("y") yvarlabel("f(x)" "derivative")

* trigonometric
gen d_sin_y = cos(x)

* plot twoway
sort x
graph twoway line sin_y d_sin_y x, ytitle("y") yvarlabel("f(x)" "derivative")

*****************************************************************************
* learn how to program Stata functions
clear

* get rid of all previously defined programs
program drop _all

* define a new program that gives us the summary of a random normal distribution
program define normalDistribution, rclass
	syntax [, obs(integer 1) mean(real 0) sd(real 1)]
	drop _all
	set obs `obs'
	tempvar mu
	g `mu' = rnormal(`mean',`sd')
	sum `mu'
	return scalar mu = r(mean)
end

* see how it works
normalDistribution, obs(1000) mean(1.5) sd(7.12)

* employ the new program in a loop
foreach s in 5 50 500 5000 {
	simulate mu=r(mu), reps(`s') seed(010101) saving(sim, replace): ///
	normalDistribution, obs(15) mean(0) sd(1)
	use sim, clear
	hist mu, `graphr' col(blue) name(hist`s', replace) ///
	ti("Histogram of mu for S = `s'", col(black))
}

* combine the graph and save it in pdf
gr combine hist5 hist50 hist500 hist5000, rows(2)
gr export output/hist_simNormalDistrStata.pdf, replace


/*********************************************************************************/
/* Exercise 1                                                                    */
/* Program a function that takes as arguments:                                   */
/* a) the number of observations to be drawn from a random binomial distribution */
/* b) the number of trials                                                       */
/* c) the probability of success for a trial                                     */
/*                                                                               */
/* Exercise 2                                                                    */
/* Simulate the following:                                                       */
/* a) toss a fair coin 2, 20, 200, 2000, 4000 and 8000 times                     */
/*    and compute the mean                                                       */
/* b) store a boxplot and a histogram relative to the means obtained for         */
/*    each of the six iterations and export a pdf showing them side by side      */
/* c) export a graph showing all iterations and all plots together               */
/*    (a total of 12 plots, arranged in 3 rows)                                  */
/*********************************************************************************/

/*********************************************************************************/
/*********************************************************************************/
/*********************************************************************************/
/*********************************************************************************/
/*********************************************************************************/
/*********************************************************************************/
/*********************************************************************************/

*************
* Solutions *
*************

* Exercise 1

* get rid of all previously defined programs
program drop _all

* define a new program that gives us the mean of a random binomial distribution
program define binomDistrib, rclass
	syntax [, obs(integer 1) n(real 10) p(real 0.5)]
	drop _all
	set obs `obs'
	tempvar mean
	g `mean' = rbinomial(`n',`p')
	sum `mean'
	return scalar mean = r(mean)
end

* see how it works. It takes as arguments the desired number of observations, 
* the number of trials and the probability of success for a trial
binomDistrib, obs(1000) n(10) p(0.5)

* Exercise 2
* The following loop obtains the mean value drawn from a random binomial distribution with 
* n = 10 and p = 0.5. Samples drawn have size 2, 20, 200, 2000, 4000 and 8000 respectively
* The loop obtains boxplots and histograms and combines them
foreach i in 2 20 200 2000 4000 8000{
	simulate mean = r(mean), reps(`i') seed(112358) saving(sim, replace): ///
	binomDistrib, obs(1) n(10) p(0.5)
	use sim, clear
	graph box mean, `graphr' name(box`i', replace) ///
	title("Boxplot of mean for n = `i'", col(black))
	histogram mean, `graphr' name(hist`i', replace) ///
	title("Histogram of mean for n = `i'", col(black))
	gr combine hist`i' box`i', rows(1)
	gr export output/simBinomData_sample`i'.pdf, replace
}

* combine the graph and save it in pdf
gr combine hist2 box2 hist20 box20 hist200 box200 hist2000 box2000 hist4000 box4000 hist8000 box8000, rows(3)
gr export output/hist_box_simBinomData.pdf, replace

***************
*** THE END ***
***************
