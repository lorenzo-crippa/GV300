/******************************************************************************/
/* Project:                 GV300 - LABa02 sessions                           */
/*                                                                            */
/* University:              University of Essex                               */
/*                                                                            */
/* Programmer:              Lorenzo Crippa                                    */
/*                                                                            */
/* Week:                    Week 8 (Monday 18th of November, 2019)            */
/******************************************************************************/

* Lorenzo Essex
*cd "C:/Users/lc19059/Dropbox/Shared_Essex/GTA/GV300"

* Lorenzo Macbook
cd "/Users/Lorenzo/Dropbox/Shared_Essex/GTA/GV300"

* import data
import delimited "data/Greene_data.csv", clear

* summarize data
sum, detail

* plot data
histogram price, xline(3.089996) xline(1.312782, lcolor(blue))
kdensity price, xline(3.089996) xline(1.312782, lcolor(blue))
graph box price, over(signed)
graph export "output/week_8_stata_box.pdf", as(pdf) replace

histogram height, xline(27.64698) xline(25.6, lcolor(blue))
kdensity height, xline(27.64698) xline(25.6, lcolor(blue))

histogram width, xline(32.1114) xline(31.9, lcolor(blue))
kdensity width, xline(32.1114) xline(31.9, lcolor(blue))

histogram signed
histogram house

kdensity signed
kdensity house

* Question for the class: why are the two latter graphs conceptually wrong?

***********************************************************************************
* 1) test the hypothesis that the mean price of a paint equals 4 million dollars.

* 2) test the hypothesis that the mean price of a paint sold in the 
* first house is different from that of a paint sold in the second house

* 3) test the hypothesis that the mean price of a signed paint is higher from the mean price of a non-signed one

* 1)
ttest price == 4
* reject it

* 2)
ttest price if house != 3, by(house)
* never reject it

* 3)
mean price if signed == 0
* 1.832712

ttest price == 1.832712
* you never reject the null that the mean is larger than 1.832712

***********************************************************************************
program drop _all

program define normalDistribution, rclass
	drop _all
	set obs 30
	tempvar mu
	g `mu' = rnormal(0, 1)
	sum `mu'
	return scalar mu = r(mean)
end

* see how it works:
*normalDistribution, obs(30) mean(0) sd(1)
normalDistribution

* draw 30 observations from 50 random variables and compute their means 
simulate mu=r(mu), reps(50) saving(sim, replace): ///
	normalDistribution, obs(30) mean(0) sd(1)
use sim, clear
hist mu, `graphr' col(blue) name(hist50, replace) ///
	ti("Histogram of mu for S = 50", col(black))

* employ the new program in a loop
program drop _all

program define normalDistribution, rclass
	syntax [, obs(integer 1) mean(real 0) sd(real 1)]
	drop _all
	set obs `obs'
	tempvar mu
	g `mu' = rnormal(`mean',`sd')
	sum `mu'
	return scalar mu = r(mean)
end

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


***************
*** THE END ***
***************
