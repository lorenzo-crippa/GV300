/******************************************************************************/
/* Project:                 GV300 - LABa02 sessions                           */
/*                                                                            */
/* University:              University of Essex                               */
/*                                                                            */
/* Programmer:              Lorenzo Crippa                                    */
/*                                                                            */
/* Week:                    Week 10 (Monday 2nd of December, 2019)            */
/******************************************************************************/

* Lorenzo Essex
cd "C:/Users/lc19059/Dropbox/Shared_Essex/GTA/GV300"

* Lorenzo Macbook
*cd "/Users/Lorenzo/Dropbox/Shared_Essex/GTA/GV300"

* import data
import delimited "data/Greene_data.csv", clear

* summarize data
sum, detail

pwcorr price height width

* graphs
twoway scatter price height
twoway scatter price width

graph box price, over(signed)
graph box price, over(house)

* multivariate
twoway (scatter price height if signed == 0) (scatter price height if signed == 1), ///
	legend(label(1 not signed) label(2 signed))
twoway (scatter price height if house == 1) (scatter price height if house == 2) (scatter price height if house == 3), ///
	legend(label(1 house 1) label(2 house 2) label(3 house 3))

	
*********
* model
reg price height width signed house	

* post-estimation analysis:
rvfplot, yline(0)
* we clearly have heteroskedasticity

* White-robust estimators for the standard errors:
reg price height width signed house, robust
ereturn list r2_a

***************
*** THE END ***
***************
