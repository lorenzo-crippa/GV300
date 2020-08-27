/*************************************************************************************************/
/* Project:                 GV300 - LABa02 sessions                                              */
/*                                                                                               */
/* University:              University of Essex                                                  */
/*                                                                                               */
/* Programmer:              Lorenzo Crippa                                                       */
/*                                                                                               */
/* Week:                    Week 24 (Monday 9th of March, 2020)                                  */
/*************************************************************************************************/

* Lorenzo Essex
cd "C:/Users/lc19059/Dropbox/Shared_Essex/GTA/GV300"

* Lorenzo Macbook
* cd "/Users/Lorenzo/Dropbox/Shared_Essex/GTA/GV300"

**************
* Question 1 *
**************

* data import
import delimited data/indicators.csv, clear

* first turn some string values into numeric
replace gdppercapita = "." if gdppercapita == "NA"
replace exportssharegdp = "." if exportssharegdp == "NA"
replace taxrevenuesharegdp = "." if taxrevenuesharegdp == "NA"
replace importssharegdp = "." if importssharegdp == "NA"

destring gdppercapita exportssharegdp taxrevenuesharegdp importssharegdp, replace

* (a)
* control and treatment indicator
gen treatment = 0 + (yearjoineu == 2004)
* 1 if treatment group, 0 otherwise

* add also indicators for pre-post intervention and the interaction with the previous one
gen intervention = 0 + (year >= 2004)
* 1 if after 2004, 0 otherwise

* countries in the treatment group
br if treatment == 1
* Czech Republic  Estonia         Hungary         Latvia          Lithuania       Poland          Slovak Republic
* Slovenia    

* countries in the control group
br if treatment == 0

* Albania        Armenia        Bulgaria       Croatia        Georgia        Kosovo         Macedonia, FYR Moldova       
* Montenegro     Serbia         Ukraine     

* (b)
* compute means of GDP per capita by group pre and after intervention
egen GDPcapita_treat_pre = mean(gdppercapita) if treatment == 1 & intervention == 0
egen GDPcapita_treat_pos = mean(gdppercapita) if treatment == 1 & intervention == 1
gen GDPcapita_treat = GDPcapita_treat_pre if treatment == 1 & intervention == 0 
replace GDPcapita_treat = GDPcapita_treat_pos if treatment == 1 & intervention == 1

egen GDPcapita_contr_pre = mean(gdppercapita) if treatment == 0 & intervention == 0
egen GDPcapita_contr_pos = mean(gdppercapita) if treatment == 0 & intervention == 1
gen GDPcapita_contr = GDPcapita_contr_pre if treatment == 0 & intervention == 0 
replace GDPcapita_contr = GDPcapita_contr_pos if treatment == 0 & intervention == 1

* compute DiD
display (15495.61 - 5306.678) - (4873.54 - 1574.91)
* 6890.302

* plot them
graph twoway (scatter GDPcapita_treat intervention) (line GDPcapita_treat intervention, lcolor(navy)) ///
	(scatter GDPcapita_contr intervention, color(maroon)) (line GDPcapita_contr intervention, lcolor(maroon)), ///
	graphregion(color(white)) ytitle("GDP per capita") xlabel(0 1) ///
	legend(order(2 "treatment" 4 "control"))

* (c)
* means of control and treatment groups over year
egen GDPcapita_mean_tre = mean(gdppercapita) if treatment == 1, by(year)
egen GDPcapita_mean_con = mean(gdppercapita) if treatment == 0, by(year)

* now we need to create the counterfactual
display 9810.952 - 2830.886
* 6890.66 is the difference between the mean GDP per capitas in the two groups in 2004
gen GDPcapita_mean_count = GDPcapita_mean_con + 6890.66 if year >= 2004

* plot them
sort year
twoway (line GDPcapita_mean_tre year) (line GDPcapita_mean_con year) ///
	(line GDPcapita_mean_count year), xline(2004, lcolor(black)) graphregion(color(white)) ///
	legend(order(1 "treatment" 2 "control" 3 "counterfactual")) ytitle("GDP per capita")

* (d)
reg gdppercapita intervention treatment c.intervention#c.treatment, robust
est store d

* (e)
reg gdppercapita intervention treatment c.intervention#c.treatment, vce(cluster country)
est store e

* (f)
* for instance we might want to include exports as a share of GDP, which might confound 
* both the probability of joining the EU (receiving the treatment) and the GDP per capita
reg gdppercapita intervention treatment c.intervention#c.treatment exportssharegdp, vce(cluster country)
est store f1

* or we might want to control for joining the EU after 2004:
reg gdppercapita intervention treatment c.intervention#c.treatment yearjoineu, vce(cluster country)
est store f2

* print results for d, e, f:
esttab d e f1 f2, star(* .1 ** .05 *** .01) scalars(N r2 r2_a F p) se mtitles("(d)" "(e)" "(f)" "(f)")

**************
* Question 2 *
**************

* import data
use data/anesByState.dta, clear

* declare dataset
xtset state year

* (a)
* summary statistics from psych package
sum FTM white poor turnout voteDem dem, separator(0)

* plots
* just usual distributions:
hist FTM, bin(12) graphregion(color(white)) bcolor(gs9) xtitle("feeling thermometer") kdensity

* or you can show different distributions by states
graph box white, by(state, graphregion(color(white))) ytitle("share of white people")

* FTM by state over time, with overall mean by year:
egen FTM_mean = mean(FTM), by(year)

xtline FTM FTM_mean, ytitle("feeling thermometer") legend(order(1 "state value" 2 "mean value"))
* some variation between units but similar to the overall mean over time

* (b)
xtsum FTM white poor turnout voteDem dem

* (c)
* then run the pooled model
reg FTM white poor dem turnout, robust
est store c
* betas could be biased cause we are not controlling for between-unit variations (fixed or random effect),
* moreover we are not clustering the SEs, which means they might be biased due to serial correlation

* (d)
reg FTM white poor dem turnout, vce(cluster state)
est store d

* (e)
* we can introduce the dummies manually in a pooled OLS model:
reg FTM voteDem dem poor white i.state, vce(cluster state)
est store e

* (f)
* or (better) we use entity demeaned model thanks to xtreg, fe:
xtreg FTM voteDem dem poor white, fe vce(cluster state)
est store f

* (g)
* introduce random effect
xtreg FTM voteDem dem poor white, re vce(cluster state)
est store g

* print regression results
esttab c d f g, star(* .1 ** .05 *** .01) scalars(N r2 r2_a F p) se mtitles("(c)" "(d)" "(f)" "(g)")

* (h)
* run a Hausman test for deciding between a FE (model f) and RE (model g)
* first rerun the models without robust SEs (otherwise it won't run)
xtreg FTM voteDem dem poor white, fe 
est store fe

xtreg FTM voteDem dem poor white, re 
est store re
 
hausman fe re 
* you should probably go for a fixed effect here

***************
*** THE END ***
***************
