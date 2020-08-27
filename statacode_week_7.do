/***************************************************************************************************/
/* Project:                 GV300 - LABa02 sessions - Problem Set 2 answers                        */
/*                                                                                                 */
/* University:              University of Essex                                                    */
/*                                                                                                 */
/* Programmer:              Lorenzo Crippa                                                         */
/*                                                                                                 */
/***************************************************************************************************/
* Exercise 7
set obs 1000

gen obs = rbinomial(7, 0.3)

histogram obs

sum obs

* mean is 2.088
* std deviation is 1.208187 thus the variance is:

display 1.208187^2

* variance is 1.4597158

******************
