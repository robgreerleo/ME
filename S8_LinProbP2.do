* Development Microeconometrics using STATA
* Solution for Session 8: La Ferrara - Soap Operas and Fertility, Part 2
* Robert Leonard

version 15 

set more off, permanently // do not stop after each page

********************************************************
* Setting up the directories

global datadir "C:\Users\roble\Desktop\Working\STATA\HW 8\HW8 Data"
global workdir "C:\Users\roble\Desktop\Working\STATA\HW 8"

********************************************************

capture log close 
log using "$workdir/leonard_exc8.log", replace

********************************************************
* Q.1 Collapsing the dataset and the district level

use "$datadir/LCD_new", clear

xtset id year
xtdes

global controls "age age2 stock stock2 yrsedu_head wealth_noTV married catholic rural doctors ipc"
* drop large districts
drop if large!=0
gen n = 1
* need to add the weights (otherwise we'd oversample wrong areas, etc)
collapse (mean) birth globo $controls (sum) pop = n [aweight=weight] ///
, by(uf_code amc_code year) 
* generate variable that takes different values for all amc codes
egen amc = group(amc_code)

// set panel dataset, must specify level of variation
xtset amc year
* check balancing
xtdes

********************************************************
* Q.2 Re-run the model for the regional level regressions
* column 3
areg birth globo i.year, absorb(amc_code) cluster(amc_code)
est store e3a
* add controls, column 6
areg birth globo $controls i.year, absorb(amc_code) cluster(amc_code)
est store e6a
* Rerun, weighted by population, column 3
areg birth globo i.year [aweight=pop], absorb(amc_code) cluster(amc_code)
est store e3b
* again add controls, column 6
areg birth globo $controls i.year [aweight=pop], absorb(amc_code) cluster(amc_code)
est store e6b

//compare the results
esttab e3a e3b e6a e6b, b(%9.4f) se(%9.4f) stat(N r2) drop(*.year) label
/* The first sets are underestimations compared to the paper.  We aren't
including weighting here, so we don't have the correct sample for comparison.
The second set includes weighting factors giving more weight to districts with
higher populations. The results are in line with what the paper describes.
So the first set results in statistically insignificant results and lower 
coefficients while the second set results in much higher coefficients and
statistically significant results.  So the conlcusion of the paper that TV
coverage of soap operas reduces births is only true when we apply the weighting
factors.  The effects of globo is stronger in districts with more population.
So the coefficient is not evenly distributed across district population size.
Split the sample to show this further:  
*/

// split the sample into more and less populous districts
sum pop, detail 
scalar median = r(p50)

areg birth globo $controls i.year if pop<median, absorb(amc_code) cluster(amc_code)
est store e3a_alt1

areg birth globo $controls i.year if pop>median, absorb(amc_code) cluster(amc_code)
est store e3a_alt2
// compare the results
esttab e3a_alt1 e3a_alt2, b(%9.4f) se(%9.4f) stat(N r2) drop(*.year) label


********************************************************
* Q.3 Compare Standard errors

reg birth globo $controls i.year
est store e6a_pols
* compute errors from pooled OLS, can calculate residuals directly with option r
predict error_pols, r

* cluster
reg birth globo $controls i.year, cluster(amc_code)
est store e6a_pols_clustered

* add fixed effects
areg birth globo $controls i.year, absorb(amc_code)
est store e6a_fe
predict error_fe, r
* note this compares to e6a from above

esttab e6a_pols e6a_pols_clustered e6a_fe e6a, b(%9.4f) se(%9.4f) stat(N r2) drop(*.year) label

// let us compare the error terms on the regional level
sum error_pols error_fe // we impose them to be zero on average

tab uf_code, sum(error_pols)
tab uf_code, sum(error_fe)

* want to check for structure in the variances (e.g. heterosked.)
* are the variances different depending on the geographic region?
// plot of residuals across uf regions
scatter error_pols uf_code
scatter error_fe uf_code
* within these clusters the errors are not independent, but are correlated
* so if we use clustered errors, we are getting more accurate s.e.'s
* this is why we cluster on the district level

********************************************************
* Q.4 Generating a new exposure measure
use "$datadir/LCD_new", clear
drop if large!=0

sort id year
br id year yr1stcov age globo yrsexp*
* call another do file, Stata will run the code in the do file (the age calc)
do "$datadir/corrected_exposure.do"
* new variables have underscore
br id year yr1stcov age globo _yrsexp*

// HOMEWORK: Comment on the exposure file, briefly describe which steps the 
// code implements 
/*  The original data file has incorrect globo exposure levels in each age 
cohort level - they mostly seem to sum to 10 rather than the true value.

The corrected do file does the following:
1) Calculates the total years of exposure to Globo
2) Identify which 10 year age cohort each person is currently in
3) Set up a counter "globocoverage" starting at 1 in the year the globo
   coverage becomes available (1 year lag) 
4) Assign the counter to the current cohort group (i.e. 10to19 or 20to29 etc)
5) Replace the counter in the cohort group if total exposure years is less than
   the total number of years being in the age cohort group
6) Sum up the rowtotals of exposure (the counters for each cohort, only 1 cohort
   for each person so far) and calculate the extra years of exposure if total
   exposure years exceeds the cohort counter (we will use this value to calc.
   the exposure for all preceeding cohorts in steps 7 through 9, iteratively).
   This extra exposure is the total expousure that falls into prior age cohorts.
7) The 40-49cohort is already correct, so begin with the 30-39 cohort and 
   using the extra exposure value, fill in the exposure amounts for this cohort.
8) Repeat steps 6 and 7 for the 20-29 cohort, recalculating the extra exposure
   for the earlier age cohorts if any remains.
9) Repeat steps 6 and 7 for the 10-19 cohort.
We now should have correct exposure levels for each cohort via this iterative
backward process.
*/

********************************************************
* Q.5 Re-estimate columns 4 and 5 of table 4

// estimation with wrong variable, to match column 4 in paper
areg birth yrsexp1019 yrsexp2029 yrsexp3039 $controls i.year ///
 [aweight=weight], absorb(amc_code) cluster(amc_code)
 est store e1
sum age if e(sample)
// wrong sample!

// imposing the right sample, still wrong exposure levels
areg birth yrsexp1019 yrsexp2029 yrsexp3039 $controls i.year ///
 [aweight=weight] if age>=30 & age<=49, absorb(amc_code) cluster(amc_code)
 est store e2
sum age if e(sample)

//right variable
areg birth _yrsexp1019 _yrsexp2029 _yrsexp3039 $controls i.year ///
 [aweight=weight], absorb(amc_code) cluster(amc_code)
 est store e3
sum age if e(sample)
// right variable with constraint
areg birth _yrsexp1019 _yrsexp2029 _yrsexp3039 $controls i.year ///
 [aweight=weight] if age>=30 & age<=49, absorb(amc_code) cluster(amc_code)
 est store e4
sum age if e(sample)

esttab e1 e2 e3 e4, b(%9.4f) se(%9.4f) stat(N r2) label ///
 keep(yrsexp* _yrsexp*)
 
 // HOMEWORK: instead of instruction on paper, replicate column 5 of table 4
 // discuss your findings, what do you think about the validity of the paper
 // in light of these findings? 
 * do you throw out whole paper, a part of it? what do you do with it?

// match column 5, but still wrong exposure levels and wrong sample selection
areg birth yrsexp2029 yrsexp3039 yrsexp4049 $controls i.year ///
 [aweight=weight], absorb(amc_code) cluster(amc_code)
 est store e5match
sum age if e(sample)
 
// redo column 5, correcting exposure levels and sample
areg birth _yrsexp2029 _yrsexp3039 _yrsexp4049 $controls i.year ///
 [aweight=weight] if age>=40 & age<=49, absorb(amc_code) cluster(amc_code)
 est store e5corr
sum age if e(sample)

* compare the paper results with the corrected results
esttab e5match e5corr, b(%9.4f) se(%9.4f) stat(N r2) label ///
 keep(yrsexp* _yrsexp*)
 
 /*
 Table 4's results as published in the paper are certainly invalid and should
 be corrected, with the the conclusions revised.  
 For column 5, the effects of exposure in the 20 to 29 and 30 to 39 cohort 
 change from being significant to being insignificant, both economically and 
 statistically.  Exposure to globo during these years for women in their 40's
 doesn't have any impact on fertility.  Exposure in their 40's does have some 
 small and significant impact.
 In column 4, the effects of exposure in the 30-39 and 40-49 ages are well
 overestimated, especially for the latter.
 
 Table 4 does have some serious mistakes, and should be corrected.  But some
 of the earlier results with regards to effects of coverage on fertility are 
 interesting and useful, and the discussion of heterogeneity of effects with 
 regards to wealth and education are useful.
 
 I would argue that the paper should be republished with a corrected table 4
 and corrected discussion of the heterogeneity of effect.  This makes the 
 conclusion and impact less broad, but nonetheless still applicable in a 
 narrower sense and still interesting.
 */
 
********************************************************
* Q.6 Replicating Figure 4 by collecting coefficients

// setting up the exact years for the coverage
gen t_0 = (year==yr1stcov)
forvalues k = 1/9 {
gen t_lag_`k' = (year==yr1stcov-`k')
gen t_lead_`k' = (year==yr1stcov+`k')
}
 
// running equation (4)
areg birth t_* $controls i.year [aweight=weight], absorb(amc_code) ///
cluster(amc_code)
* need coefficients, signficance bands and years
 
// collecting coefficients
gen coefyear = 0 in 10 // in row 10, we have a 0
gen coef = _b[t_0] in 10
gen lb = _b[t_0] - _se[t_0]*1.645 in 10 // 10% significance interval is 1.645
gen ub = _b[t_0] + _se[t_0]*1.645 in 10

* fill in remaining parts of the loop, _n is the row number
forvalues k = 1/9 {
replace coefyear = - `k' if _n==10-`k'
replace coefyear = `k' if _n==`k'+10

replace coef = _b[t_lag_`k'] if coefyear==-`k'
replace coef = _b[t_lead_`k'] if coefyear==`k'

replace lb = _b[t_lag_`k'] - _se[t_0]*1.645 if coefyear==-`k'
replace lb = _b[t_lead_`k'] - _se[t_0]*1.645 if coefyear==`k'

replace ub = _b[t_lag_`k'] + _se[t_0]*1.645 if coefyear==-`k'
replace ub = _b[t_lead_`k'] + _se[t_0]*1.645 if coefyear==`k'

}

twoway (line coef coefyear) (line lb coefyear) (line ub coefyear)

// NOTE: don't need to hand in graph as stated on homework, do the other task
 
* close the log file 
log close
