* Development Microeconometrics using STATA
* Solution for Session 12: Railroads and Growth in Prussia, Part 2
* Robert Leonard

version 15 

set more off, permanently // do not stop after each page

********************************************************
* Setting up the directories

global datadir "C:\Users\roble\Desktop\Working\STATA\HW 12\HW12 Data"
global workdir "C:\Users\roble\Desktop\Working\STATA\HW 12"

********************************************************

capture log close 
log using "$workdir/leonard_exc12.log", replace

********************************************************
* Q.1 Open and inspect the dataset

use "$datadir\hornung-rail-cross-section_new.dta", clear

global control street1848_dummy ship1849_dummy ln_pop1849civil ///
ln_pop1849military fac1849_total2_pc county_mining county_landownership ///
pop1849_young_pc edu1849_pri_enrol dist1

global exantecontrols ln_pop1837civil gr_1821_1837 occ1819_merch_pc ///
 loom1819_city_pc pop1816_prot_pc buil1821_home_city_pc buil1821_fac_city_pc ///
 ln_assu1821_value_city_ph

********************************************************
* Q.2 Logit and Linear Probability Model

reg rail1848 $exantecontrols 
margins, dydx(gr_1821_1837) atmeans
* with nonlinear models, we get different marginal effect
logit rail1848 $exantecontrols 
margins, dydx(gr_1821_1837) atmeans
margins, dydx(gr_1821_1837) at(ln_pop1837civil==1.2)

logit rail1848 gr_1821_1837
margins, dydx(gr_1821_1837) atmeans
* check by hand
scalar beta = 35.40957 
scalar betax = beta*.0124133  - 2.636866
scalar p = exp(betax)/(1+exp(betax)) 
di beta*p*(1-p)
// if you want to compute Delta(x) absolute deriv. instead of del(x) partial
// deriv., you should commpue the difference in probabilities between the two 
// values.  example: x=0.01 to x=0.015
scalar betax1 = beta*.01  - 2.636866
scalar p1 = exp(betax1)/(1+exp(betax1)) 
scalar betax2 = beta*.015  - 2.636866
scalar p2 = exp(betax2)/(1+exp(betax2)) 
di " first probability " p1 " second probability " p2 " and the difference " (p2-p1)

// marginal at 0.01 should be close to the average from moving between 0.01 and
// 0.011.
di beta*p1*(1-p1) " this is the marginal"
scalar betax3 = beta*.011  - 2.636866
scalar p3 = exp(betax3)/(1+exp(betax3))
di (p3-p1)/(0.011-0.01) " this is the effect of going from 0.01 to 0.011" 


// HOMEWORK: Use the linear probability model and the logit model with the 
// full specification to answer: What is the marginal effect for a city that 
// grows annualy with 5% between 1821 and 1837 (i.e. gr_1821_1837 = 0.05)?
* for the Linear probability model
reg rail1848 $exantecontrols 
margins, dydx(gr_1821_1837) at(gr_1821_1837==0.05)
* so 2.25% increased chance of having rail access

* for the logit model
logit rail1848 $exantecontrols 
margins, dydx(gr_1821_1837) at(gr_1821_1837==0.05)
* so 7.24%

********************************************************
* Q.3 Implement PSM 

ssc install psmatch2

// implement PSM using psmatch2
psmatch2 rail1848 $exantecontrols if node1848==0 & addrail71==0 & areachange1==. ///
 & areachange2>1871, kernel kerneltype(normal) logit common out(gr_1849_1871)

* try to match the propensity score using logit (should get same table in theory)
logit rail1848 $exantecontrols if node1848==0 & addrail71==0 & areachange1==. ///
 & areachange2>1871 & gr_1849_1871!=.

* how to get propensity score - use predict, it's simply the probability of 
* being treated. also compare from psmatch2
predict pscore // really should be constraining predict here
corr pscore _pscore
sum pscore _pscore if node1848==0 & addrail71==0 & areachange1==. ///
 & areachange2>1871 & gr_1849_1871!=.

 // check the distribution of the PScore and the common support
psgraph, bin(100)
 
********************************************************
* Q.4 Treatment effects of the PSM

// we can simply generate weighted averages
mean gr_1849_1871 $exantecontrols [pweight=_weight] if node1848==0, over(rail1848) 

* since it's weighted, we cant simply use the t-test
reg gr_1849_1871 rail1848 [pweight=_weight]

reg gr_1831_1837 rail1848 [pweight=_weight]
// note: matching is always base on observables!  Therefore, unobserved
// heterogeneity cannot be taken into account.  
 
********************************************************
* Q.5 PSM and IV: Table 8 Panel B, columns (1) and (2)

ivreg2 gr_1831_1837 (rail1848=slc1848) gr_1816_1831 $control if node1848==0 ///
 & areachange1==. [pweight=_weight], cluster(kreiskey1849) 

ivreg2 gr_1849_1871 (rail1848=slc1848) gr_1831_1837 $control if node1848==0 ///
 & areachange2>1871 [pweight=_weight], cluster(kreiskey1849) 
 
********************************************************
* Q.6 Panel estimates, table 9, columns (1) and (2)

use "$datadir\hornung-rail-panel-city.dta", clear

xtset townkey1849 year

gen ln_popcivil = ln(popcivil)
gen ln_popmilitary = ln(popmilitary)
* lots of missing observations (probably a lot of zeros, maybe add tiny numer
* so ln operation doesn't lose the observation)
replace ln_popmilitary=ln(popmilitary+0.01)
* drop observations with too large city growth rates
bysort townkey1849: gen gr_civil = (ln_popcivil - ln_popcivil[_n-1])/(year-year[_n-1])
drop if abs(gr_civil) > 0.1
* this also drops the missings

xtreg ln_popcivil railaccess ln_popmilitary distance areachange i.year  ///
 if year>=1840 & year<=1861, cluster(kreiskey1849)

xtreg ln_popcivil railaccess ln_popmilitary distance areachange i.year  ///
 if year>=1840 & year<=1861, cluster(kreiskey1849) fe
 
********************************************************
* Q.7 Table 9, columns (11) and (12)

// HOMEWORK: estimate the model with xtivreg2 to replicate columns (11) and (12)
// and briefly interpret your findings.

ssc install xtivreg2

* column (11) and (12)
* initial coding below did not work
* xtivreg2 ln_popcivil (railaccess=slc) ln_popmilitary distance areachange ///
* node i.year if year>=1840 & year<=1861, cluster(kreiskey1849) fe first
 
* problem with i.year in xtivreg2, so perhaps break out the year into 
* individual dummy variables and add those to the regression using y18*

tab year
forvalues k = 1840(3)1861{
di `k'
gen y`k'=0
replace y`k'=1 if year==`k'
}

xtivreg2 ln_popcivil (railaccess=slc) ln_popmilitary distance areachange ///
 y18* node if year>=1840 & year<=1861, cluster(kreiskey1849) fe first

* careful on the interpretation, as we only have data every 3 years 

/*  This gets pretty close to the estimates in table 9.
For first stage regression, the instrument has a coefficient of .475 in the
paper vs .473, a difference of only 2/1,000ths.  The total observations is 
different, and may be causing some noise.  The error of .06 in the paper is 
matched.  The statistical significance is strong in both results.
The high coefficient, large F stats, and large Cragg-Donald and Kleibergen-Papp
stats show that the instrument is not weak and can be used to address 
endogeneity questions arrising from the earlier estimates.  The paper also 
mentions that the SLC instrument is time variant as well, as new nodes are 
connected, new SLC's also are defined, adding to its strength as an IV.

For the second stage, railroad access in the table shows an extra 7.2% growth
over a 3 year time interval, so about 2.4% extra growth per year from having
rail access.  This regression shows 7.9% over 3 years, which is approximately
2.6% annual growth, so the results match up fairly well with the table.
The differences may come from the small difference in total observations (11).
Errors are pretty close as well, 0.040 in the table vs 0.043 here.  The paper
shows that the rail access is significant at the 10% level and this regression
shows the same.
*/ 
 
* close the log file 
log close
