* Development Microeconometrics using STATA
* Solution for Session 11: Railroads and Growth in Prussia, Part 1
* Robert Leonard

version 15 

set more off, permanently // do not stop after each page

********************************************************
* Setting up the directories

global datadir "C:\Users\roble\Desktop\Working\STATA\HW 11\HW11 Data"
global workdir "C:\Users\roble\Desktop\Working\STATA\HW 11"

********************************************************

capture log close 
log using "$workdir/leonard_exc11.log", replace

********************************************************
* Q.1 Data - Generate Pop Growth Rates

use "$datadir\hornung-rail-cross-section.dta", clear

// compute population growth rates
* for loops, you can't just indicate use the next or previous variable
* but you can use a local/global macro looping over all the years, always
* overiding the initial value with a new one
local priorperiod = 1816
foreach k of numlist 1821 1831 1837(3)1867 1871 1880 1885 {
gen gr_`priorperiod'_`k' = ( ln(pop`k'civil)-ln(pop`priorperiod'civil) ) /  ///
(`k'-`priorperiod')
local priorperiod = `k'
}  

********************************************************
* Q.2 Match Summary Statistics of Table 2

tabstat gr_*, by(rail1848) stats(mean sd N)
* need to partition the sample into 3 groups, not 2. create a new indicator
gen indi_sum = 0 if node1848==1
replace indi_sum = 1 if rail1848==1 & node1848==0
replace indi_sum = 2 if rail1848==0 & node1848==0
tabstat gr_*, by(indi_sum) stats(mean sd N)
// why are there still differences? we are excluding some observations 
foreach x of var gr_* {
replace `x' = . if abs(`x')>=0.1
}
tabstat gr_*, by(indi_sum) stats(mean sd N)

tab node1848 rail1848
list city railyear if node1848==1 & rail1848==0

tabstat gr_* if city!="Waldenburg", by(indi_sum) stats(mean sd N)
* use estpost as we have a lot of observations to ttest
estpost ttest gr_* if node1848==0, by(rail1848)
* note 259 cities get treatment at some point, something to consider for table2
count if railyear>1848 & railyear<1872
// If you are bored, you could set up an alternative growth rate for the 
// re-classified cities and re-do the analysis (partition the sample into 4
// groups).

// HOMEWORK: Do the results change under scenario (a) or (b)? see problem set
* exclude all observations that have area changes prior to 1870 (areachange1,2)
* use listwise option (constrains to the same cities all the time)
* does the same pattern hold?

* Option a - remove incorporations prior to 1872 (areachange1 = incorporations
* prior to 1938, areachange2 = inc. after 1837
tab areachange1 indi_sum 
tab areachange2 indi_sum
* most are missing values in areachange1, all others recoded 1900 for areachange2
* if they aren't coded as incorporating in earlier years.  missing values for
* areachange1 are treated as large numbers in tabstat
tabstat gr_* if city!="Waldenburg" & areachange1>=1872 & areachange2>=1872, ///
 by(indi_sum) stats(mean sd N)
estpost ttest gr_* if node1848==0 & areachange1>=1872 & areachange2>=1872, by(rail1848)
/* No, the results do not significantly change for the t-tests if we exclude
incorporations before 1872.  The magnitudes and statistical significance levels
pretty much remain the same.  The 1840-43 growth rate is the only somewhat 
notable change, with the t value increasing towards greater statistical signif.
from 1.6 to 1.88 but this is already in line with the paper's analysis as the 
difference in mean growth between railroad and nonrailroad cities grows here.
*/

* Option b, use listwise command to remove missing values
estpost ttest gr_* if node1848==0, by(rail1848) listwise
/* Listwise deletion removes the sample if any of the variables has a missing
value (for each growth rate period comparison).  Here we remove anywhere from 
49 to 103 samples depending on the period, which is much more than those
eliminated in option a above.  
Nevertheless, the results don't change too much.  In general, the difference 
between the means is reduced somewhat.  A notable change is in the 1858 to
1861 period, where the difference in mean growth rate is no longer statistically
significant.  1846 to 1849 also moves more towards not being statistically
significant.  Otherwise, the rest of the periods from 1843 on do show a difference
in mean growth rates that are still statistically significant and in line with
the paper's analysis and conclusions.
*/

********************************************************
* Q.3 Replicate Table 3

global control street1848_dummy ship1849_dummy ln_pop1849civil ///
ln_pop1849military fac1849_total2_pc county_mining county_landownership ///
pop1849_young_pc edu1849_pri_enrol dist1

tabstat slc1848 $control if city!="Waldenburg", by(indi_sum) stats(mean sd N)

estpost ttest slc1848 $control if node1848==0, by(rail1848)

********************************************************
* Q.4 Estimate OLS 

// re-estimate panel A for columns 1 and 2 of Table 5
// generate city growth rates
gen gr_1816_1831 = ( ln(pop1831civil)-ln(pop1816civil) ) / (1831-1816)
* ignore those that grew or declined more than 10%
replace gr_1816_1831 = . if abs(gr_1816_1831)>=0.1

gen gr_1849_1871 = ( ln(pop1871civil)-ln(pop1849civil) ) / (1871-1849)
replace gr_1849_1871 = . if abs(gr_1849_1871)>=0.1

// column (1), note std errors are clustered at county level - kreiskey
* also ignore those that had areachanges (incorporations, sb = nonmissing)
reg gr_1831_1837 rail1848 gr_1816_1831 $control if node1848==0 ///
 & areachange1==., cluster(kreiskey1849)
// # obs difference of 2 stems from 10% constraint on growth, they added
// and & abs()<0.1 in the if condition instead of resetting to missing.
// only constrained on current period growth rate, contradicts in the control

// column(2), addrail is now a control, also change areachange to 2
// note these were recoded, so cannot just use missing, but larger, and larger
// includes the missing
reg gr_1849_1871 rail1848 addrail71 gr_1831_1837 $control if node1848==0 ///
 & areachange2>1871, cluster(kreiskey1849)

********************************************************
* Q.5 IV Regression

// column (1) of panel B and C
ivreg2 gr_1831_1837 (rail1848=slc1848) gr_1816_1831 $control if node1848==0 ///
 & areachange1==., cluster(kreiskey1849) first

// column (2) of panel B and C
ivreg2 gr_1849_1871 (rail1848=slc1848) addrail71 gr_1831_1837 /// 
 $control if node1848==0 & areachange2>1871, cluster(kreiskey1849) first

// HOMEWORK: use slc1848_lcp_1_5km (least cost path) as an alternative 
// instrument and briefly comment on it, whether other instruments would
// predict the same.

// column (1) of panel B and C
ivreg2 gr_1831_1837 (rail1848=slc1848_lcp_1_5km) gr_1816_1831 $control ///
if node1848==0 & areachange1==., cluster(kreiskey1849) first

// column (2) of panel B and C
ivreg2 gr_1849_1871 (rail1848=slc1848_lcp_1_5km) addrail71 gr_1831_1837 /// 
 $control if node1848==0 & areachange2>1871, cluster(kreiskey1849) first

/* The overall general results do not change by using the new IV of least 
cost corridor, but the magnitude of the effect of railroad connection on growth
as well as the statistical significance, declines (magnitude of effect from 
2.1% to 1.96%, and lower significance level but still strongly significant).

The new IV is a weaker instrument, due to much lower F-values and weak 
identification tests like Cragg-Donald or Kleibergen-Paap Wald.  This can 
also be seen in the first stage IV regression with the coefficient dropping from
.53 to .34 using the new IV of least cost path.  One reason for this may be due
to the larger corridor area.  The original IV uses a corridor of 1 km, and the
new IV expands the corridor to 1.5km.  

The exclusion restriction, that the covariance between the instrument and the
error term 0 or close to it cannot be tested.  But given this is a wider 
corridor, and also a weaker instrument, it is very possible that this 
restriction wouldn't hold for this IV.

It seems the corridor measurement, whichever is used, is correlated with 
several other city characteristics like roads, mining, higher ex ante 
populations and commmercial density, so other instruments likely have a similar 
problem.
*/ 

********************************************************
* Q.6 

// HOMEWORK: Restimate column (1) and (2) for OLS + IV
// constrain on those that never incorporated other areas through 1871
// want first and second estimation sample to be the same
// we want to have the same sample in both and briefly comment on if its the
// same or if we see differences. 

* Column 1 panel A
reg gr_1831_1837 rail1848 gr_1816_1831 $control if node1848==0 ///
 & areachange1==. & areachange2>=1872 ///
 & gr_1849_1871!=. & addrail71!=. & gr_1831_1837!=., cluster(kreiskey1849)

 * Column 1 panel B & C
ivreg2 gr_1831_1837 (rail1848=slc1848) gr_1816_1831 $control if node1848==0 ///
 & areachange1==. & areachange2>=1872  ///
 & gr_1849_1871!=. & addrail71!=. & gr_1831_1837!=., cluster(kreiskey1849) first

 * Column 1 panel D
reg gr_1831_1837 slc1848 gr_1816_1831 $control if node1848==0 ///
 & areachange1==. & areachange2>1871   ///
 & gr_1849_1871!=. & addrail71!=. & gr_1831_1837!=., cluster(kreiskey1849)

/*  Our sample size drops to 880.  There aren't any substantitive changes.
The most notable whould be the increase in the coefficient in the first stage
of the IV regression, panel B from 0.531 to 0.574 with stronger F stats and
stronger weak identification test results indicating that the IV is slightly
more powerful with this sample.  
Rail access 1838-48 does decline slightly from .002 to .00185 or .0019.
Otherwise, most of the coefficients only change by 1/1000, or .1% if at all.
Statitstical significance levels remain pretty similar.
*/

 * Column 2 panel A
reg gr_1849_1871 rail1848 addrail71 gr_1831_1837 $control if node1848==0 ///
 & areachange1==. & areachange2>=1872 ///
 & gr_1831_1837!=. & gr_1816_1831!=., cluster(kreiskey1849)

 * Column 2 panel B & C
ivreg2 gr_1849_1871 (rail1848=slc1848) addrail71 gr_1831_1837 /// 
 $control if node1848==0 & areachange1==. & areachange2>=1872   ///
 & gr_1831_1837!=. & gr_1816_1831!=., ///
 cluster(kreiskey1849) first

 * Column 2 panel D
reg gr_1849_1871 slc1848 addrail71 gr_1831_1837 $control if node1848==0 ///
 & areachange1==. & areachange2>=1872  ///
 & gr_1831_1837!=. & gr_1816_1831!=., cluster(kreiskey1849)

/*  We have the same sample size of 880.  The coefficients across the board
for all 4 panels do decline some, but not by a lot, mostly by 1/1000 or so
except for Panel B first stage IV declines from .553 to .541 and the same
with the weak identification numbers and F tests, indicating the IV is a little
weaker for this sample for the period in column 2, but not qualititatively.
Statistical significance levels remain pretty similar.  
So overall, qualitatively, not much changes with this slightly more restricted 
sample.
*/

********************************************************
* Q.7 Prior on Propensity Score Matching and Non-Linear Models

gen gr_1821_1837 = ( ln(pop1837civil)-ln(pop1821civil) ) / (1837-1821)
replace gr_1821_1837 = . if abs(gr_1821_1837)>=0.1

global exantecontrols ln_pop1837civil gr_1821_1837 occ1819_merch_pc ///
 loom1819_city_pc pop1816_prot_pc buil1821_home_city_pc buil1821_fac_city_pc ///
 ln_assu1821_value_city_ph

tabstat $exantecontrols if city!="Waldenburg", by(indi_sum) stats(mean sd N)

estpost ttest $exantecontrols if node1848==0, by(rail1848)
* if we had linear probability model
reg rail1848 $exantecontrols
* predict probabiltiy of being treated
predict pscore 

twoway histogram pscore if rail1848==1 || ///
histogram pscore if rail1848==0, color(red)

* Note: I'm not including a pdf of this chart for homework submission this week
* since we are finisishing this next session.

* close the log file 
log close
