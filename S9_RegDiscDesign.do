* Development Microeconometrics using Stata
* Example solution for session 9
* Volker Lindenthal

version 15 // You should always specify the version of Stata that 
// you are working with

set more off, permanently // Stata does not stop after each page

********************************************************
// Setting up the directories
global datadir "L:\Development Microeconometrics using Stata\data"
global workdir "T:\Volker Lindenthal"

********************************************************
capture log close
log using "$workdir/lindenthal_exc9.log", replace

********************************************************
// Q.1 Getting familiar with the dataset

use "$datadir/munic.dta", clear
sum 

// generate a treatment variable
gen treatment = (voters96>40500)

// # of voters above the threshold
gen treat_voters96 = (voters96-40500)*treatment

// compute the distance to the cut-off
gen bandwidth = voters96-40500
replace bandwidth = -1*bandwidth if treatment==0

// alternative, smarter way:
gen bandwidth2 = abs(voters96-40500)


histogram bandwidth if bandwidth<20000

twoway (histogram bandwidth if bandwidth<20000 & treatment==0) ///
(histogram bandwidth if bandwidth<20000 & treatment==1, color(red))

********************************************************
// Q.2 Generate all the binned values for figure 2 and 3

sum voters96
// important: we need to construct bins below and above 40500, 
// no bin should include 40500 itself

egen bin_voters96=cut(voters96), at(500(4000)200500)

// change the mean of the bins to the midpoint
replace bin_voters96=bin_voters96+2000

// compute the means for each bin for the variables needed
 
foreach x of var r_util98 r_util02 r_util94 attend regist {
egen bin_`x' = mean(`x'), by(bin_voters96)
}

// a bit of labeling
label var bin_r_util94 "Valid Votes/Turnout - 1994 Election (Paper Only)"
label var bin_r_util98 "Valid Votes/Turnout - 1998 Election (Discontinuity)"
label var bin_r_util02 "Valid Votes/Turnout - 2002 Election (Electronic Only)"
label var bin_attend "Turnout/Registered Voters"
label var bin_regist "Registered Voters/Total Population"

********************************************************
// Q.3 Replicating figure 2 and 3

global lb = 500
global ub = 80000

twoway scatter bin_r_util94 bin_voters96 if voters96<$ub & voters96>$lb, ///
 mc(green) ms(square) msize(small) || ///
scatter bin_r_util98 bin_voters96 if voters96<$ub & voters96>$lb, mc(blue) || ///
scatter bin_r_util02 bin_voters96 if voters96<$ub & voters96>$lb, mc(red) ///
ms(triangle) || ///
qfit bin_r_util94 voters96 if voters96<40500 & voters96>$lb, lc(green) || ///
qfit bin_r_util94 voters96 if voters96>40500 & voters96<$ub, lc(green) || ///
qfit bin_r_util98 voters96 if voters96<40500 & voters96>$lb, lc(blue) || ///
qfit bin_r_util98 voters96 if voters96>40500 & voters96<$ub, lc(blue) || ///
qfit bin_r_util02 voters96 if voters96<40500 & voters96>$lb, lc(red) || ///
qfit bin_r_util02 voters96 if voters96>40500 & voters96<$ub, lc(red) xline(40500)

twoway (histogram voters96 if voters96>20000 & voters96<40500, bin(32) ///
xline(40500) frequency) (histogram voters96 if voters96>40500 & voters96<61000, ///
bin(32) xline(40500) frequency)

********************************************************
// Q.4 Investigating the differences at the threshold statistically

foreach x of var income gini latitude longitude illiter less4 less8 ///
population91 population00 urbanization {
reg `x' treatment voters96 treat_voters96 if bandwidth<10000, r
est store b_`x'
reg `x' treatment voters96 treat_voters96 if bandwidth<5000, r
est store c_`x'
}

est table b_*, keep(treatment) b(%9.3f) se(%9.3f) p(%9.3f)
est table c_*, keep(treatment) b(%9.3f) se(%9.3f) p(%9.3f)

// no differences in other variables at the threshold (good sign)

********************************************************
// Q.5 Re-estimating panel A of table 2


// back out population 
gen population = voters96 / regist
// is it for 1996? maybe also 1991


foreach x of var r_util98 attend regist population {
reg `x' treatment voters96 treat_voters96 if bandwidth<10000, r
est store b2_`x'
reg `x' treatment voters96 treat_voters96 if bandwidth<5000, r
est store c2_`x'
}

est table b2_*, keep(treatment) b(%9.3f) se(%9.3f) p(%9.3f)
est table c2_*, keep(treatment) b(%9.3f) se(%9.3f) p(%9.3f)


********************************************************
// Q.6 Placebo with other thresholds

// simply copy/paste the code up to here in a new do-file
// search and replace 40500 by 70000 or to a global macro
// save it under placebo.do
// global threshold = 70000
// do "$workdir/placebo.do"

********************************************************
// Q.7 Investigating the impact on the political system

// Table 2, Panel C

reg right treatment right_state right_meas voters96 ///
treat_voters96 if bandwidth<10000, cl(uf)
reg right treatment right_state right_meas voters96 ///
treat_voters96 if bandwidth<5000, cl(uf)


********************************************************
// Q.8 results on illiteracy rate

// Table III
sum illiter, detail
// not the right median

sum illiter if bandwidth<20000, detail
local median =r(p50)

reg r_util98 treatment voters96 treat_voters96 ///
if bandwidth<10000 & illiter>`median'
estimates store a
reg r_util98 treatment voters96 treat_voters96 ///
if bandwidth<10000 & illiter<`median'
estimates store b
// coefficients seem to match

suest a b, r
lincom [a_mean]treatment - [b_mean]treatment

sum illiter if bandwidth<20000, detail
sum illiter if bandwidth<10000, detail

sum illiter if bandwidth<15000, detail
local median =r(p50)

reg r_util98 treatment voters96 treat_voters96 ///
if bandwidth<10000 & illiter>`median'
estimates store a
reg r_util98 treatment voters96 treat_voters96 ///
if bandwidth<10000 & illiter<`median'
estimates store b
// coefficients seem to match

suest a b, r
lincom [a_mean]treatment - [b_mean]treatment


log close
