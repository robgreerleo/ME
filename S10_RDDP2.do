* Development Microeconometrics using STATA
* Solution for Session 10: Fujiwara (2015) Voting Technology, Part 2
* Robert Leonard

version 15 

set more off, permanently // do not stop after each page

********************************************************
* Setting up the directories

global datadir "C:\Users\roble\Desktop\Working\STATA\HW 10\HW10 Data"
global workdir "C:\Users\roble\Desktop\Working\STATA\HW 10"

********************************************************

capture log close 
log using "$workdir/leonard_exc10.log", replace

********************************************************
* Q.1 Data at the State level

use "$datadir/state.dta", clear
br  // state level panel

xtdes // test properties of the panel - is it balanced? missing obs?
* we have a fully balanced panel - all 1's, at full rank
//uf_id is the state level and t is the time period
summarize
* check interactions
br uf year inst* // label of inst not correct, it is not an interaction
// check distribution of inst in 1998 ( = S in the paper)
histogram inst if year==1998, bin(27)
// not all S are = 0.5, there is some variation that we can exploit

********************************************************
* Q.2 Estimation of Equations 6 & 7 from Table 4

// estimating (6) for the first coefficient in column (1)
reg d.util_rate inst98e reg2-reg5 if year==1998, r // cl(uf) does the same
* as robust std errors, clustered errors are always robust 
* doesnt matter if we use interatction or just inst, only estimating for 1 year
* controls (X in equation) are the regions
* ,r stands for robust standard errors (small sample, so may have issues)
* d. is the difference operator - same as l., difference from previous value
* or use util_rate - util_rate[_m-1], or the value of the cell above it

// estimating (7)
reg d.util_rate inst02e reg2-reg5 if year==2002, r

// corresponding fixed effect regressions
areg util_rate inst98e delec2 reg2_98-reg5_98 if year!=2002, cl(uf) absorb(uf)

// the original values in the paper come from a partialling out procedure
// 1) Regress the vote share on the controls
reg d.util_rate reg2-reg5 if year==1998, r
// predict residuals from regression
predict res_util_rate_98, resid
// 2) regress the electronic vote share on the controls
reg inst98e reg2-reg5 if year==1998, r
// predict residuals
predict res_inst98e, resid
// 3) Finally, we can regress residuals on residuals
reg res_util_rate_98 res_inst98e if year==1998, r

// HOMEWORK: Think about a reason why the standard errors come out differently
// Which ones are the "right" ones that you would choose?
* Please refer to the pdf for the response.

/* A general challenge is that we do not have many observations to draw
statistical conclusions.  On top of this we have few clusters and for each
cluster, we have only 1-3 observations.  One proposed way to solve the problem
is to implement bootstrapping. */

********************************************************
* Q.3 Some basic bootstrapping

/* Bootstrapping is a method of sampling with replacement.  Instead of assuming
a theoretical distribution for the error terms (e.g. normal), the actual
sampling distribution is constructed by drawing out of the sample over &
over again.  A way to think about this procedure of bootstrapping is that you 
estimate your move 1, store the error terms, re-shuffle them to generate new data
points, and estimate the model once again.  You do this over and over again
to generate a distribution of your coefficients.  

 It is useful to specify a seed to always generate the same randomness; the idea
 is to generate the same random numbers on all computers.  (check seed help for
 more information) 
 
 For bootstrapping, you should use a large number of repetitions, here we will
 use 1000.
 
* We will need cgmwildboot
ssc install clustse
ssc install unique
// cgmwildboot doesn't allow using d.
gen dif_utl_rate = d.util_rate

cgmwildboot dif_utl_rate inst98e reg2-reg5 if year==1998, cl(uf) ///
bootcluster(uf) seed(1000) reps(1000)

Program not working, skip for now
*/

********************************************************
* Q.4 Scatter Plots of residuals after partialling out

twoway scatter res_util_rate_98 res_inst98e if year==1998, ms(circle) ///
mc(blue) || lfit res_util_rate_98 res_inst98e if year==1998, color(red)

graph export "$workdir\graph1.pdf", replace 

// generate additional residuals for 2002
// 1) Regress the vote share on the controls
reg d.util_rate reg2-reg5 if year==2002, r
// predict residuals from regression
predict res_util_rate_02, resid
// 2) regress the electronic vote share on the controls
reg inst02e reg2-reg5 if year==2002, r
// predict residuals
predict res_inst02e, resid

twoway scatter res_util_rate_02 res_inst02e if year==2002, ms(circle) ///
mc(blue) || lfit res_util_rate_02 res_inst02e if year==2002, color(red)

graph export "$workdir\graph2.pdf", replace 


// Residual scatter plots help to reduce the dimensionality and let you check
// for outliers + you get a sense of the quality of your estimation.

********************************************************
* Q.5 Estimating coefficients for column (3) of Table 4

// equation (8)
reg util_rate inst98e delec* reg* duf*, cl(uf)
//other ways to specify the same regression with fixed effects
areg util_rate inst98e delec* reg*, absorb(uf) cl(uf)
areg util_rate inst98e delec2 reg2_98-reg5_98, cl(uf) absorb(uf)

********************************************************
* Q.6 Estimating coefficients for column (4) of Table 4

// HOMEWORK: run the regression for column (4).  To do so you have to set up
// an additional variable, which changes the equation of regression (8) in
// such a way that the estimated coefficient corresponds to 
// (theta_98+theta_02)/2

* To replicate column 4, we need the electronic voter proportion interaction, 
* S in each row.  We also need the change in turnout between 1994-1998 and 
* from 1998-2002, so define a new util3 variable to capture these amounts
* in the correct rows.

gen evprop = 0
replace evprop = inst98e if year == 1998
replace evprop = -(1-inst02e) if year == 2002

gen util3 = 0 if year==1994
replace util3 = d.util_rate if year == 1998
replace util3 = d.util_rate if year == 2002

reg util3 evprop delec* reg* duf*, cl(uf)

* To match the std error, we need to follow the same partialing out procedure
* from question 2 above. 
// 1) Regress the vote share on the controls
reg util3 delec* reg* duf*, cl(uf)
// predict residuals from regression
predict util3resid, resid
// 2) regress the electronic vote share on the controls
reg evprop delec* reg* duf*, cl(uf)
// predict residuals
predict evpropresid, resid
// 3) Finally, we can regress residuals on residuals
reg util3resid evpropresid, cl(uf)
* this now matches column 4

********************************************************
* Q.7 Generating a plot to investigate the impact of outliers

// Loop eq (8), always leaving out one state.  Store the coefficients, lower
// bound and upper bound.  

gen coef = .
gen lb = .
gen ub = .

lab var coef "Coefficient, leaving out one state"
lab var lb "Lower bound of 95% CI"
lab var ub "Upper bound of 95% CI"

forvalues uf=1/27 {
local line = `uf'*3
  reg util_rate inst98e duf2-duf27 delec2 delec3 reg?_* if uf_id!=`uf', cl(uf)
  replace coef = _b[inst98e] in `line'
  replace lb = _b[inst98e] - _se[inst98e]*1.96 in `line'
  replace ub = _b[inst98e] + _se[inst98e]*1.96 in `line'
}

// using the initial equation (8) as benchmark
qui reg util_rate inst98e duf2-duf27 delec2 delec3 reg?_*, cl(uf)
local effect = _b[inst98e]

scatter coef uf_id, xlabel(1/27, labsize(small)valuelabel) || ///
rcap lb ub uf_id, legend(off) xtitle("") ylabel(0(0.05)0.15) yline(`effect', lcolor(red))
 
graph export "$workdir\graph3.pdf", replace

********************************************************
* Q.8 Health Care Spending 

use "$datadir/yearly.dta", clear

areg ssaude inst1996-inst2006 reg* if year_f>1994 & year_f<2007, rob a(uf)
 gen te_s = 0 if year_f==1995
 forvalues i = 1996(1)2006 {
  replace te_s = _b[inst`i'] if year_f==`i'
 }
 
sort year_f
scatter te_s year_f if year_f>1994 & year_f<2007, connect(l) ytitle("Coefficient on S") ///
 xline(1998.5) xline(2002.5) xlab(1995(1)2006) xtitle("")

graph export "$workdir\graph4.pdf", replace


* close the log file 
log close
