* Development Microeconometrics using Stata
* Example solution for session 10
* Volker Lindenthal

version 14.1

global datadir "C:\Dropbox\University of Freiburg\Courses\Development Microeconometrics Using Stata\S10_F_2015\data"
global workdir   "C:\Dropbox\University of Freiburg\Courses\Development Microeconometrics Using Stata\S10_F_2015\solution"

cd "$datadir"

cap log close 
log using "$workdir/lindenthal_exc10.log", replace


*************************************************************************************************
// Q.1 Moving to the state level dateset

use state.dta, clear
br // looks like a panel

xtdes // ok, we have a balanced panel with three observations for the 27 states
// uf_id is the state identifier, t reports the time dimension

sum 
// duf* are state dummies, reg? are regional dummies, reg_?? are interactions of regional dummies and time,
// share* represents voting results for the political parties, delec* are dummies for the election years
// inst seems to represent S from the paper
histogram inst if year==1998

br uf year inst*
// label of inst not correct as it is not interacted with 1998

*************************************************************************************************
// Q.2 Estimation of equation (6) and (7)

// estimating equation (6) for the first coefficient in column (1)
// let us use clustered standard errors as described in the paper
reg d.util_rate inst98e reg2-reg5 if year==1998, cl(uf)
// which essentially boils down do robust standard errors (clustering does not really make any sense)
reg d.util_rate inst98e reg2-reg5 if year==1998, r
// not sure that the usage of robust standard errors makes really sense with this small sample;
// I would be very cautious...

// estimating equation (7) for the first coefficient in column (2)
reg d.util_rate inst02e reg2-reg5 if year==2002, cl(uf)


// an alternative way to estimate the regressions would be to implement fixed effects regressions
areg util_rate inst98e delec2 reg2_98-reg5_98 if year!=2002, cl(uf) absorb(uf)
areg util_rate inst98e delec2 reg2_98-reg5_98 if year!=1994, cl(uf) absorb(uf)


// coefficients seem to be correct, but what about the standard errors; what Fujiwara is actually doing is netting out
// all other effects and simply regressing the residuals of the valid vote share on the residuals of the S or (1-S) variable

// Let's try to do it for 1998. First we regress the vote share on the controls, ...
reg d.util_rate reg2-reg5 if year==1998
// ... predict the residuals, ...
predict res_util_rate_98, resid
// ... regress the electronic vote share on the controls, ...
reg inst98e reg2-reg5 if year==1998
// ... predict again the residuals, ...
predict res_inst98e, resid
//... and finally regress residuals on residuals:
reg res_util_rate_98 res_inst98e if year==1998, cl(uf)


// HOMEWORK: Think about a reason why the standard errors come out differently? Which ones are the "right" ones?

// When applying the partialling out by hand, we neglect the fact that our partialled out data series are estimated
// (In some sense this resembles the adjustments we had to implement when doing the fixed effect panel regressions by
// hand.) Therefore, we would need to adjust the degrees of freedom, because Stata does not understand these 
// differences by itself. Usually the standard errors are very close, but if you had to rely on one set of standard 
// errors you should normally opt for the standard errors from the complete regression.




// A general challenge is that we do not have many observations to draw statistical conclusions. And
// at the same time we do have a structure in which we have not many clusters and not many within cluster
// observations. This could be problematic for the estimation of the standard errors. A bootstrapping 
// procedure could help to overcome these problems -- or at least improve the estimation of the standard errors.

*************************************************************************************************
// Q.3 Some basic bootstrapping

// Bootstrapping is a method of sampling with replacement. Instead of assuming a theoretical distribution for the 
// error terms -- such as the normal distribution -- the actual sampling distribution is constructed by drawing 
// out of the sample over and over again. There are different forms of bootstrapping and in general it roughly 
// descibes a whole bunch of methods in which you draw and replace. It could be non-parametric setting in which 
// you draw sub-samples from your sample to execute statistical procedures on them. A more common form, however 
// is a parametric setting. A way to think about this way of bootstrapping is that you estimate your model, store 
// the error terms, re-shuffle them to generate new data points, and estimate the model once again. You do this 
// over and over to generate a distribution of your coefficients.


// 1) It is useful to specify a seed so that Stata can always generate the same randomness again; the general idea
// is that you allow to reproduce the same random numbers from a specific initial starting point
// (check help seed for more information)

// 2) You should use a high enough number of repetitions, probably around 1000

// now let us run the code
*cgmwildboot d.util_rate inst98e reg2-reg5 if year==1998, cl(uf) bootcluster(uf) seed(1000) reps(1000)
// ok, this does not work; so we would have to construct a variable that contains d.util_rate

gen dif_util_rate = d.util_rate

// p-value for column (1)
cgmwildboot dif_util_rate inst98e reg2-reg5 if year==1998, cl(uf) bootcluster(uf) seed(1000) reps(1000)

// In the actual paper, Fujiwara relies on the residuals that we derived already above...
cgmwildboot res_util_rate_98 res_inst98e if year==1998, cl(uf) bootcluster(uf) seed(1000) reps(1000)
// getting close, but there seems to be still something else

// p-value for column (2)
cgmwildboot dif_util_rate inst02e reg2-reg5 if year==2002, cl(uf) bootcluster(uf) seed(1000) reps(1000)


// maybe the original codes were not written in Stata, but other statistical software packages, 
// which deviate slightly in their routines. This could explain the different findings for the bandwidth as
// well.

*************************************************************************************************
// Q.4 Scatter plots of residuals after partialling out

twoway scatter res_util_rate_98 res_inst98e if year==1998, ms(circle) mc(blue) || lfit res_util_rate_98 res_inst98e if year==1998, color(red) ///
xtitle("% of Voters above Cutoff (Residuals)") ytitle("Change in Valid Votes/Turnout, 94-98 (Residuals)")

// generate additional residuals
reg d.util_rate reg2-reg5 if year==2002
predict res_util_rate_02, resid
reg inst02e reg2-reg5 if year==2002
predict res_inst02e, resid

twoway scatter res_util_rate_02 res_inst02e if year==2002, ms(circle) mc(blue) || lfit res_util_rate_02 res_inst02e if year==2002, color(red) ///
xtitle("% of Voters above Cutoff (Residuals)") ytitle("Change in Valid Votes/Turnout, 98-02 (Residuals)")

// What we plot are basically the underlying data points and the regression line that we ran before for equation (6) and (7).
// These plots can help us understand how precisely we estimated the effect and we can visually inspect the role of outliers, etc.

*************************************************************************************************
// Q.5 Estimating the coefficients for column (3)
// let us regress equation (8)
reg util_rate inst delec* reg* duf*, cl(uf)


// estimating equation (8)
// we need to use the interaction of S and 1998. Then we can estimate the average effect
br uf year inst98e // seems to be correct


// so let's regress the valid vote share on state dummies, year dummies, and regionalXyear dummies
reg util_rate inst98e duf2-duf27 delec2 delec3 reg?_*, cl(uf)

// we could also leave it up to Stata to drop redundant controls
reg util_rate inst98e delec* reg* duf*, cl(uf)

// we could also use the "areg" command
areg util_rate inst98e delec* reg*, absorb(uf) cl(uf)

// It is a neat strategy to identify the coefficients and implement a quite natural test of equality.

*************************************************************************************************
// Q.6 Estimating the coefficients for column (4)

// HOMEWORK: Run the regression for column (4). To do so you have to set up an additional variable
// inst2, which changes the equation of regression (8) in such a way that the estimated coefficient
// corresponds to (theta_98 + theta_02)/2

// Now let us set up a new variable to estimate the sum of the coefficients divided by 2 the variable should be 
// identical to "inst" in period 1994 and 1998, but for 2002 it should contain the value of S-(1-S) so that the
// difference between 1998 and 2002 is given by -(1-S). Thus, if S=0.4 in 1998, we want to add the entry -0.2 
// for 2002.

gen inst2 = inst
replace inst2 = inst02e - (inst - inst02e) if year==2002

reg util_rate inst2 delec* reg* duf*, cl(uf)

*************************************************************************************************
// Q.9 Generating a plot to investigate the impact of outliers

// Let us loop equation (8), always leaving out one state, and store the coefficients, lower bound, and upper bound

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

qui reg util_rate inst98e duf2-duf27 delec2 delec3 reg?_*, cl(uf)
local effect = _b[inst98e]
scatter coef uf_id, xlabel(1/27, labsize(small)valuelabel) || rcap lb ub uf_id, legend(off) xtitle("") ylabel(0(0.05)0.15) yline(`effect', lcolor(red))

*************************************************************************************************
// Q.10 Health Outcomes over time

use yearly.dta, clear

areg ssaude inst1996-inst2006 reg* if year_f>1994 & year_f<2007, rob a(uf)
 gen te_s = 0 if year_f==1995
 forvalues i = 1996(1)2006 {
  replace te_s = _b[inst`i'] if year_f==`i'
 }
 

sort year_f
scatter te_s year_f if year_f>1994 & year_f<2007, connect(l) ytitle("Coefficient on S") ///
 xline(1998.5) xline(2002.5) xlab(1995(1)2006) xtitle("")

log close
