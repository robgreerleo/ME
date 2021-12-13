* Development Microeconometrics using STATA
* Solution for Session 7: La Ferrara - Soap Operas and Fertility
* Robert Leonard

version 15 

set more off, permanently // do not stop after each page

********************************************************
* Setting up the directories

global datadir "C:\Users\roble\Desktop\Working\STATA\HW 7\HW7 Data"
global workdir "C:\Users\roble\Desktop\Working\STATA\HW 7"

********************************************************

capture log close 
log using "$workdir/leonard_exc7.log", replace

********************************************************
* Q.1 Set up dataset
use "$datadir/LCD_ind", clear
* set up Globo coverage
gen globo = (yr1stcov<year)
lab var globo "Globo coverage in previous year"

// indicator specifying areas below 95 percentile
sum geoarea80, detail
* Problem: some districts have more observations due to a longer time span
* of measurment.  Want to take each AMC area only once.
bysort amc_code: gen _nobs = _n
sum geoarea80, detail
sum geoarea80 if _nobs==1, detail

gen large = (geoarea80>=r(p95))
tab large
* actually ends up being 12.5% of the total data sample
* can keep data and add constraints, or drop the data (here we don't need it)
drop if large==1

********************************************************
* Q.2 Replicate Table 2

// column 1
// we could set up the time dummies by hand
tab year, gen(y_)
reg birth globo y_* 
* note one is dropped, could rerun to change output order
reg birth globo y_1-y_12
* Cluster the standard errors - observations aren't drawn randomly
* they are related and their error terms are not independent
reg birth globo y_1-y_12, cluster(amc_code)
//use sample weights
reg birth globo y_1-y_12 [aweight = weight], cluster(amc_code)

// use i.year instead to genereate dummies, i is for integer data
reg birth globo i.year [aweight = weight], cluster(amc_code)
est store e1

// column 2, now include area fixed effects, absorb(fixed effect)
areg birth globo i.year [aweight=weight], cluster(amc_code) ///
 absorb(uf_code)
est store e2

// column 3, now fixed effects at AMC level
areg birth globo i.year [aweight=weight], cluster(amc_code) ///
 absorb(amc_code)
est store e3

// generate additional variables for columns 4-6
gen age2 = age^2
gen stock2 = stock^2
label var age2 "Age sq."
label var stock2 "Stock sq."
// set up global macro for all the control variables
global controls "age age2 stock stock2 yrsedu_head wealth_noTV married catholic rural doctors ipc"

// column 4
reg birth globo $controls i.year [aweight = weight], cluster(amc_code)
est store e4

// column 5
areg birth globo $controls i.year [aweight=weight], cluster(amc_code) ///
 absorb(uf_code)
est store e5

// column 6
areg birth globo $controls i.year [aweight=weight], cluster(amc_code) ///
 absorb(amc_code)
est store e6
* take coefficients from c6 and the info from the fixed effect
predict pr_birth_6, xbd
predict pr_birth_6_test
sum pr_*
// when predicting values we have to make sure that we take into account the 
// values for the absorbed variables as well; otherwise it would be similar to 
// not using year dummies to make the predictions.

esttab e1 e2 e3 e4 e5 e6, drop(*.year) 

********************************************************
* Q.3

foreach x of var birth globo y_* {
bysort amc_code: egen av_`x' = mean(`x')
gen fe_`x' = `x' - av_`x'
drop av_`x'
}

// column 3
areg birth globo i.year, cluster(amc_code) absorb(amc_code)
reg fe_birth fe_globo fe_y_2-fe_y_13, cluster(amc_code)
// we make an error with not adjusting the degrees of freedom
// areg is taking care of this

// what about the "constant"

foreach x of var birth globo y_* {
bysort amc_code: egen av_`x' = mean(`x')
sum av_`x'
gen fe2_`x' = `x' - av_`x' + r(mean)
drop av_`x'
}
areg birth globo i.year, cluster(amc_code) absorb(amc_code)
reg fe2_birth fe2_globo fe2_y_2-fe2_y_13, cluster(amc_code)
* now the constants match
// technical note: fixed effect model does not have a constant!
// areg reports something like an average fixed effect as constant

// aside: detour on fixed effects and dummies.  use column 1
reg birth globo i.year, cluster(amc_code)
areg birth globo, cluster(amc_code) absorb(year)


********************************************************
* Q.4
// the LPM could predict values outside of the unit interval, i.e. prob. larger
// than 1 or less than 0
gen outofbounds = (pr_birth_6>1 | pr_birth_6<0)
tab outofbounds
* for 7% of our predictions, we get nonsense results (but can't discard them)
* would need to change models to get everything inside the unit interval

// goodness of fit
gen gf_birth_6 = (pr_birth_6>0.5)
// check correctness of the preditiction
gen correct_6 = (gf_birth_6==birth)
tab correct_6 if birth==0
tab correct_6 if birth==1

// terrible fit, but for this model we are not interested in the predictive 
// power to get the zeros and ones correct, but rather want to understand
// whether globo has an effect, i.e. the treatment effect of globo


********************************************************
* Q.5 Interaction terms

gen globoXeduh = globo * yrsedu_head
gen globoXedu = globo * yrs_edu
gen globoXwealth = globo * wealth_noTV

areg birth globo globoXeduh $controls i.year [aweight = weight], ///
 cluster(amc_code) absorb(amc_code)
 est store intl

 // HOMEWORK: add the other two columns; add some interpretations
 // for the interactions, e.g. compute the thresholds, argue about 
 // heterogenous effects, etc.
 * when birth rate declines in quadratic age model
 
 // Column 2, pay attention to the controls
global controls2 "age age2 stock stock2 yrs_edu wealth_noTV married catholic rural doctors ipc"
 
areg birth globo globoXedu $controls2 i.year [aweight = weight], ///
 cluster(amc_code) absorb(amc_code)
est store int2
 
// Column 3
areg birth globo globoXwealth $controls i.year [aweight = weight], ///
 cluster(amc_code) absorb(amc_code)
est store int3

/* Comment 
1) The quadatic terms for age and stock of children allow for turning points
in expected births once the age levels and numbers of children rise beyond
the turning point or threshold.  First we check to make sure the signs of the
coefficients for age and age^2 changes and then we can calc the turning point
as B1/(2*B2).  Here the turning point for age is .0230842/(2*.0004206) = 27.4
years old.  So the probability of giving birth in any given year will rise with
age until 27 years old, then the probability will begin declining.
For # of children, the turning point is  .0017513/(2*.0000256) = 34 which really
isn't a significant measure. For column 2 where we control for the mother's 
education level instead the effect is reverse - we expect a lower probability
of giving birth at first until the turning point of 1.85 children.  So once 
a woman has 2 children say, it becomes slightly more likely she will have a 
3rd child this year.  

2) We've added three interaction terms here - Globo TV coverage area combined
with # years of education for the head of household, for the woman and finally
with a wealth index.  We are trying to see if the years of education and wealth
levels has any impact in terms of increasing or reducing the effect on fertility
that globo has.  That is, does more education or wealth lead to a less strong 
effect of globo coverage on birth probability ("fertility")?

For column 2, the effect of globo coverage reduces the probability of birth in 
the year by the coefficient of globo of -.013, which is the effect assuming
that the woman has 0 years of education.  

So this effect is mitigated by the  years of education that the woman has 
(coefficient of .0018 * # years).  If  a woman only has 1 year of education, 
then the negative effect of Globo on fertility is reduced 
to -0.013 + (.0018*1 year) = -0.011 or 1.1% reduction (and not 1.3% reduction).
At some point, enough education fully eliminates the effect of Globo on 
fertility.  This is foundas follows: the coefficient on globo / the coefficient 
on the interaction term, or -.013/.0018 = 7.22 years.  Since education is 
measured as a discrete variable, once a woman attains 8 years of education,
then we expect the effect of globo on fertility to be reduced to zero.
Thus, more educated women's fertility is less impacted by Globo.

For column 3, we introduce wealth as the interaction term.  We want to test to
see if higher wealth levels reduce the impact of Globo's negative impact on 
fertility.  Wealth is an index variable.  The paper quotes effects based on
20th percentile and 80th perecentile wealth levels.  Calculate those here now:
*/

* set up percentile breaks (use 5 to get 20%, 40%, etc.)
 _pctile wealth_noTV, nquantiles(5)
 * display the results
 return list
 /* So r(r1) = 20th perecentile which is -1.563.  The effect of Globo taking
 into account this 20th percentile wealth index level is thus calculated as:
 -0.0043 + .0018*(-1.563) = -0.007 or a .7% decline in fertility for those at 
 the 20th percentile ranking in wealth (so poorer than average).
 
 Now r(r4) = 80th percentile whic is 1.132, and inserting this into the same
 equation above, the negative effect of Globo on fertility is reduced to only
 0.2%.  So a higher wealth level means that Globo access has less of a negative
 impact on fertility.  Those that are poorer will be more impacted by Globo.
 
 The paper hypothesizes that richer people, and more educated people are more 
 likely to have had other exposure to role models with smaller family size,
 so Globo soap opera's would have less effect on them.
 */

 
********************************************************
* Q.6 Output tables, columns 4-6 from exercise 2

ssc install outreg2
// estout outreg outreg2 packages, lyx is nice program for latex editing
 
est restore e4 
outreg2 using "$workdir/leonard_table9", replace label nonotes ///
  title(Table 2 - Globo Coverage and Fertility, Panel B) ///
  drop(i.year) dec(4) asterisk(se) ctitle([4]) ///
  addnote(Notes: Table reports OLS coefficients.  Standard errors in ///
parenthesis are corrected for clustering at the AMC level.  AMC stands for ///
Minimally Comparable Area and is a geographic aggregate slightly broader ///
than a municipality.,*** Significant at the 1 percent level., ///
** Significant at the 5 percent level., ///
* Significant at the 10 percent level. ) ///
 addtext(Year Fixed Effects, Yes, Area Fixed Effects, No) ///
  word 
 
// HOMEWORK: add e5 and e6 (using the append option instead of repalce), use
// labels, titles, add notes, etc.  Try to produce a super nice table!  
est restore e5 
outreg2 using "$workdir/leonard_table9", append label nonotes ///
  drop(i.year) dec(4) asterisk(se) ctitle([5]) ///
addtext(Year Fixed Effects, Yes, Area Fixed Effects, State) ///
 word

* column 6
est restore e6 
outreg2 using "$workdir/leonard_table9", append label nonotes ///
  drop(i.year) dec(4) asterisk(se) ctitle([6]) ///
addtext(Year Fixed Effects, Yes, Area Fixed Effects, AMC) ///
 word

* Note: outreg2 doesn't seem to have text justification options - the help file 
* refers the user to word.  Also can't seem to find an option to delete the 
* automatic column numbering and the word "Variables" from the column header,
* so this was done in word as well.
 
 
* close the log file 
log close
