* Development Microeconometrics using STATA
* Solution for Session 6: AJR On the Colonial Origins of Growth
* Robert Leonard

version 15 

set more off, permanently // do not stop after each page

********************************************************
* Setting up the directories

global datadir "C:\Users\roble\Desktop\Working\STATA\HW 6\HW6 Data"
global workdir "C:\Users\roble\Desktop\Working\STATA\HW 6"

********************************************************

capture log close 
log using "$workdir/leonard_exc6.log", replace

********************************************************
* Q.1 Running IV by hand for column 8, table 4

use "$datadir/AJRdata.dta", clear
// first stage regression
reg avexpr logem4 lat_abs asia africa other if baseco==1
est store ols1
* option xb is for linear prediction
// getting R hat for 2SLS
predict predicted_avexpr if baseco==1, xb 
* for AGO, calculate predicted value by hand (getting R hat)
di 7.73+-0.34*5.63+2*.14-.26

// second stage regression
reg logpgp95 predicted_avexpr lat_abs asia africa other if baseco==1
est store ols2

// biased OLS regression
reg logpgp95 avexpr lat_abs asia africa other if baseco==1
est store ols

est table ols1 ols2 ols, b(%8.2f) se(%8.2f) p(%8.2f) stats(N r2)

// an increase in predicted avexpr of 1 unit leads to an incease of gdp by 111
// log points (from OLS2). To convert it into percentages: 
di exp(1.11)

// for smaller deviations, the correspondence of log points and percentage
// points is almost 1:1
di exp(.1)
di exp(.5)
di exp(1)

* State two main assumptions
* 1) Exclusion restriction: coviariance between instrument and error term is 0
* 2) Relevance: covariance between independent variable (institutional quality
* and instrument (settler mortality) is not 0, i.e. settler mortality drive
* institutional quality.
* What problems did we create? 
* After completing this process, we have supposedly stripped the endogeneity 
* from institutional quality, unbiasing our estimators.  But we now have 
* incorrect standard errors for our t and F-tests, and these must be
* corrected as well.
* We may also have a problem of weak instruments, in which case, we should
* try to find a better instrument.

********************************************************
* Q.2 Running IV by ivreg, replicating table 4

// column 2, put potentially endogenous variable in brackets 
* option first reports the first stage
ivregress 2sls logpgp95 lat_abs (avexpr = logem4) if baseco==1, first
est store iv2

// column 4, change the constraint
ivregress 2sls logpgp95 lat_abs (avexpr = logem4) if baseco==1 ///
  & rich4!=1, first
est store iv4

// column 8
ivregress 2sls logpgp95 lat_abs asia africa other (avexpr = logem4) ///
 if baseco==1, first
est store iv8

est table iv2 iv4 iv8 ols1 ols2, b(%8.2f) se(%8.2f) p(%8.2f) stats(N r2)

* Interpret results of column 8 statistically and economically
/* Once we have stripped out the endogeneity of institutional quality, we see
that the relationship between institutions and per capita income is actually
economically stronger at 1.1 rather than .52 from the original regression.  
We have controls for geography (latitude, continent dummies) which doesn't
significantly alter our estimation of the institutional quality coefficient.
The institutional quality measure is statistically significant with a t-value of
approx 2.39.  However, this is a much smaller statistical significance than
projected in the original OLS regression.
The african continent dummy is not significant, indicating that their per 
capita income levels are due mainly to the quality of institutions, rather
than geography.
*/ 

********************************************************
* Q.3 
* regress column 8 again, need to specify the small sample option
* so std error calcs appropriately adjust df
ivregress 2sls logpgp95 lat_abs asia africa other (avexpr = logem4) ///
 if baseco==1, first small
est store iv8b
mat v_2sls = e(V) 

est table iv8 iv8b, b(%8.2f) se(%8.2f) p(%8.2f) stats(N r2)
* std. error comes from diagonal elements of this matrix:
mat list v_2sls
di v_2sls[1,1]^(1/2)
* can use MATA to take the square root of whole matrix, or do it point by point
* the above calculation only works with scalars

// running the "wrong" second stage regression
regress logpgp95 predicted_avexp lat_abs asia africa other if baseco==1
mat v_wrong = e(V)
* nead MSE and DF
scalar mse_wrong = e(rmse)^2
scalar dfk = e(df_r)

// generate the correct residuals: original regressors * second stage coeff.
gen fitted_values = _b[predicted_avexpr]*avexpr + _b[lat_abs]*lat_abs + ///
_b[asia]*asia + _b[africa]*africa + _b[other]*other +_b[_cons] if baseco==1
gen error = logpgp95 - fitted_values if baseco==1 
gen error_squared = error^2 if baseco==1
sum error_squared
scalar mse_corrected = (r(sum) / dfk)

mat v_corrected = mse_corrected / mse_wrong * v_wrong

mat list v_corrected

mat diff = v_2sls - v_corrected
mat list diff

// use mata to get the standar errors - calc square roots and save them
mata st_matrix("st_errors", sqrt(st_matrix("v_corrected")))

mat list st_errors

ivregress 2sls logpgp95 lat_abs asia africa other (avexpr = logem4) ///
 if baseco==1, small
* now have the correct standard errors

********************************************************
* Q.4 Table 6, columns 2, 6, 9

global temperature "temp1 temp2 temp3 temp4 temp5"
global humidity "humid1 humid2 humid3 humid4"
global soil "steplow deslow stepmid desmid drystep drywint"
* note that paper identifies highland soil, but no such variable in data

// column 2   
ivregress 2sls logpgp95 lat_abs $temperature $humidity (avexpr = logem4) ///
 if baseco==1, first small
* test that these are jointly = 0, p-value is 0.97 so they aren't significant
test $temperature
* humidity: if drop small option in regression, humid 3 and 4 are close to
* being significant.  but with joint test, they aren't
test $humidity
* drop the others and focus on humid 3 as additional regressor
ivregress 2sls logpgp95 lat_abs humid3  (avexpr = logem4) ///
 if baseco==1, first small
// it is not the case if we only include humid3

//HOMEWORK: Add columns (6) and (9) and comment on the tests.

* column 6, define global variable for natural resource data, 
* note: data includes silv which probably means silver, included here
* and no variable for total number of minerals in the country, so left out
global natres "goldm iron zinc silv oilres"
ivregress 2sls logpgp95 lat_abs $soil $natres landlock /// 
 (avexpr = logem4) if baseco==1, first small
test $soil
test $natres
* note: some coefficients or std errors don't match up exactly in magnitude
* but are close in range.  The paper states that for column 6 and 9
* the sample size drops to 63 from 64, due to a problem with natural resource
* variables, but it is unclear which country falls out.  All have data here.
* It could be an issue with the extra silver data or the lack of a data field
* for number of minerals present

* column 9
ivregress 2sls logpgp95 lat_abs $temperature $humidity $soil $natres  ///
 landlock edes1975 avelf (avexpr = logem4) if baseco==1, first small
test $temperature
test $humidity
test $soil
test $natres
* test desmid, only one with a relatively high t-value
ivregress 2sls logpgp95 lat_abs desmid  (avexpr = logem4) ///
 if baseco==1, first small
* still not significant

* What is the purpose of the joint significance tests
* To test multiple parameters at once.  It's an exclusion test to see if a 
* group of variables has any effect on the dependent variable (income per pers)
* comment on omitted climate and natural resource coefficients 
* neither climate nor natural resource coefficients are significant, either
* economically or statistically

********************************************************
* Q.5 Table 7, columns 7,8 and 9

//column 7
ivregress 2sls logpgp95 (avexpr malfal94 = logem4 lat_abs meantemp lt100km) ///
 if baseco==1, first small
 
 // HOMEWORK: Add the regressions for columns 8 and 9 and briefly comment.
* column 8
ivregress 2sls logpgp95 (avexpr leb95 = logem4 lat_abs meantemp lt100km) ///
 if baseco==1, first small

*column 9 
ivregress 2sls logpgp95 (avexpr imr95 = logem4 lat_abs meantemp lt100km) ///
 if baseco==1, first small
 
 /* Testing to see if malaria and life expectancy have any impact on economic
 performance for comparing results to other models that treat these variables
 as endogenous along with institutional quality.  The results don't change, 
 only institutional quality is significant after stripping out the endogeniety
 from all of these variables.
 */

********************************************************
* Q.6 Table 7, with overid tests and weak instruments
* testing the second assumption - relevance and weak instruments
ssc install ivreg2
ssc install ranktest

ivreg2 logpgp95 (avexpr malfal94 = logem4 lat_abs meantemp lt100km) ///
 if baseco==1, first 
 
* further analysis, aside 
* check main specification from colum 8 (case #2)
ivreg2 logpgp95 (avexpr = logem4) lat_abs asia africa other ///
 if baseco==1, first 
* low F score at 3.46, so it's a very weak instrument (not called attention to
* when paper was first published)
* Labor economics, no one would consider this significant.  Same for development
* economics.  

// HOMEWORK: Look at the diagnostic tests and comment on them (from help file).
/* The diagnostic tests seek to identify whether or not the regression equation
is correctly identified.  That is, the independent variables are not strongly 
endogenous, or strongly correlated with the error term.  If these variables are 
strongly endogenous, then it is possible the estimated coefficients will not be 
consistent estimators and need to be corrected, here using instrumental 
variables.  However, we also have to check to make sure that the instrumental
variables are not weak, i.e. they have a strong covariance with the endogenous
regressors.

In the first stage of 2SLS, we are testing the hypothesis that the instruments
alone are not related to the endogenous regressor, i.e. their coefficient for 
explaining the regressor is 0.  The higher the t-statistic, the less likely 
they are weak for each indiviudal instrument.
The Sanderson-Windmeijer F-test is a joint measure of the excluded instruments
strength as a group.  Here the F-stat is a low 1.35 indicaint a risk of the 
instruments as a groupd being weak.  
The Anderson LM statistic tests whether or not the regression is 
underidentified, that the instruments capture the fullest prediction of the 
dependent variable (here our measure of institutional quality) and thus serve
as good instruments for stripping out the endogeneity of institutional quality.
The null hypotheis is that the matrix is underidentified, that the instruments
are relevant to explaining institutional quality.  Here the p-value of rejecting
the null hypothesis is 25%, leaving us not very confident that the instruments
in the regression here have a strong relationship to institutional quality.
If instruments are weak, then they arent well related to the ednogenous regressor
and using these to strip out the endogeneity will not correct the inconsistencies
in the estimated coefficients of the endogenous regressor.
The Cragg-Donald Wald F statistic tests for weakness, and here our result is an
F-statistic of only 1, indicating that this equation is weakly identified.
Stock and Yogo provide critical F-test values based on amount of bias and sample
sizes.  Here, the lowest critical value is 4.73 and 6.28 respectively (bias and
sample size) well above our 1.0 F-statistic indicating our instruments are weak.

The Sargan statistic tests for overidentification, that the instruments included
aren't correlated with the error term and do not predict per capita gdp, and 
thus are correctly left out of equation 1's original regression.  
The null hypotheis is that we don't have overidentification and 
here we have a low Sargan statistic of 1.023 nad a p-value of close to .60, so
we wouldn't reject the null hypthesis of no overidentification here.
The instruments aren;t overly correlated with the error in equation 5, and 
they also shouldn't be included in our original regression model, equation 1
as they don't impact per capita gdp.

So, in conclusion, we don't seem to have an overidentified problem with the 
settler mortality instrument explaining institutional quality, but we do seem
to have a problem with settler mortality being a weak instrument, which may
impact the bias of the corrected coefficients when settler mortality is 
employed as an instrumental variable.

*/

log close
