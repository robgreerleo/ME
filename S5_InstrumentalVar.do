* Development Microeconometrics using STATA
* Solution for Session 5: AJR On the Colonial Origins of Growth
* Robert Leonard

version 15 

set more off, permanently // do not stop after each page

********************************************************
* Setting up the directories

global datadir "C:\Users\roble\Desktop\Working\STATA\HW 5\HW5 Data\maketable"
global workdir "C:\Users\roble\Desktop\Working\STATA\HW 5"

********************************************************

capture log close 
log using "$workdir/leonard_exc5.log", replace

********************************************************
* Q.1 Setting up the dataset
* missing observation 3 so use foreach instead of forvalues
foreach k of numlist 1/2 4/8 {
// short for 1 2 4 5 6 7 8
use "$datadir/maketable`k'",clear
*describe
duplicates list shortnam
* result is 0 duplicates, shortnam uniquely describes counties in all datasets
}
// merging all parts of the dataset
use "$datadir/maketable1",clear
foreach k of numlist 2 4/8 {
merge 1:1 shortnam using "$datadir/maketable`k'", gen(m_`k') 
// remove the merge code for each iteration via drop, or option no gen, or 
// give a specific name for each file.  
}
// All obs matched so merge code doesn't contain any useful information
// in principle, it could identify which countries are part of one dataset
// but not part of another.
drop m_*

save "$workdir/AJRdata", replace

********************************************************
* Q.2 Checking settler mortality
// look at the distribution of settler mortality
histogram extmort4
* don't forget to exclude missing values (which are treated as large numbers)
list shortnam extmort4 if extmort>1000 & extmort!=.
* must deal with these 3 very large outliers compared to the rest of the data
* they adjusted the distribution by taking the log, so a narrower distribution
* that reduces the importance of the outliers
gen log_extmort4 = log(extmort4)  // ln() does the same
// histogram looks better, less driven by outliers
histogram log_extmort4
* check to see if log_extmort4 == logem4, scatter shows how close graphically
scatter log_extmort4 logem4
* looks like one outlier off the 45 degree line, can check with list (Tanzania)
list shortnam extmort4 logem4 if log_extmort4!=logem4

replace logem4 = log_extmort4 if log_extmort4!=logem4

drop log_extmort4

********************************************************
* Q.3 Match the Descriptive Statistics
* use a global macro to define the variables we'll repeatedly use
* note: cannot break global macro over multiple lines
global sumstats "logpgp95 loghjypl avexpr cons90 cons00a cons1 democ00a euro1900 logem4"

sum $sumstats

* create column 1 for the whole world
estpost tabstat $sumstats, stat(mean sd) columns(statistics)
replace euro1900 = euro1900 / 100
* create column 1 for the whole world
estpost tabstat $sumstats, stat(mean sd) columns(statistics)
est store stat1

* for the baseline sample
estpost tabstat $sumstats if baseco==1, stat(mean sd) columns(statistics)
est store stat2

* for the quartiles (baseline sample or overall sample choice)
xtile quartiles = extmort4 if baseco==1, nquantiles(4)
* to check how the dataset is split
table quartiles if baseco==1, contents(min extmort4 max extmort4 n extmort4)
* we have slightly different cutoff values, we can impose the papers values
* cutoff function only works in egen, 3000 is greater than any value in data
egen ajr_quartiles = cut(extmort4) if baseco==1, at(0 , 65.4 , 78.1 , 280, 3000) icodes

table ajr_quartiles if baseco==1, contents(min extmort4 max extmort4 n extmort4)

forvalues k = 0/3 {
estpost tabstat $sumstats if ajr_quartiles==`k' & baseco==1, stat(mean sd) columns(statistics)
est store quart_stats`k'
}

* generate overall table using esttab
esttab stat1 stat2 quart_stats*, main(mean %12.1fc) aux(sd %12.1fc) ///
 nonote unstack label nonum nogaps  mtitle(world)

********************************************************
* Q.4 Running the OLS regression for equation (1), reproducing column
* run regression for column 1 and store values
regress logpgp95 avexpr
est store c1

regress logpgp95 avexpr if baseco==1
est store c2
 
regress logpgp95 avexpr lat_abs asia africa other if baseco==1 
est store c6

reg loghjypl avexpr if baseco==1
est store c8

esttab c1 c2 c6 c8, b(%9.2f) se(%9.2f) stats(N r2)

// HOMEWORK: 1) estimate the regressions from above with robust standard errors
// (option ", robust"  ; 2) would you take conventional or robust std errors?
* add robust option, and store values (can use robust or vce(robust))
regress logpgp95 avexpr, robust
est store c1r

regress logpgp95 avexpr if baseco==1, robust
est store c2r
 
regress logpgp95 avexpr lat_abs asia africa other if baseco==1, robust 
est store c6r

reg loghjypl avexpr if baseco==1, robust
est store c8r

esttab c1r c2r c6r c8r, b(%9.2f) se(%9.2f) stats(N r2)
/*  Robust vs. conventional standard errors:
Using robust errors is never incorrect if the sample size is large enough.
Here, however, sample sizes for the base categories are pretty small
at only 64 observations, which would lead me to lean towards sticking with
conventional standard errors.  

Heteroskedasticity may pose a problem for the t-statistics in conventional 
standard errors, and robust errors helps to correct for this, but only when
sample sizes are sufficiently large, and 64 samples wouldn't meet this 
criteria, in my opinion.  Furthermore, the results of robust errors don't 
really change any of the conclusions other than reducing the t-statistic on the 
Asia dummy in column 6, but not by a lot to change the conclusions.

Heteroskedasticiy can be tested for; a White test could be employed here to 
check to make sure we don't have it.

Model transformations can also address any heterosk. issues.  
Transforming a non-log model to a log model, as we have already done here to
mitigate the problems of the 3 outliers in the data, can correct for a lot
of heterosk. problems, so it's possible that nothing more needs to be done.
Further model forms could be considered, such as weighted least squares if 
we thought heterosk. was a big problem.
*/

********************************************************
* Q.5 Running more regressions
* column A-4
reg avexpr democ00a lat_abs if baseco==1
* redefine data screen to match sample size and Table 3 results better
reg avexpr democ00a lat_abs if excolony==1 & euro1900!=. & logem4!=. & ///
 democ00a!=. & avexpr!=. & lat_abs!=.
est store a4
* column A-10
reg avexpr lat_abs logem4 if baseco==1
est store a10
* column B-6 
reg democ00a euro1900 lat_abs if excolony==1 & euro1900!=. & logem4!=. & ///
 logpgp95!=. & democ00a!=.
est store b6
* column B-10
reg euro1900 logem4 lat_abs if euro1900!=. & excolony==1 & logem4!=. & ///
 lat_abs!=. & logpgp95!=.  
est store b10

// HOMEWORK: set up a table with the corresponding results.
* what is the purpose of this table?
esttab a4 a10 b6 b10, b(%9.2f) se(%9.2f) stats(N r2)
/* The purpose of the table:	
The OLS results from Table 2 do not alone establish causality.  Table 3 is meant
to help address this issue, namely to help rule out reverse causality and omitted
variable biases.  The authors are using an instrumental variable approach,
accounting for the variation in the quality of institutions variable (avexpr) 
that would not also affect income per capita today.   

This table helps to show that settler mortality rates are related to both the 
formation of european colonies, and to early institutions.  It also shows
that institutional forms tend to persist over time; that early institutions
are related to current protection from appropriation today.
*/

********************************************************
* Q.6 Reproduce Figure 2 Scatterplot and plot population share of 
* Europeans against settler mortality

* Figure 2
scatter logpgp95 avexpr if baseco==1 || lfit logpgp95 avexpr if baseco==1
twoway (scatter logpgp95 avexpr if baseco==1) (lfit logpgp95 avexpr if baseco==1)
* Or do as a two-way graph, which has additonal options
twoway (scatter logpgp95 avexpr if baseco==1, msymbol(i) mlabel(shortnam) ///
 mlabpos(11) mlabcolor(black) mlabsize(vsmall) ) ///
 (lfit logpgp95 avexpr if baseco==1, lcolor(gs0) yscale(range(4 10)) ///
  ylabel(4(2)10) xscale(range(4 10)) xlabel(4(2)10) ///
  ytitle("Log GDP per capita, PPP, 1995") xtitle("Average Exp. Risk 1985-95") ///
  legend(off) title("Figure 2") scheme(s1color) plotregion(style(none)) )
graph export "$workdir/gdp.pdf", replace

// HOMEWORK: Plot European settlements against settler mortality
// same as B10 estimate from Q5, plot 2 against eachother.
// you can simply copy paste and adjust from above
// Do you observe anything special?
twoway (scatter euro1900 logem4 if baseco==1, msymbol(i) mlabel(shortnam) ///
 mlabpos(11) mlabcolor(black) mlabsize(vsmall) ) ///
 (lfit euro1900 logem4 if baseco==1, lcolor(gs0) yscale(range(0 1.2)) ///
  ylabel(0(.2)1.2) xscale(range(2 8)) xlabel(2(1)8) ///
  ytitle("European Settlements in 1900") xtitle("Log of Settler Mortality") ///
  legend(off) title("Figure 4: Determinants of Institutions") ///
  scheme(s1color) plotregion(style(none)) )
graph export "$workdir/Settlement Graph.pdf", replace

/* The graph shows a negative relation between European settlements 
and settler mortality rates.  The higher the mortality rates, the less likely
we would expect there to be long term European settlements, and the more likely 
those colonies would be extractive ones instead.  The linear regression line 
crosses the x-axis, but there's no real-world meaning to having a population of 
less than 0 percent Europeans, so this portion of the line can be ignored.  

The 4 outliers of New Zealand, Australia, Canada and the USA (or the 
"neo-Europes" are clearly delineated here in the upper left corner, of high 
European settlement and low mortality rates.  These 4 are also sometimes 
removed from the base sample in the study without major impacts to the
authors' conclusions.  
*/

********************************************************
* Q.7 Commment on source of bias

//HOMEWORK: briefly answer the questions from the problem set.
/* Potential bias of the coefficients is usually due to omitted variable
bias, where we have excluded a relavant variable for explaining economic
performance (typically either because we lacked the data for it, or were 
unaware of it).  So the independent variable is correlated to the error term.
The direction of the bias on the coefficients in equation 1 will be determined
by the sign of the estimated coefficient for the omitted variable(s) and the 
sign of the correlation between the independent variables and the omitted
variable (OV).  
If the correlation is positive, then the bias is as follows:
positive bias if the estimated coefficient of the OV is positive.
negative bias if the estimated coefficient of the OV is negaive.
If the correlation is negative, then the bias is as follows:
negative bias if the estimated coefficient of the OV is positive;
positive bias if the estimated coefficient is negative.

Measurement error may also induce bias.  Here the quality of institutions
depends on an index which is likely an incomplete measure of the quality.
The resulting bias will depend on the nature and direction of the measurement
errors.

*/

log close
