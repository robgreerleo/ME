* Development Microeconometrics using STATA
* Solution for Final 
* Robert Leonard

version 15 

set more off, permanently // do not stop after each page

********************************************************
* Setting up the directories

global datadir "C:\Users\roble\Desktop\Working\STATA\final\Data"
global workdir "C:\Users\roble\Desktop\Working\STATA\final"

********************************************************

capture log close 
log using "$workdir/leonard_final.log", replace

********************************************************
* Q.1 Set up dataset 

* import data
use "$datadir/final.dta", clear
* review data, especially proxy variable e955
br
describe
codebook
sum 
* no regression variables have 0 as min value, so no ln(0) problem
sum e95 e955, detail

* Check to make sure e955, suicides by firearms is <= total suicides
* also check the one missing value for e955 (none for e95)
tab year fips if e955==.
br if e955==.
* There were only 2 suicides in Sonoma County, California in 2001 but they
* don't report firearm data here, or the number of homicides

* check to make sure e95 is always >= e955
br if e955 > e95
tab year fips if e955 > e95
* check to make sure # suicides is less than say 1/2 of 1% of population
* to make sure # is reasonable 
tab year fips if e95 >= .005*pop

* other data issues - FBI UCR data for 1993 is missing
tab year if ucrrobbery==.
tab year if ucrburglary==.

* some counties have a negative rural population
sum rural
tab fips if rural<0
tab year if rural<0
* all 22 negative values fall outside our 1980-1999 range
* but can't have a negative rural population, so reset to zero or could do 
* absolute value
replace rural=0 if rural<0

* check to make sure households / female head of households
tab year if fhh>households

* check population changes
tabstat pop, by(fips) statistics(mean sd v r max n) format(%9.3g)


********************************************************
* Q.2 Set up FSS Variable

* set up fss 
set type double
gen fss = e955/e95*100
* 1 MV for Sonoma
label var fss "% of suicides with firearms"
sum fss

* check stability of fss by county and time, look at overall first
tabstat fss, by(year) stats(mean N sd)
* now look by county
* look at range as well as std deviation and variance to examine variation
* some fairly wide ranges in the proxy of gun ownership percentage
* can also compare coefficient of variation to see which counties are more 
* variable than others  
tabstat fss, by(fips) statistics(mean max min sd v r cv) format(%6.3g)

* calculate yearly changes
sort fips year
gen fss_ch=.
label var fss_ch "Annual change in fss %"
replace fss_ch = (fss-fss[_n-1]) if year>=1980
* Note, Sonoma County's 2001 and 2002 values are missing due to the missing
* 2001 suicide by firearm data.  
  
* now evaluate average change by each county (unweighted at this point)
tabstat fss_ch, by(fips) statistics(mean sd v r max n) format(%6.3g)

* evaluate how often fss changes by 10% or more 
count if fss_ch >=10
* 834 times!
by fips: count if fss_ch>=10
* limit to time period 1980-99
count if fss_ch >=10 & year >=1980 & year<=1999
by fips: count if fss_ch>=10 & year >=1980 & year<=1999


********************************************************
* Q.3 Replicate Table 1

* need to generate homicide rate, homicide rate with firearms and % urban
gen homrate = e96/pop*100000
* 10 missing values (MV), rate is per 100,000 
label var homrate "homicide rate per 100,000"
sum homrate  // check to make sure results are reasonable
gen homrateFA = e965/pop*100000
* 74 MV
label var homrateFA "homicide rate with firearms per 100,000"
sum homrateFA
gen purban = (1 - (rural/pop))*100
label var purban "% urban population"
sum purban


* generate rates for black and female head of households, note denominator
* for female HH is households and not pop
gen pblack = (black/pop)*100
label var pblack "% black population"
sum pblack

gen pfhh = (fhh/households)*100
label var pfhh "% female headed households"
sum pfhh

* generate unweighted stats
gen f80 = fss if year==1980
label var f80 "fss in 1980"
gen f90 = fss if year==1990
label var f90 "fss in 1990"
gen f99 = fss if year==1999
label var f99 "fss in 1999"

global table1 fss homrate homrateFA purban pblack pfhh e95 f80 f90 f99

* unweighted descriptive stats
estpost tabstat $table1 if year<=1999 & year >=1980, columns(statistics) ///
  statistics(mean n) 
est store unweighted

* weighted descriptive stats, use analytical weights since we are calculating
* average characteristics of a group
estpost tabstat $table1 if year<=1999 & year >=1980 [aweight=pop],  ///
  columns(statistics) statistics(mean n) 
est store weighted
* cal also check [aweight]=pop90 which is used in regression

esttab unweighted weighted, main(mean %12.3fc) unstack ///
 title(Table 1.1: Descriptive Statistics for County Data)  ///
 noparentheses noobs nonote nonum nogaps  ///
 mtitles("unweighted" "weighted") refcat(fss "{it:Full Period (1980-1999)}" ///
 f80 "{it:FSS in selected years}", nolabel) varwidth(30)  ///
 coeflabels(fss "FSS in selected years" homrate "Homicide rate" ///
 homrateFA "Gun homicide rate" purban "%Urban" pblack "%Black" ///
 pfhh "%Female household head" e95 "# Suicides" f80 "1980" ///
 f90 "1990" f99 "1999") 
* this table was also rerun after determining the regression samples
* resulting in only minor changes, e.g. a 1 unit higher # suicides


********************************************************
* Q.4 Replicate Results of Table 2
 
* use xtset to identify panel data
xtset fips year
xtdes

* generate variables for regressions, see note on scaling (not using per 100k)
* y (ln homicide rate)
gen ln_homrate = ln(e96/pop) 
* 10 MV
label var ln_homrate "Natural Log Homicide Rate"

* FSS at t-1
gen ln_lfss = ln(l.e955/l.e95) if year>=1980
* 1 MV for Sonoma, the other 196 are for the year 1979
label var ln_lfss "Natural Log of Proxy Gun Ownership % 1 yr lag"  

* robberies, uses FBI UCR Data, not clear if we use UCR pop or pop
* assume we use pop throughout
gen ln_robberies = ln(ucrrobbery/pop)
* 258 MV
label var ln_robberies "Natural Log of Robbery rate"

*burglaries
gen ln_burglaries = ln(ucrburglary/pop)
* 258 MV
label var ln_burglaries "Natural Log of Burglary rate"

* African Americans
gen ln_pblack = ln(black/pop)
* 0 MV
label var ln_pblack "Natural Log of rate of AA's in pop"

* Urban
gen ln_purban = ln(1-(rural/pop))
* 0 MV
label var ln_purban "Natural Log of % Urban in pop"

* Female Head of Households
gen ln_pfhh = ln(fhh/households)
* 0 MV
label var ln_pfhh "Natural Log of % Female Head House in pop"

* Same Population
gen ln_sameres = ln(resid5yago/pop)
* 0 MV
label var ln_sameres "Natural Log of % Residents Remaining in pop"

sum ln_*
codebook ln_*

* for xtreg weighting, the weights have to be constant within the panel
* the paper does not explain what they did here.  However, they did select
* the largest counties based on 1990 population.  I will use 1990's population
* for the constant weighting here.
gen pop90 = pop if year==1990
label var pop90 "Population in 1990 for weighting"

sort fips year
replace pop90 = pop90[_n-(year-1990)] if year!=1990
* check that there is no variation in each county
tabstat pop90, by(fips) statistics(mean sd n)
codebook pop90 
 
/* I reran the xtregs for each column changing the year used for the weighting
but it doesn't impact the results too severely.
I also tried using the average population across time, and again, the results
remained fairly similar.  So I will just use pop90 as defined above.
Code for varying the weights to use in xtreg
  replace pop90 = pop if year==xxxx
  replace pop90 = pop90[_n-(year-xxxx)] if year!=xxxx
code for using average population across time period
  bys fips: egen pop90 = mean(pop) if inrange(year,1980,1999) // or 2004
  still doesn't make a difference */

* Define an identifier to determine the sample for every regression
* Based on removing missing values 
gen include = 1
label var include "1980-1999 Indicator"
* Remove Sonoma County in 2001 and 2002 due to missing suicide and homicide
* data in 2001 (remove 2002 due to lagged FSS)
replace include = 0 if fips==6097 & year==2001
replace include = 0 if fips==6097 & year==2002
* Footnote 3 also says Oklahoma County/City was removed in 1995 due to terrorism
replace include = 0 if fips==40109 & year==1995

/* remove indicator for those with missing values in our full model so we get
the same sample across all 4 variations.  One problem here is what to do with
the 1993 missing UCR data for burglaries and robberies.  The paper doesn't
mention anything about this, and their sample size of 3822 seems to indicate
that they had this data for at least some/most of the sample given there are 
missing values in other years and other fields.  Tested removing 1993 data
and also using interpolated data for 1993, which I use for this do file here
and for the pdf.  See question 1 on the rationale for using interpolated data.   
Later I found that 2 counties have missing UCR data after 1993 in interpolation
commands, see below, so correct their inclusion code here too, getting us to
3822 to match the paper */
replace include = 0 if fips==17163 & year==1993
replace include = 0 if fips==20091 & year==1993

* remove non included years 1979 and 2000 onwards
replace include = 0 if year==1979
replace include = 0 if year>=2000 // set up include04 for the larger sample

* remove the missing values entries included in the model
replace include =0 if ln_homrate==.
replace include = 0 if ln_lfss==.
* these below don't currently have MV, but in case for later
replace include = 0 if ln_pblack==.
replace include = 0 if ln_purban==.
replace include = 0 if ln_pfhh==.
replace include = 0 if ln_sameres==.
* Problem with missing 1993 UCR data, came back to interpolate these values
replace include = 0 if ln_robberies==. & year!=1993
replace include = 0 if ln_burglaries==. & year!=1993 
tab year if include==1
/* Resulting sample size is 3865 vs 3822 used in the paper.  Difference of 45
 data entries.  It's not clear what the paper includes.  I reviewed data missing 
 values and one candidate used in Table 3 is firearm homicide rates.  We have 
 43 of these with missing values.  Tested including and excluding these in our
 model for Table 2 replication.  It's not used in the model in the table 2 
 results, but this seems like the most likely candidate for exlcusion. */
tab year if include==1 & homrateFA==.
* remove MV entries for firearm homicides, or comment out in regression runs
replace include = 0 if homrateFA==.
tab year if include==1
* 3822 or 3867 sample entries depending on if included or not, excluded here
xtdes if include==1
* unbalanced sample, ran the following regressions with this sample for pdf
* but also tested running a balanced sample outside of this run, no major change
* and it required reducing the sample fairly severely as only 148 counties stay

* Replicate Table 2
* replicate column 1 fixed effects by county and year, weighted by pop90
* tested the robustness of these results by chaning include, pop weight and
* total included years as described in comments above.  This run tries to match
* what is shown in table 2 as best as possible
* use robust errors to correct for heteroskedasticitity - see pdf notes on this
xtreg ln_homrate ln_lfss i.year if include==1 [aweight=pop90], fe vce(cl fips)
est store col1a
* note, due to a 2008 Econometrica article, vce(r) = vce(clustered) 
* xtreg ln_homrate ln_lfss i.year if include==1 [aweight=pop90], fe vce(r)
* this gets the same std errors as using vce(cl)
/* for run based on 3822 sample.  Coeff of .074 vs .1 is pretty far
   off in my opinion, but still stat significant at 10% level.  Std error 
   matches what is shown in table 2. 
Notes on other runs not shown here due to extremely long do and log file:
  I explored changing some things to see if I could better match table 2, for 
  all 4 variations:
   Pop Weights: chaning the pop weight year or using the average doesn't affect
     things too much.
   Years: including years 2000-2004 improves the stat significance and increases 
     the coefficient FSS 
   Firearm homicides, missing values: including the 43 missing entries reduces
     the stat signifince to insignificant and the coefficient to .0055!
   Lagged proxy: I also explored if they may have mistakenly not lagged FSS and 
     used current year values.  The current year values did get better 
	 coefficient matches in some cases but much different standard errors.
   Areg instead of xtreg using pop for weighting instead of pop90, and robust

This is a pretty bad data set, and the effect of the proxy on the homicide 
rate is not robust when including these 43 entries, which we have rates for
but not firearm homicide rates.  This also calls into question the results. */

* replicate column 2 - add robberies and burglaries
* interpolate 1993 missing values for robberies and burglaries, or comment out
tab year if ucrrobbery==. & include==1
tab year if ucrburglary==. & include==1
* only 1993 has missing values for our included sample, so just use the average
* of 1992 and 1994 values (testes using 1992 values, 1994, avg of all years
gen uc93flag = 0
replace uc93flag = 1 if include==1 & year==1993
lab var uc93flag "1 if interpolated 1993 UCR values"
replace ucrrobbery = ((ucrrobbery[_n+1]+ucrrobbery[_n-1])/2) ///
  if year==1993 & include==1 & ucrrobbery==.
* 192 changes made - St Clair Illinois doesn't have UCR data after 1993 at all
* Johnson County Kansas has  missing data from 1993 to 1999, edit for full
* years through 2004 data run, change include in 2000 through 2003 to 1, 2004
* value missing.
replace ucrburglary = ((ucrburglary[_n+1]+ucrburglary[_n-1])/2) ///
  if year==1993 & include==1 & ucrburglary==.

* recalculate the ln_robberies and ln_burglaries
replace ln_robberies = ln(ucrrobbery/pop) if year==1993 & ln_robberies==. 
replace ln_burglaries = ln(ucrburglary/pop) if year==1993 & ln_burglaries==.  
  
* run with interpolated values for 1993 (avg of '92 and '94)
xtreg ln_homrate ln_lfss ln_robberies ln_burglaries i.year if include==1 ///
 [aweight=pop90], fe vce(cl fips)
est store col2a
/* tested using 1992 or 1994 or average over entire period for 1993 crime
 rates but can't match the statistical significance or coefficient for 
 robberies as quoted in the paper.  Perhaps data in 1993 is very different
 than these estimated values, or I still don't have the right sample entries.  
 Impossible to determine the differences without original data set. */

* replicate column 3 - add other population characteristics
xtreg ln_homrate ln_lfss ln_pblack ln_purban ln_sameres ln_pfhh i.year ///
 if include==1 [aweight=pop90], fe vce(cl fips)
est store col3a
/* More problems here matching the paper's results.  The paper's result for
the coefficient on same residence of 10 must be a typo, and should be 1.02, 
but see pdf for more comments. Again, tested changing the sample entries,
chaning the years, chaning to non lagged FSS, just can't come up with any
scenario across all 4 variants that provides consistently similar results
to what the paper shows.  */

* relplicate column 4, full model
xtreg ln_homrate ln_lfss ln_robberies ln_burglaries ln_pblack ln_purban ///
 ln_sameres ln_pfhh i.year if include==1 [aweight=pop90], fe vce(cl fips)
est store col4a

/* areg vs xtreg & issue of robust vs clustered errors
I also ran the 4 variations using areg which allows us to use pop for the 
weighting instead of pop90 and allows for the use of robust vs clustered 
errors. */

* Now rerun, using AREG and clustered errors
* column 1
areg ln_homrate ln_lfss i.year if include==1 [aweight=pop], absorb(fips) ///
 cluster(fips)
est store col1b
 * column 2
areg ln_homrate ln_lfss ln_robberies ln_burglaries i.year if include==1 ///
 [aweight=pop], absorb(fips) cluster(fips)
est store col2b
 * column 3
areg ln_homrate ln_lfss ln_pblack ln_purban ln_sameres ln_pfhh i.year ///
 if include==1 [aweight=pop], absorb(fips) cluster(fips)
est store col3b
 * column 4
areg ln_homrate ln_lfss ln_robberies ln_burglaries ln_pblack ln_purban ///
 ln_sameres ln_pfhh i.year if include==1 [aweight=pop], absorb(fips) cluster(fips)
est store col4b
* end up with lower coefficients and higher error.  Higher error due to 
* the lower degrees of freedom.  Coefficient differences probably due to 
* different weighting metric - pop (changing) and not pop90 (fixed)
 
* Rerun AREG but with robust errors instead
* column 1
areg ln_homrate ln_lfss i.year if include==1 [aweight=pop], absorb(fips) ///
 vce(r)
est store col1c
 * column 2
areg ln_homrate ln_lfss ln_robberies ln_burglaries i.year if include==1 ///
 [aweight=pop], absorb(fips) vce(r)
est store col2c
 * column 3
areg ln_homrate ln_lfss ln_pblack ln_purban ln_sameres ln_pfhh i.year ///
 if include==1 [aweight=pop], absorb(fips) vce(r)
est store col3c
 * column 4
areg ln_homrate ln_lfss ln_robberies ln_burglaries ln_pblack ln_purban ///
 ln_sameres ln_pfhh i.year if include==1 [aweight=pop], absorb(fips) vce(r)
est store col4c

* Rerun columns 1-4 using all years 1980 through 2004
* Generate new indicator for correct sample    
gen include04 = 0
label var include04 "1980-2004 Indicator"
replace include04 = 1 if include==1
replace include04 = 1 if year>=2000 & year<=2004
replace include04 = 0 if fips==6097 & year==2001
replace include04 = 0 if fips==6097 & year==2002
global regvar ln_homrate ln_lfss ln_robberies ln_burglaries ln_pblack ln_purban ///
 ln_sameres ln_pfhh 

foreach k of global regvar {
replace include04=0 if `k'==. & year>=2000 & year<=2004
}
* C1
xtreg ln_homrate ln_lfss i.year if include04==1 [aweight=pop90], fe vce(cl fips)
est store col1d
* C2
xtreg ln_homrate ln_lfss ln_robberies ln_burglaries i.year if include04==1 ///
 [aweight=pop90], fe vce(cl fips)
est store col2d
* C3
xtreg ln_homrate ln_lfss ln_pblack ln_purban ln_sameres ln_pfhh i.year ///
 if include04==1 [aweight=pop90], fe vce(cl fips)
est store col3d
* C4
xtreg ln_homrate ln_lfss ln_robberies ln_burglaries ln_pblack ln_purban ///
 ln_sameres ln_pfhh i.year if include04==1 [aweight=pop90], fe vce(cl fips)
est store col4d

* output tables for comparison
est restore col1a
outreg2 using "$workdir/leonard_table2compare.xls", replace label nonotes ///
   title(Table 2, "Baseline results: county-level data 1980-1999", /// 
   Comparison of Methods) ///
   drop(i.year) dec(4) asterisk(se) ctitle([1a])  ///
   addnote(Notes: , ///
   Parenthesis contain standard errors adjusted for serial correlation., ///
   Estimates utilize county population as weight., /// 
   Analytic sample consists of annual observations for 196 largest counties, ///
   in U.S. over the period 1980-1999., ///
   Column a represents XTREG with clustered errors, ///
   Column b represents AREG with clustered errors, ///
   Column c represents AREG with robust errors, ///
   Column d represents XTREG with clustered errors for years 1980-2004, ///
   *** Significant at the 1 percent level., ///
   ** Significant at the 5 percent level., ///
   * Significant at the 10 percent level. ) ///
   addtext(Year Fixed Effects?, Yes, County Fixed Effects?, Yes) ///
   excel 

foreach k in "1b" "1c" "1d" "2a" "2b" "2c" "2d" "3a" "3b" "3c" "3d" "4a" ///
"4b" "4c" "4d" {
  est restore col`k'
  outreg2 using "$workdir/leonard_table2compare.xls", append label nonotes ///
    drop(i.year) dec(3) asterisk(se) ctitle([`k'])  ///
    addtext(Year Fixed Effects?, Yes, County Fixed Effects?, Yes) excel
}  
   
  
* Note on scaling: I don't use robbery, burglary or homicide, etc. rates per 
* 100,000 population as shown in the descriptive statistics, since our 
* assignment didn't indicate this in question 4's equation, but a scalar  
* transformation of these variables will not impact the results.  For example:

gen ln_homrate2 = ln(e96/pop*100000) if include==1
label var ln_homrate2 "Homicide rate per 100k"
gen ln_lfss2 = ln(l.e955/l.e95*100) if year>=1980 & include==1
label var ln_lfss2 "Firearm suicide rate per 100"
gen ln_robberies2 = ln(ucrrobbery/pop*100000) if include==1
label var ln_robberies2 "Robbery rate per 100k"
gen ln_burglaries2 = ln(ucrburglary/pop*100000) if include==1
label var ln_burglaries2 "Burglary rate per 100k"
gen ln_pblack2 = ln(black/pop*100) if include==1
label var ln_pblack2 "Black per 100"
gen ln_purban2 = ln(100-(rural/pop*100)) if include==1
label var ln_purban2 "Urban per 100"
gen ln_pfhh2 = ln(fhh/households*100) if include==1
label var ln_pfhh2 "Female Head per 100"
gen ln_sameres2 = ln(resid5yago/pop*100) if include==1
label var ln_sameres2 "Same house per 100"
* replicate column 4, full model
xtreg ln_homrate2 ln_lfss2 ln_robberies2 ln_burglaries2 ln_pblack2 ln_purban2 ///
 ln_sameres2 ln_pfhh2 i.year if include==1 [aweight=pop90], fe vce(cl fips)
* still get same results with rescaling


* Serial Correlation is common in time series
xtserial ln_homrate ln_lfss ln_robberies ln_burglaries ln_pblack ln_purban ///
 ln_sameres ln_pfhh if include==1  
* F stat of 17.32 means we reject the null hypothesis of no first order
* auto correlation, so the model has serial correlation problems still
* but measures error term from the mean, not the regular error 
xtserial ln_homrate ln_lfss ln_robberies ln_burglaries ln_pblack ln_purban ///
 ln_sameres ln_pfhh if include04==1  
* same result
* Hence the paper's use of robust errors to correct for some autocorrelation
* and now use of clustered errors.  May have problems if not stationary 
* and weakly dependent however.
 
 
********************************************************
* Q.5 Add population to the full model from column 4   
 
gen ln_pop = ln(pop)
label var ln_pop "Natural Log of Population" 
   
* full model with population added, using clustered errors
xtreg ln_homrate ln_lfss ln_robberies ln_burglaries ln_pblack ln_purban ///
 ln_sameres ln_pfhh ln_pop i.year if include==1 [aweight=pop90], fe vce(cl fips)
est store quest5

* now add years 2000-2004
xtreg ln_homrate ln_lfss ln_robberies ln_burglaries ln_pblack ln_purban ///
 ln_sameres ln_pfhh ln_pop i.year if include04==1 [aweight=pop90], ///
 fe vce(cl fips)
est store quest5b

* output table
est restore quest5
outreg2 using "$workdir/leonard_quest5.xls", replace label nonotes ///
   title(Question 5: county-level data) ///
   drop(i.year) dec(3) asterisk(se) ctitle([Q5])  ///
   addtext(Year Fixed Effects?, Yes, County Fixed Effects?, Yes) ///
   excel 

est restore quest5b
outreg2 using "$workdir/leonard_quest5.xls", append label nonotes ///
    drop(i.year) dec(3) asterisk(se) ctitle([Q5b])  ///
    addtext(Year Fixed Effects?, Yes, County Fixed Effects?, Yes) excel   
 
    
********************************************************
* Q.6 Estimate revised model  
   
* generate new variables
gen ln_hom = ln(e96) 
gen ln_rob = ln(ucrrobbery) 
gen ln_burg = ln(ucrburglary) 
gen ln_bl = ln(black)     
gen urban = pop-rural 
gen ln_urb = ln(urban) 
gen ln_sh = ln(resid5yago) 

label var ln_hom "Natural log of # homicides"
label var ln_rob "Natural log of # robberies"
label var ln_burg "Natural log of # burglaries"
label var ln_bl "Natural log of # Afr Amer"
label var urban "# of urbanites"  
label var ln_urb "Natural log of # urbanites" 
label var ln_sh "Natural log of # residents who did not move"  

xtreg ln_hom ln_lfss ln_rob ln_burg ln_bl ln_urb ln_sh ln_pfhh ln_pop i.year ///
 if include==1 [aweight=pop90], fe vce(cl fips)      
est store quest6
outreg2 using "$workdir/leonard_quest6.xls", replace label nonotes ///
   title(Question 6: county-level data) ///
   drop(i.year) dec(3) asterisk(se) ctitle([Q6])  ///
   addtext(Year Fixed Effects?, Yes, County Fixed Effects?, Yes) ///
   excel 

* now evaluate model with years through 2004
xtreg ln_hom ln_lfss ln_rob ln_burg ln_bl ln_urb ln_sh ln_pfhh ln_pop i.year ///
 if include04==1 [aweight=pop90], fe vce(cl fips)      
est store quest6b

est restore quest6b
outreg2 using "$workdir/leonard_quest6.xls", append label nonotes ///
    drop(i.year) dec(3) asterisk(se) ctitle([Q6b])  ///
    addtext(Year Fixed Effects?, Yes, County Fixed Effects?, Yes) excel   

	
********************************************************
* Q.7 Test the validity of the relevant restriction
* use test command for linear restriction imposed on delta, coeff of pop
* reun Q6 regression, using the 1980-1999 model
xtreg ln_hom ln_lfss ln_rob ln_burg ln_bl ln_urb ln_sh ln_pfhh ln_pop i.year ///
 if include==1 [aweight=pop90], fe vce(cl fips)      

test ln_pop = (1-ln_rob-ln_burg-ln_bl-ln_urb-ln_sh) 

* reun Q.6 regression, using the 1980-2004 model
xtreg ln_hom ln_lfss ln_rob ln_burg ln_bl ln_urb ln_sh ln_pfhh ln_pop i.year ///
 if include04==1 [aweight=pop90], fe vce(cl fips)      

test ln_pop = (1-ln_rob-ln_burg-ln_bl-ln_urb-ln_sh) 
* this constraint doesn't work for this data, reject null hypothesis

* set up a new general model and test restriction, 1980-1999 model
* set up required variables
gen ln_fasuic = ln(l.e955) if include==1 | include04==1    
gen ln_suic = ln(l.e95) if include==1 | include04==1
gen ln_fhh = ln(fhh) if include==1 | include04==1
gen ln_house = ln(households) if include==1 | include04==1

label var ln_fasuic "Natural log of # firearm sucides 1 yr lag"
label var ln_suic "Natural log of # suicides 1 yr lag"
label var ln_fhh "Natural log of # female head of households"
label var ln_house "Natural log of # households"

xtreg ln_hom ln_fasuic ln_suic ln_rob ln_burg ln_bl ln_urb ln_sh ln_fhh ///
 ln_house ln_pop i.year if include==1 [aweight=pop90], fe vce(cl fips)

test ln_pop = (1-ln_rob-ln_burg-ln_bl-ln_urb-ln_sh)
test ln_fasuic = -(ln_suic), accum
test ln_fhh = -(ln_house) , accum
 
*1980-2004 model
xtreg ln_hom ln_fasuic ln_suic ln_rob ln_burg ln_bl ln_urb ln_sh ln_fhh ///
 ln_house ln_pop i.year if include04==1 [aweight=pop90], fe vce(cl fips)

test ln_pop = (1-ln_rob-ln_burg-ln_bl-ln_urb-ln_sh)
test ln_fasuic = -(ln_suic), accum
test ln_fhh = -(ln_house) , accum

	
********************************************************
* Q.8 Apply constraint   
* 1980-1999 Model
constraint 1 ln_pop = (1-ln_rob-ln_burg-ln_bl-ln_urb-ln_sh)
cnsreg ln_hom ln_lfss ln_rob ln_burg ln_bl ln_urb ln_sh ln_pfhh ln_pop ///
 i.fips i.year if include==1 [aweight=pop90], vce(cl fips) c(1)      
* results match those from Q4, so constraint is verified
* fe option isn't allowed in cnsreg, so add i.fips
est store quest8a
outreg2 using "$workdir/leonard_quest8.xls", replace label nonotes ///
    drop(i.year) dec(3) asterisk(se) ctitle([Q8a])  ///
    addtext(Year Fixed Effects?, Yes, County Fixed Effects?, Yes) excel   

* 1980-2004 Model
cnsreg ln_hom ln_lfss ln_rob ln_burg ln_bl ln_urb ln_sh ln_pfhh ln_pop ///
 i.fips i.year if include04==1 [aweight=pop90], vce(cl fips) c(1)      
est store quest8b
outreg2 using "$workdir/leonard_quest8.xls", append label nonotes ///
    drop(i.year) dec(3) asterisk(se) ctitle([Q8b])  ///
    addtext(Year Fixed Effects?, Yes, County Fixed Effects?, Yes) excel   

	
********************************************************
* Q.9 Estimate equation 1.2 using First Differences
* Removes county fixed effects

* 1980-1999, we need to have 2 years of data inclusion to include here
xi: reg D.(ln_hom ln_lfss ln_rob ln_burg ln_bl ln_urb ln_sh ///
 ln_pfhh ln_pop i.year) if include==1 & l.include==1 ///
 [aweight=pop90], vce(cl fips)  
est store quest9a
outreg2 using "$workdir/leonard_quest9.xls", replace label nonotes ///
    drop(i.year) dec(3) asterisk(se) ctitle([Q9a])  ///
    addtext(Year Fixed Effects?, Yes) excel   

* 1980-2004
xi: reg D.(ln_hom ln_lfss ln_rob ln_burg ln_bl ln_urb ln_sh ///
 ln_pfhh ln_pop i.year) if include04==1 & include04==1 ///
  [aweight=pop90], vce(cl fips)      
est store quest9b
outreg2 using "$workdir/leonard_quest9.xls", append label nonotes ///
    drop(i.year) dec(3) asterisk(se) ctitle([Q9b])  ///
    addtext(Year Fixed Effects?, Yes) excel   
* Proxy is no longer significant, effect is not significantly differnt than 0

	
********************************************************
* Q.10 Additional Analysis

* replicate Table 3 alternative specifications for baseline model, weighted
gen ln_gunrate = ln(e965/pop)
label var ln_gunrate "Natural Log of Gun Homicide Rate"
gen nongun = e96-e965
label var nongun "Nongun homicides"
gen ln_nongun = ln(nongun/pop)
label var ln_nongun "Natural Log of Non Gun Homicide Rate"
* 1980-99 only
xtreg ln_gunrate ln_lfss ln_robberies ln_burglaries ln_pblack ln_purban ///
 ln_sameres ln_pfhh i.year if include==1 & ln_gunrate!=. & ln_nongun!=. ///
 [aweight=pop90], fe vce(cl fips)
est store gunw
outreg2 using "$workdir/leonard_quest10.xls", replace label nonotes ///
    drop(i.year) dec(3) asterisk(se) ctitle([gunw])  ///
    addtext(Year Fixed Effects?, Yes) excel   
* nongun rate
xtreg ln_nongun ln_lfss ln_robberies ln_burglaries ln_pblack ln_purban ///
 ln_sameres ln_pfhh i.year if include==1 & ln_gunrate!=. & ln_nongun!=. ///
 [aweight=pop90], fe vce(cl fips)
est store nongunw
outreg2 using "$workdir/leonard_quest10.xls", append label nonotes ///
    drop(i.year) dec(3) asterisk(se) ctitle([nongunw])  ///
    addtext(Year Fixed Effects?, Yes) excel   

* run non weighted model
* replicate table 2 model first, using consistent sample, removing MVs
xtreg ln_homrate ln_lfss ln_robberies ln_burglaries ln_pblack ln_purban ///
 ln_sameres ln_pfhh i.year if include==1 & ln_gunrate!=. & ln_nongun!=. ///
 , fe vce(cl fips)
est store hrunw
outreg2 using "$workdir/leonard_quest10.xls", append label nonotes ///
    drop(i.year) dec(3) asterisk(se) ctitle([hrunw])  ///
    addtext(Year Fixed Effects?, Yes) excel   
* gun hom rate unweighted
xtreg ln_gunrate ln_lfss ln_robberies ln_burglaries ln_pblack ln_purban ///
 ln_sameres ln_pfhh i.year if include==1 & ln_gunrate!=. & ln_nongun!=. ///
 , fe vce(cl fips)
est store gununw
outreg2 using "$workdir/leonard_quest10.xls", append label nonotes ///
    drop(i.year) dec(3) asterisk(se) ctitle([gununw])  ///
    addtext(Year Fixed Effects?, Yes) excel   
* nongun rate unweighted
xtreg ln_nongun ln_lfss ln_robberies ln_burglaries ln_pblack ln_purban ///
 ln_sameres ln_pfhh i.year if include==1 & ln_gunrate!=. & ln_nongun!=. ///
 , fe vce(cl fips)
est store nongununw
outreg2 using "$workdir/leonard_quest10.xls", append label nonotes ///
    drop(i.year) dec(3) asterisk(se) ctitle([nongununw])  ///
    addtext(Year Fixed Effects?, Yes) excel   

* check first differences for gunrate	
xi: reg D.(ln_gunrate ln_lfss ln_rob ln_burg ln_bl ln_urb ln_sh ///
 ln_pfhh ln_pop i.year) if include==1 & l.include==1 ///
 & ln_gunrate!=. & ln_nongun!=. & l.ln_gunrate!=. & l.ln_nongun!=. ///
 [aweight=pop90], vce(cl fips)  
est store fdgunw
outreg2 using "$workdir/leonard_quest10.xls", append label nonotes ///
    drop(i.year) dec(3) asterisk(se) ctitle([fdgunw])  ///
    addtext(Year Fixed Effects?, Yes) excel   

* check first differences for unweighted gun rate
xi: reg D.(ln_gunrate ln_lfss ln_rob ln_burg ln_bl ln_urb ln_sh ///
 ln_pfhh ln_pop i.year) if include==1 & l.include==1 ///
 & ln_gunrate!=. & ln_nongun!=. & l.ln_gunrate!=. & l.ln_nongun!=. ///
 , vce(cl fips)  
est store fdgununw
outreg2 using "$workdir/leonard_quest10.xls", append label nonotes ///
    drop(i.year) dec(3) asterisk(se) ctitle([fdgununw])  ///
    addtext(Year Fixed Effects?, Yes) excel   

* check first differences for unweighted nongun rate
xi: reg D.(ln_nongun ln_lfss ln_rob ln_burg ln_bl ln_urb ln_sh ///
 ln_pfhh ln_pop i.year) if include==1 & l.include==1 ///
 & ln_gunrate!=. & ln_nongun!=. & l.ln_gunrate!=. & l.ln_nongun!=. ///
 , vce(cl fips)  
est store fdnonunw
outreg2 using "$workdir/leonard_quest10.xls", append label nonotes ///
    drop(i.year) dec(3) asterisk(se) ctitle([fdnonunw])  ///
    addtext(Year Fixed Effects?, Yes) excel   

save "$workdir\final_edited.dta", replace
	
	
* Test the idea that Homicide rate and Gun ownership proxy have unit root
* I chose to use the HT unit root test, but others could work as well
* requires a balanced panel, so need to drop nonbalanced data (missings)
tab fips if e96==.
tab fips if e955==.	
tab fips if pop==.
tab fips if e95==.
drop if fips==6097 | fips==9011 | fips==25021 | fips==27037 | fips==34029 ///
 | fips==49049 | fips==55133
 
gen hrate = ln(e96/pop) 
xtunitroot ht hrate, trend demean
* p-value of 0 so reject the null hypothesis that Panel contains unit root
* test # homicides
xtunitroot ht e96, trend demean // accept null, unit root
* so incoporating population into the denominator causes unit root to disappear

* check proxy
gen lfss=ln(l.e955/l.e95)
xtunitroot ht lfss if year>=1980, demean trend 
* unit root for RHS independent variable
xtunitroot ht l.e955 if year>=1980, demean trend
* unit root

save "$workdir\final_Q10edited.dta", replace

	
********************************************************
* Q.11

* save data - saved in question 10 before unit root testing
* reload that data
use "$workdir\final_edited.dta", replace
 
* close the log file 
log close
