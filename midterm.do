* Development Microeconometrics using STATA
* Solution for Midterm 
* Robert Leonard

version 15 

set more off, permanently // do not stop after each page

********************************************************
* Setting up the directories

global datadir "C:\Users\roble\Desktop\Working\STATA\midterm\Data"
global workdir "C:\Users\roble\Desktop\Working\STATA\midterm"

********************************************************

capture log close 
log using "$workdir/leonard_midterm.log", replace

********************************************************
* Q.1 Set up dataset for Unemployment
* import the CSV data file and specify variable names row
import delimited "$datadir\unrate.csv", varnames(1)
* generate a Stata date from the date filed
gen date2=date(date,"YMD")
format date2 %td
* split out month and year components
gen year=year(date2)
gen month=month(date2)
*adjust date to take into account March-February calendar year
replace year=year+1 if month>=3
replace month=month-2
replace month=11 if month==-1
replace month=12 if month==0
* collapse to annual average unemployment rate
collapse (mean) unrate, by(year)
* save the data file
save "$workdir\unrate.dta", replace


********************************************************
* Q.2 Set up dataset for GDP
* import the CSV data file and specify variable names row
import delimited "$datadir\GDPC96.csv", varnames(1) clear
* generate a Stata date from the date filed
gen date2=date(date,"YMD")
format date2 %td
* split out month and year components
gen year=year(date2)
gen month=month(date2)
* adjust date
replace year=year+1 if month!=1
* collaps to annual average GDP
collapse (mean) gdpc96, by(year)
* generate log of gdp
gen lgdp = log(gdpc96)
* save the data file
save "$workdir\gdpc96.dta", replace


********************************************************
* Q.3
* import the CSV data file and specify variable names row
import delimited "$datadir\bds_f_ageisz_release.csv", varnames(1) clear
* Recalc the denominator 
gen denom_test = (emp + emp - job_creation + job_destruction)/2
* Compare values
gen net_denom = denom - denom_test
sum denom denom_test net_denom
sort net_denom
br year2 emp job_creation job_destruction denom denom_test net_denom
sum net_denom if abs(net_denom)>1
tab net_denom if abs(net_denom)>0
tab year2 if net_denom==.
tab denom if net_denom==.
tab ifsize d_flag if net_denom==. 
tab d_flag if denom==.
tab ifsize d_flag if denom==.
tab fage4 if denom==.

* Recalc the job creation rate
gen job_creation_rate_test = 100 * (job_creation/denom) 
* Compare values
gen net_creation_rate_test = (job_creation_rate - job_creation_rate_test)
sum job_creation_rate job_creation_rate_test net_creation_rate_test
sort net_creation_rate_test
br year2 denom job_creation_rate job_creation_rate_test net_creation_rate_test

* Recalc the job creation rate, but add rounding to tenths
replace job_creation_rate_test = round(100 * (job_creation/denom),.1) 
* Compare values
replace net_creation_rate_test = (job_creation_rate - job_creation_rate_test)
sum job_creation_rate job_creation_rate_test net_creation_rate_test
sort net_creation_rate_test
br year2 denom job_creation_rate job_creation_rate_test net_creation_rate_test
tab d_flag if job_creation_rate==.


********************************************************
* Q.4 Generate age and size Variables
* Age is defined by a string variable, fage4
sort year2 fage4 ifsize
codebook fage4
tab fage4
gen fage4_code = substr(fage4,1,1)
tab fage4_code
* Set all firms labeled a-f as young (or 1)
* note, from 1977 until 1981 we have a problem with those firms labled
* as "left censored" (established before 1977).  By 1982 all of these firms
* would be labled as mature firms, but for 1977 through 1981 we cannot determine
* if these firms are young or mature.  So for now, they have been left blank
* in terms of the variable age, for those years.
gen float age = 1 if fage4_code=="a" | fage4_code=="b"  | fage4_code=="c" ///
  | fage4_code=="d"  | fage4_code=="e" | fage4_code=="f"
* Set age code to 0 for mature firms, leaving out those left censored from above
replace age = 0 if fage4_code=="g" | fage4_code=="h" | /// 
  fage4_code=="i" | fage4_code=="j" | fage4_code=="k"     
replace age = 0 if fage4_code=="l" & year2>=1982   
tab year2 fage4 if age==.
* Drop the "left censored" data from 1977 to 1981 because we cannot classify
* it as young or mature.  Also, we will be dropping all years prior to 1982 
* later on anyhow.
drop if age==.
tab age fage4_code 
 
* Set firm size  
codebook ifsize
tab ifsize
gen ifsize_code = substr(ifsize,1,1)
tab ifsize_code 
* now assign size
gen float size = 0 if ifsize_code=="a" |ifsize_code=="b" | ifsize_code=="c" | ///
 ifsize_code=="d" 
replace size = 1 if ifsize_code=="e" |ifsize_code=="f" | ifsize_code=="g" | ///
 ifsize_code=="h" 
replace size = 2 if ifsize_code=="i" |ifsize_code=="j" | ifsize_code=="k" | ///
 ifsize_code=="l" 
tab size ifsize_code

* Reclasify firms with 0 age to being small firms 
replace size = 0 if fage4_code=="a"  
* drop all young and large firms
drop if age==1 & size==2

* set up labels
label define firmage 0 "mature" 1 "young"
label values age firmage
label define firmsize 0 "small" 1 "medium" 2 "large"
label values size firmsize
lab var age "0=mature, 1=young" 
lab var size "0=small, 1=medium, 2=large"
tab age size


********************************************************
* Q.5 Collapse and Rename
* Collapse to get sums 
collapse (sum) denom job_creation job_destruction, by(age size year2) 
*Rename variables 
rename job_creation jc  
rename job_destruction jd 
rename year2 year 
 
 
********************************************************
* Q.6 Reshape the dataset

ssc install sdecode

sdecode age, replace
sdecode size, replace

gen agesize = age + size

drop age size
* Reshape, but keep year as we will be merging in yearly unemployment
* and GDP data
reshape wide denom jc jd, i(year) j(agesize) string
 
 
********************************************************
* Q.7 Generate Subgroups 

* for each year calc denom jc jd for the 5 different subgroups
set type double
global age "young mature"
global size "small medium large"
* loops to generate variable names for new aggregated age and size categories
foreach k of global age {
 gen denom`k' = 0
 lab var denom`k' "`k' denom"
 format denom`k' %12.0g
 gen jc`k' = 0
 lab var jc`k' "`k' jc"
 format jc`k'  %12.0g
 gen jd`k'  = 0
 lab var jd`k' "`k' jd"
 format jd`k' %12.0g
 }
foreach j of global size {
 gen denom`j' = 0
 lab var denom`j' "`j' denom"
 format denom`j' %12.0g
 gen jc`j'  = 0
 lab var jc`j' "`j' jc"
 format jc`j'  %12.0g
 gen jd`j'  = 0
 lab var jd`j' "`j' jd"
 format jd`j'  %12.0g
}
* need a placeholder value for young and large since we dropped these previously
gen denomyounglarge = 0
gen jcyounglarge = 0
gen jdyounglarge = 0

* now generate aggregated variable values using a nested loop  
foreach k of global age {
display "`k'"
 foreach j of global size {
   display "`j'"
   * aggregate on size
   replace denom`j' = denom`j' + denom`k'`j'
   replace jc`j' = jc`j' + jc`k'`j'
   replace jd`j' = jd`j' + jd`k'`j'
   * aggregate for age
   replace denom`k' = denom`k' + denom`k'`j'
   replace jc`k' = jc`k' + jc`k'`j'
   replace jd`k' = jd`k' + jd`k'`j'
   }
}
 
* check totals to make sure summation operations are correct
tabstat denom*, stat(sum) format(%12.0g) columns(statistics) varwidth(24)
tabstat jc*, stat(sum) format(%12.0g) columns(statistics) varwidth(24)
tabstat jd*, stat(sum) format(%12.0g) columns(statistics) varwidth(24)
* missing values are from years 1977 to 1981, which are later deleted anyhow,
* so ignore these discrepancies.  now check variable labels and formatting
describe denom*
describe jc*
describe jd*
 
 
********************************************************
* Q.8 Generate net job creation rates

* define net job creation variable and calculate them
foreach k of global age {
display "`k'"
gen njcr`k' = ((jc`k'/denom`k')-(jd`k'/denom`k'))
lab var njcr`k' "njcr `k'"
format njcr`k' %7.6g
 foreach j of global size {
 * generate combination variables
      display "`k'`j'"  
      gen njcr`k'`j' = ((jc`k'`j'/denom`k'`j')-(jd`k'`j'/denom`k'`j'))
      lab var njcr`k'`j' "njcr `k'`j'"
      format njcr`k'`j' %7.6g
    }
}

 foreach j of global size {
  display "`j'"
   gen njcr`j' = ((jc`j'/denom`j')-(jd`j'/denom`j'))
   lab var njcr`j' "njcr `j'"
   format njcr`j' %7.6g
 }
 
describe njcr*
 
 
********************************************************
* Q.9 Merge unemployment and GDP

* merge unemployment
merge 1:1 year using "$workdir/unrate.dta"
* merge GDP
merge 1:1 year using "$workdir/gdpc96.dta", generate(_merge2)
* define the data as a yearly time series
tsset year, yearly
* first difference unemployment
gen fdunrate = unrate - l.unrate
label var fdunrate "first differ. unem. rate"
* first difference log GDP
gen fdlgdp = lgdp - l.lgdp
label var fdlgdp "first differ. log GDP"
* Drop years before 1982 and after 2014
drop if year < 1982 | year > 2014
 
 
********************************************************
* Q.10 Generate Cyclical Deviations of NJCR

* create njcryounglarge placeholder for loop then drop later
replace njcryounglarge = 0

foreach k of global age {
tsfilter hp cycnjcr`k' = njcr`k', smooth(390.625) 
 foreach j of global size {
    * generate combination variables  
    tsfilter hp cycnjcr`k'`j' = njcr`k'`j', smooth(390.625) 
    }
}

 foreach j of global size {
tsfilter hp cycnjcr`j' = njcr`j', smooth(390.625)   
 }
 
 
********************************************************
* Q.11 Calculate differential job creation rates

* 1) Between small and large
gen njcr_sl = cycnjcrsmall - cycnjcrlarge 
label var njcr_sl "Diff. of cyc. btw. small & large" 
* 2) Between young and mature
gen njcr_ym = cycnjcryoung - cycnjcrmature
label var njcr_ym "Diff. of cyc. btw. young & mature" 
* 3) Between small and large for mature firms
gen njcr_msml = cycnjcrmaturesmall - cycnjcrmaturelarge
label var njcr_msml "Diff. of cyc. btw. mature small & large" 
* 4) Between young and mature for small firms
gen njcr_ysms = cycnjcryoungsmall - cycnjcrmaturesmall
label var njcr_ysms "Diff. of cyc. btw. small young & mature" 
 
 
********************************************************
* Q.12 Plot

* Plot 1 - Cycl. unem. rate and differential for small and large
* First determine min/max for determining axis specifications
sum fdunrate njcr_sl
/* Matching the graph style exactly was a little complicated - the shaded
regions for NBER recessions are, I think, based on 2 consecutive quarters of
negative GDP "growth", but we only have yearly data in our revised dataset.
Getting the recession highlights to look the same and match the quarterly data
would require reverting to quarterly time series data.  Instead, I have 
visually approximated the recession highlighting to match the quarterly periods 
using xlines.  I had trouble getting the xline color bar to show in the legend
so I have added a third graph called recshade with essentially 0 area.  This
allows the grey bar to now show up in the legend for the areas of NBER 
recessions, which correspond to the xlines.  I later drop the recshade data as 
it is not relevant.

Second, the yline at the origin get covered by the xline recession highlighting.
So I have moved the yline to axis 2 for to better match the look of the graph.
I approximated the correct intercept point on axis 2 to match the origin of
axis 1. */
gen recshade = 0
* calculate the correlation and store in a local macro
corr fdunrate njcr_sl
local corrco = r(rho)
twoway(line fdunrate year, yaxis(1) color(blue) fcolor(none) ///
     yscale(range(-.04 .04))) ///
 (line njcr_sl year, yaxis(2) color(red) fcolor(none)) ///
 (area recshade year, bcolor(gs13)), ///  
  yscale(range(-2 3)) xscale(range(1982 2014)) xlabel(1982(3)2015, grid) ///
  ylabel(-2(1)3) title("{bf:Cyclical Measures}") ///
legend(label(1 "unemployment rate (left axis)") ///
label(3 "diff. small-large (right axis)")  ///
label(2 "NBER recession") ///
label(4 "Corr: `: di %3.2f `corrco''") ///
 order(2 1 3 4) region(lwidth(0) lcolor(none))) ///
 scheme(s1color)  ///
xtitle("") ytitle("") ytitle("",axis(2)) ///
 xline(1982.2, lwidth(4.5) lcolor(gs13) noextend) ///
 xline(1990.7, lwidth(2) lcolor(gs13) noextend) ///
 xline(2001.5, lwidth(3) lcolor(gs13) noextend) ///
 xline(2008.5, lwidth(6) lcolor(gs13) noextend) ///
 yline(-.0096, axis(2) lcolor(black) noextend)  

graph export "$workdir/Graph1.pdf", replace

* Plot 2 - Cycl. unem. rate and differential for young and mature

sum fdunrate njcr_ym

corr fdunrate njcr_ym
local corrco = r(rho)

twoway(line fdunrate year, yaxis(1) color(blue) fcolor(none) ///
     yscale(range(-.04 .04))) ///
 (line njcr_ym year, yaxis(2) color(red) fcolor(none)) ///
 (area recshade year, bcolor(gs13)), ///  
  yscale(range(-2 3)) xscale(range(1982 2014)) xlabel(1982(3)2015, grid) ///
  ylabel(-2(1)3) title("{bf:Cyclical Measures}") ///
legend(label(1 "unemployment rate (left axis)") ///
label(3 "diff. young-mature (right axis)")  ///
label(2 "NBER recession") ///
label(4 "Corr: `: di %3.2f `corrco''") ///
 order(2 1 3 4) region(lwidth(0) lcolor(none))) ///
 scheme(s1color)  ///
xtitle("") ytitle("") ytitle("",axis(2)) ///
 xline(1982.2, lwidth(4.5) lcolor(gs13) noextend) ///
 xline(1990.7, lwidth(2) lcolor(gs13) noextend) ///
 xline(2001.5, lwidth(3) lcolor(gs13) noextend) ///
 xline(2008.5, lwidth(6) lcolor(gs13) noextend) ///
 yline(-.022, axis(2) lcolor(black) noextend)  

graph export "$workdir/Graph2.pdf", replace
 
 
********************************************************
* Q.13 Correlations and Significance

pwcorr fdunrate fdlgdp njcr_sl, sig
pwcorr fdunrate fdlgdp njcr_ym, sig
pwcorr fdunrate fdlgdp njcr_msml, sig
pwcorr fdunrate fdlgdp njcr_ysms, sig
 
********************************************************
* Q.14 Save data
* drop the younglarge placeholders, merge codes, unneeded data
drop denomyounglarge
drop jcyounglarge
drop jdyounglarge
drop njcryounglarge
drop _merge
drop _merge2
drop recshade
drop cycnjcryounglarge

*sort the data
sort year

* save data
save "$workdir\midterm.dta", replace
 
* close the log file 
log close
