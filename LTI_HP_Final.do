********************************************************************************
************************ HOUSE PRICE GROWTH RESULTS ****************************
********************************************************************************

*** Data:

** House price index (two types. Using all transactions (c.1), using repeat postcode-type transactions (c. 2-6)) ===> PUBLIC
* The code for the computation of house price index can be found at github.com/frtous/LTI_HP along with additional results 

** Shares of constrained banks and low-income borrowers by local area ===> Extracted from confidential information
** Counties ===> PUBLIC

*global path "/Users/sbkg537/Library/CloudStorage/Dropbox/Research/Peydro_Tous_Uluc_Tripathy_Macropru/Published tables and figures/" // ADD PATH IN QUOTES ""
*cd $path

cd "/Users/sbkg537/Library/CloudStorage/Dropbox/Research/Peydro_Tous_Uluc_Tripathy_Macropru/Published tables and figures/"

********** Table 10A

***** COLUMN 1: 25TH PERCENTILE WITH ALL TRANSACTIONS *****

*** Load house price index data

use Land_registry_clean_semiannual.dta, clear

drop if period_s == . // 5,569 observations

* S12000026 only one period

drop if lau117cd == "S12000026" // now 16 periods x 348 LA = 5,568 observations

*** Merging with other data:

* Shares of treatment and low income: shares_local_auth_FINAL
merge m:1 lau117cd using shares_la.dta, /*
*/ keepusing(lau117cd region dconstr dlowinc)

keep if _merge == 3 // all master data matched
drop _merge

*** Creation of dependent variables:
egen lac = group(lau117cd)
xtset lac period_s

*** Annual change in prices:
gen lp25 = log(price_p25)
gen c25 = lp25 - l2.lp25

*** Windsorising:
gen c25w = c25
sum c25, d
replace c25w = `r(p1)' if c25 < `r(p1)'
replace c25w = `r(p99)' if c25 > `r(p99)' & c25 != .

*** Counties:
merge m:1 lau117cd using Counties.dta
drop _merge


*** Generate time dummies:

gen policy = 0 // captures the post-policy period (2014Q3 - 2018Q2)
replace policy = 1 if period_s >= 9

gen brexit = 0 // captures the post-referendum period (2016Q3 - 2018Q2)
replace brexit = 1 if period_s >= 13


*** Generate fixed effects
egen region_time = group(region period_s)
egen region_time_const_FE = group(region_time dconstr)
egen region_time_lowinc_FE = group(region_time dlowinc)
egen county_time = group(cty19cd period_s)


*** REGRESSIONS

* c.1
reghdfe c25w /*
*/ c.brexit##c.dconstr##c.dlowinc /*
*/ c.policy##c.dconstr##c.dlowinc /*
*/ if period_s >= 5 & period_s <= 16, /*
*/ absorb(region_time_const_FE region_time_lowinc_FE county_time lac) vce(cluster lac)
outreg2 using "LTI_T10A.tex", keep(c.brexit#c.dconstr#c.dlowinc c.policy#c.dconstr#c.dlowinc) nocons dec(3) tex replace


***** COLUMNS 2 AND 3: P25 / P75 WITH REPEAT POSTCODE-TYPE *****

use HP_annual_pcd.dta, clear

merge m:1 lau117cd using shares_la.dta, /*
*/ keepusing(lau117cd region dconstr dlowinc)

keep if _merge == 3
drop _merge


*** Creation of dependent variables:

egen lac = group(lau117cd)
xtset lac period_a


*** Annual change in prices:

gen lp25 = log(price_p25)
bysort lac: gen c25 = lp25 - l.lp25

gen lp75 = log(price_p75)
bysort lac: gen c75 = lp75 - l.lp75



*** Windsorise:
foreach var of varlist c25 c75 {
gen `var'w = `var'
sum `var', d
replace `var'w = `r(p1)' if `var' < `r(p1)'
replace `var'w = `r(p99)' if `var' > `r(p99)' & `var' != .
}

*** Generate time dummies:
gen policy = 0 // captures the post-policy period (2014Q3 - 2018Q2)
replace policy = 1 if period_a >= 3

gen brexit = 0 // captures the post-referendum period (2016Q3 - 2018Q2)
replace brexit = 1 if period_a >= 5

*** Merge counties
merge m:1 lau117cd using Counties.dta
drop _merge

*** Generate fixed effects:
egen region_time = group(region period_a)
egen region_time_const_FE = group(region_time dconstr)
egen region_time_lowinc_FE = group(region_time dlowinc)
egen county_time = group(cty19cd period_a)

*** Regressions

* c.2
reghdfe c25w /*
*/ c.brexit##c.dconstr##c.dlowinc /*
*/ c.policy##c.dconstr##c.dlowinc /*
*/ if period_a >= 1 & period_a <= 6, /*
*/ absorb(region_time_const_FE region_time_lowinc_FE county_time lac) vce(cluster lac)
outreg2 using "LTI_T10A.tex", keep(c.brexit#c.dconstr#c.dlowinc c.policy#c.dconstr#c.dlowinc) nocons dec(3) tex

* c.3
reghdfe c75w /*
*/ c.brexit##c.dconstr##c.dlowinc /*
*/ c.policy##c.dconstr##c.dlowinc /*
*/ if period_a >= 1 & period_a <= 6, /*
*/ absorb(region_time_const_FE region_time_lowinc_FE county_time lac) vce(cluster lac)
outreg2 using "LTI_T10A.tex", keep(c.brexit#c.dconstr#c.dlowinc c.policy#c.dconstr#c.dlowinc) nocons dec(3) tex


***** COLUMNS 4 AND 5: BOTTOM AND TOP TERCILES WITH REPEAT POSTCODE-TYPE *****

clear
use House_prices_pcdnew.dta

*** Tertiales

gen perc0 = .

replace perc0 = 1 if price < price_p33 & period_a == 0
replace perc0 = 2 if price >= price_p33 & price < price_p66 & period_a == 0
replace perc0 = 3 if price >= price_p66 & price != . & period_a == 0

bysort pcds property_type: egen perc = mean(perc0)

collapse(mean) price (sum) num, by(period_a lau117cd perc)

merge m:1 lau117cd using shares_la.dta, /*
*/ keepusing(lau117cd region dconstr dlowinc)

keep if _merge == 3
drop _merge


*** Creation of dependent variables:

egen lac = group(lau117cd)
*xtset lac period_a

gen log_price = log(price)


*** Annual change in prices

forvalues x = 1 / 3 {
sort lac perc period_a
bysort lac: gen ch_p_`x' = log_price - log_price[_n-1] if period_a == period_a[_n-1] + 1 & perc == `x' & perc[_n-1] == `x'
}

*** Windsorise

forvalues x = 1 / 3 {
gen chw_p_`x' = ch_p_`x'
sum ch_p_`x', d
replace chw_p_`x' = `r(p1)' if chw_p_`x' < `r(p1)'
replace chw_p_`x' = `r(p99)' if chw_p_`x' > `r(p99)' & chw_p_`x' != .
}


*** Generate time dummies

gen policy = 0 // captures the post-policy period (2014Q3 - 2018Q2)
replace policy = 1 if period_a >= 3

gen brexit = 0 // captures the post-referendum period (2016Q3 - 2018Q2)
replace brexit = 1 if period_a >= 5


*** Merge counties

merge m:1 lau117cd using Counties.dta
drop _merge


*** Generate fixed effects:

egen region_time = group(region period_a)
egen region_time_const_FE = group(region_time dconstr)
egen region_time_lowinc_FE = group(region_time dlowinc)
egen county_time = group(cty19cd period_a)

*** REGRESSIONS:

* c.4
reghdfe chw_p_1 /*
*/ c.brexit##c.dconstr##c.dlowinc /*
*/ c.policy##c.dconstr##c.dlowinc /*
*/ if period_a >= 1 & period_a <= 6, /*
*/ absorb(region_time_const_FE region_time_lowinc_FE county_time lac) vce(cluster lac)
outreg2 using "LTI_T10A.tex", keep(c.brexit#c.dconstr#c.dlowinc c.policy#c.dconstr#c.dlowinc) nocons dec(3) tex

* c.5
reghdfe chw_p_3 /*
*/ c.brexit##c.dconstr##c.dlowinc /*
*/ c.policy##c.dconstr##c.dlowinc /*
*/ if period_a >= 1 & period_a <= 6, /*
*/ absorb(region_time_const_FE region_time_lowinc_FE county_time lac) vce(cluster lac)
outreg2 using "LTI_T10A.tex", keep(c.brexit#c.dconstr#c.dlowinc c.policy#c.dconstr#c.dlowinc) nocons dec(3) tex


***** COLUMN 6: FINANCIAL CRISIS (PLACEBO) WITH REPEAT POSTCODE-TYPE *****

use  HP_FC_pcd_v2.dta, clear

merge m:1 lau117cd using shares_la.dta, /*
*/ keepusing(lau117cd region dconstr dlowinc)

keep if _merge == 3
drop _merge

*** Creation of dependent variables:

egen lac = group(lau117cd)
xtset lac period_a


*** Annual change in prices:

gen lp25 = log(price_p25)
bysort lac: gen c25 = lp25 - l.lp25


*** Windsorise:

gen c25w = c25
sum c25w, d
replace c25w = `r(p1)' if c25 < `r(p1)'
replace c25w = `r(p99)' if c25 > `r(p99)' & c25 != .


*** Generate time dummies:

gen fc = 0 // captures the post-policy period (2014Q3 - 2018Q2)
replace fc = 1 if period_a >= 3


*** Merge counties

merge m:1 lau117cd using Counties.dta
drop _merge


*** Generate fixed effects:

egen region_time = group(region period_a)
egen region_time_const_FE = group(region_time dconstr)
egen region_time_lowinc_FE = group(region_time dlowinc)
egen county_time = group(cty19cd period_a)


*** REGRESSIONS:

* c.6
reghdfe c25w /*
*/ c.fc##c.dconstr##c.dlowinc /*
*/ if period_a >= 1 & period_a <= 4, /*
*/ absorb(region_time_const_FE region_time_lowinc_FE county_time lac) vce(cluster lac)
outreg2 using "LTI_T10A.tex", keep(c.fc#c.dconstr#c.dlowinc) nocons dec(3) tex


********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************
