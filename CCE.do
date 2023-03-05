capture log close
clear
set more off
set matsize 11000
local bwlist="10"			// bandwidth, CDM used 10
local kernel="uniform" // triangular, CDM used uniform
local estimation="2SLS"
local deplist = "DelayCare RationCare WentToHosp"
local nrow=7*wordcount("`bwlist'")
local path="/Users/gregoriocaetano/Dropbox/Work/Research/Published/RDD Multivariate/Data/Replication/" // add your path here
use "`path'medicare.dta", clear			// read the data obtained from CDM, see readme file for source.
rename covered2 Coverage
rename dualcov3 DualCoverage
rename delaymc DelayCare
rename rationmc RationCare
rename inhosp WentToHosp
gen Minority=minority
gen Female=female
keep if year>=1999 & year<=2003
forvalues y=1999(1)2003 {
	gen Y`y'=(year==`y')
}
foreach r in 1 2 3 4 {
	gen R`r'=(region==`r')
}
gen Education=0
replace Education=1 if dropout==1
replace Education=2 if hs==1
replace Education=3 if dropout==0 & hs==0
gen Group_RaceEduc=ngroup
rename age4 Age
gen Z=Age-65
gen D=(Z>=0)
gen DZ=D*Z
gen OnePlus=(Coverage==1)
gen TwoPlus=(DualCoverage==1)
gen DRP=(Education==1)
gen HS=(Education==2)
gen COL=(Education==3)
gen WH=(Minority==0)
gen MI=(Minority==1)
gen NoHosp=1-WentToHosp
rename hispanic Hispanic
keep DelayCare RationCare WentToHosp OnePlus TwoPlus D Minority Education WH MI DRP HS COL Z DZ Age Female R? Y???? Hispanic NoHosp
order DelayCare RationCare WentToHosp OnePlus TwoPlus D Minority Education WH MI DRP HS COL Z DZ Age Female R? Y???? Hispanic NoHosp
keep if DelayCare!=. & WentToHosp~=. & OnePlus!=. & TwoPlus!=.
local Wlist="Minority Education" 
egen Group=group(`Wlist') // wh-DRP=1,wh-HS=2,wh-COL=3,mi-DRP=4,mi-HS=5,mi-COL=6
levelsof Group, local(levels)
foreach ngroup of local levels {
		gen W`ngroup'=(Group==`ngroup')
		gen DW`ngroup'=D*W`ngroup'
		gen ZW`ngroup'=Z*W`ngroup'
		gen DZW`ngroup'=DZ*W`ngroup'
}
label variable DelayCare "Outcome 1: =1 if Delayed Care"
label variable RationCare "Outcome 2: =1 if Rationed Care"
label variable WentToHosp "Outcome 3: =1 if Went to Hospital"
label variable OnePlus "RHS 1: =1 if 1+ insurance policies"
label variable TwoPlus "RHS 2: =1 if 2+ insurance policies"
label variable D "D: =1 if treated, =0 if untreated (Treatment=eligible to Medicare, Age>=65)"
label variable Minority "W: =0 if WH, =1 if MI"
label variable Education "W: =1 if DRP, =2 if HS, =3 if COL"
label variable WH "W: =1 if non-hispanic white"
label variable MI "W: =1 if minority"
label variable DRP "W: =1 if less than HS degree (HS dropout)"
label variable HS "W: =1 if exactly HS degree"
label variable COL "W: =1 if more than HS degree (some college or more)"
label variable Z "Z: =Running variable, Z=Age-65"
label variable DZ "D*Z"
label variable Group "WH-DRP=1,WH-HS=2,WH-COL=3,MI-DRP=4,MI-HS=5,MI-COL=6"
foreach ngroup of local levels {
	label variable W`ngroup' "=1 if Group=`ngroup'"
	label variable DW`ngroup' "D*W`ngroup'"
	label variable ZW`ngroup' "Z*W`ngroup'"	
	label variable DZW`ngroup' "D*Z*W`ngroup'"
}
label variable Age "Age"
label variable Female "=1 if female, =0 if male"
label variable R1 "=1 if geographic region 1 of the country"
label variable R2 "=1 if geographic region 2 of the country"
label variable R3 "=1 if geographic region 3 of the country"
label variable R4 "=1 if geographic region 4 of the country"
label variable Y1999 "=1 if year of sample=1999"
label variable Y2000 "=1 if year of sample=2000"
label variable Y2001 "=1 if year of sample=2001"
label variable Y2002 "=1 if year of sample=2002"
label variable Y2003 "=1 if year of sample=2003"
label variable Hispanic "=1 if hispanic"
foreach bw in `bwlist' {
	gen u = (Z/`bw')
	gen  weight`bw' = 1 - abs(u)
	replace weight`bw' = 0 if u<-1 | u>1
	drop u
}
tempfile temp
save `temp', replace
**************************************************************
**************************************************************
**************************************************************
* Plots
**************************************************************
**************************************************************
**************************************************************
* First Stages
** Unconditional (Figure 1)
foreach rhs in OnePlus TwoPlus {
		egen M`rhs'=mean(`rhs'), by(Age)
		egen tag`rhs'=tag(Age)
		if "`rhs'"=="OnePlus" {
			local `rhs'label="X{sub:1} = One or More Insurance Policies"
		}
		else if "`rhs'"=="TwoPlus" {
			local `rhs'label="X{sub:2} = Two or More Insurance Policies"
		}
		regress `rhs' Z DZ D `weight', vce(cluster Z)
		local alpha_`rhs' = _b[D]
		local SEalpha_`rhs' = _se[D]		
}
twoway (scatter MOnePlus Age if tagOnePlus==1, msymbol(O) msize(small) mcolor(black)) || (scatter MTwoPlus Age if tagOnePlus==1, msymbol(Oh)  msize(small) mcolor(gs8)), legend(lab(1 "`OnePluslabel'") lab(2 "`TwoPluslabel'")) xline(65) ytitle("") yscale(range(0 1)) scheme(s1mono) ylabel(0(0.2)1)
graph export "`path'Figure1.pdf", replace
** By Zs, one dimension, race  (Figure 2)
foreach rhs in OnePlus TwoPlus {
	forvalues m=0(1)1 {
			egen M`rhs'_m`m'=mean(`rhs') if Minority==`m', by(Age)
			egen tag`rhs'_m`m'=tag(Age) if Minority==`m'
			if "`rhs'"=="OnePlus" {
				local `rhs'label="X{sub:1} = One or More Insurance Policies"
			}
			else if "`rhs'"=="TwoPlus" {
				local `rhs'label="X{sub:2} = Two or More Insurance Policies"
			}
	}
}
forvalues m=0(1)1 {
		twoway (scatter MOnePlus_m`m' Age if tagOnePlus_m`m'==1, msymbol(O) msize(small) mcolor(black)) ///
			(scatter MTwoPlus_m`m' Age if tagTwoPlus_m`m'==1, msymbol(Oh)  msize(small) mcolor(gs8)) , ///
			xline(65) ytitle("") yscale(range(0 1)) scheme(s1mono)  legend(lab(1 "`OnePluslabel'") lab(2 "`TwoPluslabel'")) ylabel(0(0.2)1)
		graph export "`path'Figure2_byRace`m'.pdf", replace
}
** By Zs, two dimensions, race and education  (Figure 3)
foreach rhs in OnePlus TwoPlus {
	forvalues m=0(1)1 {
		forvalues g=1(1)3 {
			egen M`rhs'_m`m'g`g'=mean(`rhs') if Minority==`m' & Education==`g', by(Age)
			egen tag`rhs'_m`m'g`g'=tag(Age) if Minority==`m' & Education==`g'
			if "`rhs'"=="OnePlus" {
				local `rhs'label="X{sub:1} = One or More Insurance Policies"
			}
			else if "`rhs'"=="TwoPlus" {
				local `rhs'label="X{sub:2} = Two or More Insurance Policies"
			}
			regress `rhs' Z DZ D `weight' if Minority==`m' & Education==`g', vce(cluster Z)
			local alpha_`rhs'_m`m'_e`g' = _b[D]	
			local SEalpha_`rhs'_m`m'_e`g' = _se[D]					
		}
	}
}
forvalues m=0(1)1 {
	forvalues g=1(1)3 {
		twoway (scatter MOnePlus_m`m'g`g' Age if tagOnePlus_m`m'g`g'==1, msymbol(O) msize(small) mcolor(black)) ///
			(scatter MTwoPlus_m`m'g`g' Age if tagTwoPlus_m`m'g`g'==1, msymbol(Oh)  msize(small) mcolor(gs8)) , ///
			xline(65) ytitle("") yscale(range(0 1)) scheme(s1mono)  legend(lab(1 "`OnePluslabel'") lab(2 "`TwoPluslabel'")) ylabel(0(0.2)1)
		graph export "`path'Figure3_byRace`m'Educ`g'.pdf", replace
	}
}
* Plot of age by education (Figure 4)
twoway (kdensity Age if Education==1, lpattern(solid) lcolor(black) bw(1)) ///
	(kdensity Age if Education==2, lpattern(dash) lcolor(black) bw(1)) ///
	(kdensity Age if Education==3, lpattern(dot) lcolor(black) bw(1)), ///
	legend(label(1 "HS Dropout") label(2 "HS Graduate") label(3 "At Least Some College")) scheme(s1mono) ytitle("Density") xtitle("Age")
graph export "`path'Figure4_Age_byEduc.pdf", replace

* Validity Plots (Figure 5)
gen HSCOL=(HS==1 | COL==1)
foreach rhs in MI HSCOL {
	egen M`rhs'=mean(`rhs'), by(Age)
	egen tag`rhs'=tag(Age)
		if "`rhs'"=="MI" {
			local `rhs'label="Minority Share"
		}
		else if "`rhs'"=="HSCOL" {
			local `rhs'label="HS Graduate or Higher"
		}
	twoway (scatter M`rhs' Age if tag`rhs'==1, msymbol(O) msize(small) mcolor(black)), xline(65) ytitle(``rhs'label') yscale(range(0 1)) scheme(s1mono) legend(off) ylabel(0(0.2)1)
	graph export "`path'Figure5_`rhs'.pdf", replace
	drop M`rhs'* tag*	
}
**************************************************************
* TABLES
**************************************************************
* SUMMARY STATS TABLE (Table 1)
log using "`path'Table1_SummStats.txt", text replace
	// All
	sum DelayCare RationCare NoHosp OnePlus TwoPlus D
	// Non-Hispanic Whites, HS Dropout, HS Grad, >HS Grad
	sum DelayCare RationCare NoHosp OnePlus TwoPlus D if Minority==0 & DRP==1
	sum DelayCare RationCare NoHosp OnePlus TwoPlus D if Minority==0 & HS==1
	sum DelayCare RationCare NoHosp OnePlus TwoPlus D if Minority==0 & COL==1
	// Minorities, HS Dropout, HS Grad, >HS Grad
	sum DelayCare RationCare NoHosp OnePlus TwoPlus D if Minority==1 & DRP==1
	sum DelayCare RationCare NoHosp OnePlus TwoPlus D if Minority==1 & HS==1
	sum DelayCare RationCare NoHosp OnePlus TwoPlus D if Minority==1 & COL==1
log close
* REDUCED FORM TABLE (Table 2)
foreach lhs in DelayCare RationCare WentToHosp {
	if "`lhs'"=="DelayCare" {
		local `lhs'label="Delayed care"
	}
	else if "`lhs'"=="RationCare" {
		local `lhs'label="Rationed care"
	}
	else if "`lhs'"=="WentToHosp" {
		local `lhs'label="Went to hospital"
	}
	egen M`lhs'=mean(`lhs'), by(Age)
	egen tag`lhs'=tag(Age)

	regress `lhs' Z DZ D `weight', vce(cluster Z)
	local rf_`lhs' = _b[D]
	local SErf_`lhs' = _se[D]			
	forvalues m=0(1)1 {
		forvalues g=1(1)3 {
			egen M`lhs'_m`m'g`g'=mean(`lhs') if Minority==`m' & Education==`g', by(Age)
			egen tag`lhs'_m`m'g`g'=tag(Age) if Minority==`m' & Education==`g'
			regress `lhs' Z DZ D `weight' if Minority==`m' & Education==`g', vce(cluster Z)
			local rf_`lhs'_m`m'_e`g' = _b[D]
			local SErf_`lhs'_m`m'_e`g' = _se[D]			
		}
	}
}
* Organize the results into a table
clear
set obs 14
gen type=""
replace type="First" in 1
replace type="Stage" in 2
replace type="1+" in 3
replace type="Policy" in 4
replace type="2+" in 5
replace type="Policies" in 6
replace type="Reduced" in 7
replace type="Form" in 8
replace type="Delayed" in 9
replace type="Care" in 10
replace type="Rationed" in 11
replace type="Care" in 12
replace type="Went to" in 13
replace type="Hospital" in 14

gen All=.
replace All=round(`alpha_OnePlus',0.001) in 3
replace All=round(`SEalpha_OnePlus',0.001) in 4
replace All=round(`alpha_TwoPlus',0.001) in 5
replace All=round(`SEalpha_TwoPlus',0.001) in 6

replace All=round(`rf_DelayCare',0.001) in 9
replace All=round(`SErf_DelayCare',0.001) in 10
replace All=round(`rf_RationCare',0.001) in 11
replace All=round(`SErf_RationCare',0.001) in 12
replace All=round(`rf_WentToHosp',0.001) in 13
replace All=round(`SErf_WentToHosp',0.001) in 14

tostring All, replace force format(%05.3f)
replace  All="("+All+")" in 4
replace  All="("+All+")" in 6
replace  All="("+All+")" in 10
replace  All="("+All+")" in 12
replace  All="("+All+")" in 14
forvalues m=0(1)1 {
	forvalues g=1(1)3 {
		gen m`m'e`g'=.
		replace m`m'e`g'=round(`alpha_OnePlus_m`m'_e`g'',0.001) in 3
		replace m`m'e`g'=round(`SEalpha_OnePlus_m`m'_e`g'',0.001) in 4
		replace m`m'e`g'=round(`alpha_TwoPlus_m`m'_e`g'',0.001) in 5
		replace m`m'e`g'=round(`SEalpha_TwoPlus_m`m'_e`g'',0.001) in 6
		replace m`m'e`g'=round(`rf_DelayCare_m`m'_e`g'',0.001) in 9
		replace m`m'e`g'=round(`SErf_DelayCare_m`m'_e`g'',0.001) in 10
		replace m`m'e`g'=round(`rf_RationCare_m`m'_e`g'',0.001) in 11
		replace m`m'e`g'=round(`SErf_RationCare_m`m'_e`g'',0.001) in 12
		replace m`m'e`g'=round(`rf_WentToHosp_m`m'_e`g'',0.001) in 13
		replace m`m'e`g'=round(`SErf_WentToHosp_m`m'_e`g'',0.001) in 14
		tostring m`m'e`g', replace force format(%05.3f)
		replace m`m'e`g'="("+m`m'e`g'+")" in 4
		replace m`m'e`g'="("+m`m'e`g'+")" in 6
		replace m`m'e`g'="("+m`m'e`g'+")" in 10
		replace m`m'e`g'="("+m`m'e`g'+")" in 12
		replace m`m'e`g'="("+m`m'e`g'+")" in 14	
	}
}
replace All="All" in 1 
replace All="" in 2 
replace All="All" in 7
replace All="" in 8  
replace m0e1="" in 1 
replace m0e2="Whites" in 1 
replace m0e3="" in 1 
replace m0e1="Dropout" in 2 
replace m0e2="HS Grad" in 2 
replace m0e3="HS Grad+" in 2
replace m1e1="" in 1 
replace m1e2="Minorities" in 1 
replace m1e3="" in 1 
replace m1e1="Dropout" in 2 
replace m1e2="HS Grad" in 2 
replace m1e3="HS Grad+" in 2
replace m0e1="" in 7
replace m0e2="Whites" in 7
replace m0e3="" in 7
replace m0e1="Dropout" in 8 
replace m0e2="HS Grad" in 8 
replace m0e3="HS Grad+" in 8
replace m1e1="" in 7 
replace m1e2="Minorities" in 7
replace m1e3="" in 7 
replace m1e1="Dropout" in 8 
replace m1e2="HS Grad" in 8 
replace m1e3="HS Grad+" in 8
texsave type All m0e1 m0e2 m0e3 m1e1 m1e2 m1e3 using "`path'/Table2_MainRDD.tex", replace ///
hlines(0 2 2 4 6 6 8 8 10 12) size(5) align(C|C|CCC|CCC) rowsep(.05in) nonames 

* NAIVE RDD RESULTS TABLE (Table 4)
local estimation="2SLS"
local covariates="HS COL Minority Female R? Y???? Hispanic"
foreach band in `bwlist' {
	local count=0
	foreach dep in `deplist' {
		local count=`count'+1
		use `temp', clear
		
		if "`kernel'"=="uniform" {
			local  weight="if Z>=-`band' & Z<=`band'"  
		}
		else if "`kernel'"=="triangular" {
			local  weight="[w=weight`band']"
		}
		local method="naiveRDD"
		ivregress 2sls `dep' Z DZ (OnePlus = D) `weight', cluster(Z)
		local uLATE1_`count' = _b[OnePlus]
		local se_uLATE1_`count' = _se[OnePlus]
		ivregress 2sls `dep' Z DZ (TwoPlus = D) `weight', cluster(Z)
		local uLATE2_`count' = _b[TwoPlus]
		local se_uLATE2_`count' = _se[TwoPlus]
		ivregress 2sls `dep' Z DZ `covariates' (OnePlus = D) `weight', cluster(Z)
		local covLATE1_`count' = _b[OnePlus]
		local se_covLATE1_`count' = _se[OnePlus]
		ivregress 2sls `dep' Z DZ `covariates' (TwoPlus = D) `weight', cluster(Z)
		local covLATE2_`count' = _b[TwoPlus]
		local se_covLATE2_`count' = _se[TwoPlus]
		
		local method="condRDD"
		ivregress 2sls `dep' Z DZ (OnePlus = D) `weight' & (TwoPlus==0), cluster(Z)
		local CuLATE1_`count' = _b[OnePlus]
		local se_CuLATE1_`count' = _se[OnePlus]
		ivregress 2sls `dep' Z DZ (TwoPlus = D) `weight' & (OnePlus==1), cluster(Z)
		local CuLATE2_`count' = _b[TwoPlus]
		local se_CuLATE2_`count' = _se[TwoPlus]
		ivregress 2sls `dep' Z DZ `covariates' (OnePlus = D) `weight' & (TwoPlus==0), cluster(Z)
		local CcovLATE1_`count' = _b[OnePlus]
		local se_CcovLATE1_`count' = _se[OnePlus]
		ivregress 2sls `dep' Z DZ `covariates' (TwoPlus = D) `weight' & (OnePlus==1), cluster(Z)
		local CcovLATE2_`count' = _b[TwoPlus]
		local se_CcovLATE2_`count'= _se[TwoPlus]
	}
}

clear
set obs 6
gen type=""
replace type="Delayed" in 1
replace type="Care" in 2
replace type="Rationed" in 3
replace type="Care" in 4
replace type="Went to" in 5
replace type="Hospital" in 6
gen OnePlusN=.
replace OnePlusN=round(`uLATE1_1',0.001) in 1
replace OnePlusN=round(`se_uLATE1_1',0.001) in 2
replace OnePlusN=round(`uLATE1_2',0.001) in 3
replace OnePlusN=round(`se_uLATE1_2',0.001) in 4
replace OnePlusN=round(`uLATE1_3',0.001) in 5
replace OnePlusN=round(`se_uLATE1_3',0.001) in 6

gen TwoPlusN=.
replace TwoPlusN=round(`uLATE2_1',0.001) in 1
replace TwoPlusN=round(`se_uLATE2_1',0.001) in 2
replace TwoPlusN=round(`uLATE2_2',0.001) in 3
replace TwoPlusN=round(`se_uLATE2_2',0.001) in 4
replace TwoPlusN=round(`uLATE2_3',0.001) in 5
replace TwoPlusN=round(`se_uLATE2_3',0.001) in 6

gen OnePlusNcov=.
replace OnePlusNcov=round(`covLATE1_1',0.001) in 1
replace OnePlusNcov=round(`se_covLATE1_1',0.001) in 2
replace OnePlusNcov=round(`covLATE1_2',0.001) in 3
replace OnePlusNcov=round(`se_covLATE1_2',0.001) in 4
replace OnePlusNcov=round(`covLATE1_3',0.001) in 5
replace OnePlusNcov=round(`se_covLATE1_3',0.001) in 6

gen TwoPlusNcov=.
replace TwoPlusNcov=round(`covLATE2_1',0.001) in 1
replace TwoPlusNcov=round(`se_covLATE2_1',0.001) in 2
replace TwoPlusNcov=round(`covLATE2_2',0.001) in 3
replace TwoPlusNcov=round(`se_covLATE2_2',0.001) in 4
replace TwoPlusNcov=round(`covLATE2_3',0.001) in 5
replace TwoPlusNcov=round(`se_covLATE2_3',0.001) in 6

gen OnePlusC=.
replace OnePlusC=round(`CuLATE1_1',0.001) in 1
replace OnePlusC=round(`se_CuLATE1_1',0.001) in 2
replace OnePlusC=round(`CuLATE1_2',0.001) in 3
replace OnePlusC=round(`se_CuLATE1_2',0.001) in 4
replace OnePlusC=round(`CuLATE1_3',0.001) in 5
replace OnePlusC=round(`se_CuLATE1_3',0.001) in 6

gen TwoPlusC=.
replace TwoPlusC=round(`CuLATE2_1',0.001) in 1
replace TwoPlusC=round(`se_CuLATE2_1',0.001) in 2
replace TwoPlusC=round(`CuLATE2_2',0.001) in 3
replace TwoPlusC=round(`se_CuLATE2_2',0.001) in 4
replace TwoPlusC=round(`CuLATE2_3',0.001) in 5
replace TwoPlusC=round(`se_CuLATE2_3',0.001) in 6

gen OnePlusCcov=.
replace OnePlusCcov=round(`CcovLATE1_1',0.001) in 1
replace OnePlusCcov=round(`se_CcovLATE1_1',0.001) in 2
replace OnePlusCcov=round(`CcovLATE1_2',0.001) in 3
replace OnePlusCcov=round(`se_CcovLATE1_2',0.001) in 4
replace OnePlusCcov=round(`CcovLATE1_3',0.001) in 5
replace OnePlusCcov=round(`se_CcovLATE1_3',0.001) in 6

gen TwoPlusCcov=.
replace TwoPlusCcov=round(`CcovLATE2_1',0.001) in 1
replace TwoPlusCcov=round(`se_CcovLATE2_1',0.001) in 2
replace TwoPlusCcov=round(`CcovLATE2_2',0.001) in 3
replace TwoPlusCcov=round(`se_CcovLATE2_2',0.001) in 4
replace TwoPlusCcov=round(`CcovLATE2_3',0.001) in 5
replace TwoPlusCcov=round(`se_CcovLATE2_3',0.001) in 6


tostring OnePlusN, replace force format(%05.3f)
tostring TwoPlusN, replace force format(%05.3f)
tostring OnePlusNcov, replace force format(%05.3f)
tostring TwoPlusNcov, replace force format(%05.3f)
tostring OnePlusC, replace force format(%05.3f)
tostring TwoPlusC, replace force format(%05.3f)
tostring OnePlusCcov, replace force format(%05.3f)
tostring TwoPlusCcov, replace force format(%05.3f)


foreach var in OnePlusN TwoPlusN OnePlusNcov TwoPlusNcov OnePlusC TwoPlusC OnePlusCcov TwoPlusCcov {
	replace  `var' = "("+`var'+")" in 2
	replace  `var' = "("+`var'+")" in 4
	replace  `var' = "("+`var'+")" in 6
}

texsave type OnePlusNcov OnePlusCcov TwoPlusNcov TwoPlusCcov using "`path'/Table4_NaiveRDD.tex", replace ///
hlines(0 0 2 4) size(5) align(C|CC|CC) rowsep(.05in) nonames ///
headerlines("  & Strategy \(A_1\) & Strategy \(B_1\) & Strategy \(A_2\) & Strategy \(B_2\) \\ Outcome & \( X_1 \) & \( X_1 \) & \( X_2 \) & \( X_2 \)")


* MAIN RESULTS TABLE (Table 3)
if "`estimation'"=="2SLS" {
	local igmm=""
	local est_method="2sls"
}
else if "`estimation'"=="GMM" {
	local igmm="igmm" //wmatrix(unadjusted) 
	local est_method="gmm"
}
** CCE just RACE
local method="CCE just RACE"
local W2list="Minority"
local covariates="HS COL Female R? Y???? Hispanic"
local nrownew=`nrow'/7
matrix results = J(`nrownew',10,.)
foreach dep in `deplist' {
	foreach rhs in "OnePlus TwoPlus" {
		local count=0
		foreach band in `bwlist' {
			use `temp', clear
			if "`kernel'"=="uniform" {
				local  weight="if Z>=-`band' & Z<=`band'"  
			}
			else if "`kernel'"=="triangular" {
				local  weight="[w=weight`band']"
			}
			foreach var in `W2list' {
				gen W_`var'=`var'
				gen DW_`var'=D*`var'
				gen ZW_`var'=Z*`var'				
				gen DZW_`var'=DZ*`var'				
			}
			local count=`count'+1
			mat results[`count',1] = `band'
			mat results[`count',2] = 0
			ivreg2 `dep' Z DZ W_* ZW_* DZW_* `covariates' (`rhs' = D DW_*) `weight', ffirst cluster(Z)
			mat results[`count',3] = _b[OnePlus]
			mat results[`count',4] = _se[OnePlus]
			mat results[`count',5] = _b[OnePlus]/_se[OnePlus]
			mat results[`count',6] = _b[TwoPlus]
			mat results[`count',7] = _se[TwoPlus]
			mat results[`count',8] = _b[TwoPlus]/_se[TwoPlus]				
			mat results[`count',9] = e(N)
			mat results[`count',10] = e(jp) // just identified	
		}
	clear
	matrix colnames results = Bandwidth Group Beta1 SE1 t1 Beta2 SE2 t2 N pOVERID
	svmat results, names(col)
	tempfile temp1_`dep'_`rhs'
	save `temp1_`dep'_`rhs'', replace	
	}
}
** CCE with race and education
local method="CCE"
local covariates="Female R? Y???? Hispanic"
local nrownew=`nrow'/7
matrix results = J(`nrownew',10,.)
foreach dep in `deplist' {
	foreach rhs in "OnePlus TwoPlus" {
		local count=0
		foreach band in `bwlist' {
			use `temp', clear
			if "`kernel'"=="uniform" {
				local  weight="if Z>=-`band' & Z<=`band'"  
			}
			else if "`kernel'"=="triangular" {
				local  weight="[w=weight`band']"
			}
			local count=`count'+1
			mat results[`count',1] = `band'
			mat results[`count',2] = 0
			ivreg2 `dep' Z DZ W? ZW? DZW? `covariates' (`rhs' = D DW?) `weight', ffirst cluster(Z)
			mat results[`count',3] = _b[OnePlus]
			mat results[`count',4] = _se[OnePlus]
			mat results[`count',5] = _b[OnePlus]/_se[OnePlus]
			mat results[`count',6] = _b[TwoPlus]
			mat results[`count',7] = _se[TwoPlus]
			mat results[`count',8] = _b[TwoPlus]/_se[TwoPlus]				
			mat results[`count',9] = e(N)
			mat results[`count',10] = e(jp)		
		}
	clear
	matrix colnames results = Bandwidth Group Beta1 SE1 t1 Beta2 SE2 t2 N pOVERID
	svmat results, names(col)
	tempfile temp2_`dep'_`rhs'
	save `temp2_`dep'_`rhs'', replace	
	}
}
* CCE method augmented with more Ws
local method="CCE Augmented"
local otherWlist="Female R? Y???? Hispanic"
local covariates=""
local nrownew=`nrow'/7
matrix results = J(`nrownew',10,.)
foreach dep in `deplist' {
	foreach rhs in "OnePlus TwoPlus" {
		local count=0
		foreach band in `bwlist' {
			use `temp', clear
			foreach var of varlist `otherWlist' {
				gen W_`var'=`var'
				gen DW_`var'=D*`var'
				gen ZW_`var'=Z*`var'
				gen DZW_`var'=DZ*`var'
			}
			if "`kernel'"=="uniform" {
				local  weight="if Z>=-`band' & Z<=`band'"  
			}
			else if "`kernel'"=="triangular" {
				local  weight="[w=weight`band']"
			}
			local count=`count'+1
			mat results[`count',1] = `band'
			mat results[`count',2] = 0
			ivreg2 `dep' Z DZ W? ZW? DZW? W_* ZW_* DZW_* `covariates' (`rhs' = D DW? DW_*) `weight', ffirst cluster(Z)
			mat results[`count',3] = _b[OnePlus]
			mat results[`count',4] = _se[OnePlus]
			mat results[`count',5] = _b[OnePlus]/_se[OnePlus]
			mat results[`count',6] = _b[TwoPlus]
			mat results[`count',7] = _se[TwoPlus]
			mat results[`count',8] = _b[TwoPlus]/_se[TwoPlus]				
			mat results[`count',9] = e(N)
			mat results[`count',10] = e(jp)		
		}
	clear
	matrix colnames results = Bandwidth Group Beta1 SE1 t1 Beta2 SE2 t2 N pOVERID
	svmat results, names(col)
	tempfile temp3_`dep'_`rhs'
	save `temp3_`dep'_`rhs'', replace	

	}
}
** Organize the results into a table
display "`estimation'"
local count=0
local ct=0
local rhs="OnePlus TwoPlus"
foreach method in "CCE just Race" "CCE" "CCE Augmented" {
 local ct=`ct'+1
 forvalues c=1(1)2 {
	 local count=`count'+1
	 local cnt=0
	 foreach dep in `deplist' {
		foreach bw in `bwlist' {
			local cnt=`cnt'+1
				use `temp`ct'_`dep'_`rhs'', clear
				keep if Bandwidth==`bw'
				sum N
				local N=r(mean)
				keep Beta`c' SE`c' t`c' pOVERID
				replace Beta`c' = round(Beta`c',0.001)
				replace SE`c' = round(SE`c',0.001)
				replace pOVERID = round(pOVERID,0.001)
				xpose, clear varname	
				if abs(v1[3])>=1.96 {
					 tostring v1, replace force format(%05.3f)
					 replace v1=v1+"**" in 1
					 replace v1="("+v1+")   " in 2
					 replace v1="" if v1=="." in 4
					 if `c' == 1 {
						replace v1="\multicolumn{2}{c}{[J-test p-value = "+v1+"]}" in 4 if v1~=""						 						 
					 }
					 else {
						replace v1="" in 4						 					 
					 }
				}
				else if abs(v1[3])>=1.645 {
					 tostring v1, replace force format(%05.3f)
					 replace v1=v1+"*" in 1
					 replace v1="("+v1+")   " in 2
					 replace v1="" if v1=="." in 4					 
					 if `c' == 1 {
						replace v1="\multicolumn{2}{c}{[J-test p-value = "+v1+"]}" in 4 if v1~=""						 						 
					 }
					 else {
						replace v1="" in 4						 					 
					 }
				}
				else {
					 tostring v1, replace force format(%05.3f)
					 replace v1=v1+"" in 1
					 replace v1="("+v1+")   " in 2	
					 replace v1="" if v1=="." in 4					 
					 if `c' == 1 {
						replace v1="\multicolumn{2}{c}{[J-test p-value = "+v1+"]}" in 4 if v1~=""						 						 
					 }
					 else {
						replace v1="" in 4						 					 
					 }
				}
				gen bw=""
				replace bw="`bw'" in 2
				gen N=""
				local N1=substr("`N'",1,2) in 2
				local N2=substr("`N'",3,3) in 2
				local N="`N1',`N2'" in 2
				replace N="`N'" in 2
				gen dep=""
				if "`dep'"=="DelayCare" {
						replace dep="Delayed Care" in 2
				}
				else if "`dep'"=="RationCare" {
						replace dep="Rationed Care" in 2
				}
				else if "`dep'"=="WentToHosp" {
						replace dep="Went to Hospital" in 2
				}
			order dep bw N
			rename v1 beta`ct'`c'
			drop _varname
			drop if _n==3
			if `cnt'==1 {
				tempfile temp`ct'`c' temp
				save `temp`ct'`c'', replace
			}
			else {
				save `temp', replace
				use `temp`ct'`c'', clear
				append using `temp'
				save `temp`ct'`c'', replace		
			}
		}
	}
 }
}
use `temp11', clear
merge 1:1 _n using `temp12', nogenerate
merge 1:1 _n using `temp21', nogenerate
merge 1:1 _n using `temp22', nogenerate
merge 1:1 _n using `temp31', nogenerate
merge 1:1 _n using `temp32', nogenerate

texsave dep beta11 beta12 beta21 beta22 beta31 beta32 using "`path'/Table3_Main_`estimation'_`kernel'.tex", replace ///
hlines(3 6) size(5) align(C|CC|CC|CC) rowsep(.05in) nonames nofix ///
headerlines(" & \multicolumn{2}{c}{(I)} & \multicolumn{2}{c}{(II)} & \multicolumn{2}{c}{(III)} \\ & \multicolumn{2}{c}{$ W=$ Race} & \multicolumn{2}{c}{$ W=$ Race \& Educ} & \multicolumn{2}{c}{$ W=$ Race, Educ \& Other} \\ Outcome & $\hat{\beta}_{1}$ & $\hat{\beta}_{2}$ & $\hat{\beta}_{1}$ & $\hat{\beta}_{2}$ & $\hat{\beta}_{1}$ & $\hat{\beta}_{2}$") ///
// frag
