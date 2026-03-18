********************************************************************************
**																			  **
** Name: Travis Swiger												  		  **
** Date: 2/21/2026															  **
** Assignment: Project 1													  **
**																			  **
********************************************************************************

// 0. Setup assignment environment, import and explore data
cd "C:\Users\TravisSwiger\Documents\homework\Data Mgmt\stata_files"
use "data/ansur2allV2.dta", clear

codebook
describe
summarize, detail

********************************************************************************
// Section 1: Clean Data
********************************************************************************

// A. Pre-defined tasks
// -77 = .a = not recorded
// -88 = .b = refused measurement
// -99 = .c = unknown missing
// Remove participants under 18
ds, has(type numeric)
foreach var of varlist `r(varlist)' {
    recode `var' ///
        (-77 = .a) ///
        (-88 = .b) ///
        (-99 = .c)
}

drop if age < 18 & age < .


********************************************************************************
// B. Look for anomalous values
* Separate male and female as we should expect the ranges to be different between the two groups
ds, has(type numeric)
local numvars `r(varlist)'
tabstat `numvars', ///
    by(gender) ///
    statistics(min max p10 p25 p50 p75 p90) ///
    columns(statistics)

// Findings //
* weightkg: Max is over 800kg, setting highest plausible as 200kg
* heighin: Max is 94, setting max to 84in (7ft) as highest plausible
* subjectnumericrace: Flag values > 1000
* dodrace: replace missing with '.d'
* missing strings: define as ""
* handedness: Fix spelling
* weightlbs: Replace with missing ".d" if = 0
* thumbtipreach: Flagged values >= 6000 as data entry errors
* All other values appear reasonable

********************************************************************************
// D. Clean anomalous values

* For newly identified missing values:
* .d = numeric "invalid/should be missing" (e.g., weightlbs==0)
* .e = string "blank/empty response"

// Clean up numeric values
replace weightlbs = .d if weightlbs == 0 // weightlbs: 0 is impossible -> .d
replace weightkg = .d if weightkg >= 200 // Max weight > 800kg, -> .d
replace dodrace = .d  if dodrace == 7 // Not a valid response according to documentation
replace heightin = .d if heightin > 84
replace thumbtipreach = .d if thumbtipreach >= 6000 & thumbtipreach < . // thumbtipreach: clearly impossible values, likely data-entry -> set to .d

// subjectnumericrace: suspected mis-coded / not race; do not recode automatically, flag for review
capture confirm variable subjectnumericrace
if _rc==0 {
    gen flag_subjectnumericrace_hi = (subjectnumericrace > 1000 & subjectnumericrace < .)
    label var flag_subjectnumericrace_hi "subjectnumericrace unusually high (review)"
}

// Clean up blank strings
foreach v in ethnicity component branch writingpreference installation {
    capture confirm variable `v'
    if _rc==0 {
        replace `v' = strtrim(`v')
        replace `v' = "" if `v' == ""   // ensures whitespace-only becomes ""
        
        * create a companion missing-indicator coded as .e for blank responses
        gen `v'_miss = .
        replace `v'_miss = .e if `v' == ""
        label var `v'_miss "`v': blank string response (.e)"
    }
}

// writingpreference spelling fix: "Either han" -> "Either hand (No preference)"
capture confirm variable writingpreference
if _rc==0 {
    replace writingpreference = strtrim(writingpreference)
    replace writingpreference = "Either hand (No preference)" ///
        if writingpreference == "Either han"
}

********************************************************************************

// E. Identify Primary Key
* No subject ID included
* Need to use stable variables 

* Always-unique row key
gen long pk_row = _n
label var pk_row "Primary key: unique row id in this dataset"

* Candidate subject key using stable variables
* ethnicity excluded due to number of missing values
* age included as it is stable for subject-date of measurement granularity

local stable gender dodrace stature footlength heightin
egen long subject_key = group(`stable'), missing
label var subject_key "Proxy subject id from stable fields"

********************************************************************************
// F. Handle duplicates

* Expected unique at subject-date level
duplicates tag subject_key test_date, gen(dup_subjdate)
label var dup_subjdate "Duplicate count tag: subject_key x test_date"
gen byte flag_possible_dup = (dup_subjdate>0)
label var flag_possible_dup "Possible duplicate (subject_key x test_date)"

* Define the measurement set you care about for "exact duplicates"
local meas weightlbs heightin thumbtipreach span footlength kneeheightmidpatella ///
           waistheightomphalion functionalleglength cervicaleheight ///
           trochanterionheight stature waistcircumference chestcircumference ///
           bicristalbreadth hipbreadth hipbreadthsitting weightkg

* Tag exact duplicates (all meas identical within subject_key+test_date)
duplicates tag subject_key test_date `meas', gen(dup_exact)
label var dup_exact "Exact duplicate on subject_key+test_date+measures"

* Count missing among key measures (lower is better)
egen byte nmiss_meas = rowmiss(`meas')
label var nmiss_meas "Number of missing values across key measures"

* Sort within subject-date by completeness, then by pk_row for determinism
bysort subject_key test_date (nmiss_meas pk_row): gen byte keep_best = (_n==1)
label var keep_best "Keep-best record within subject_key x test_date"

* Drop exact duplicates
preserve
    keep if dup_exact>0 & keep_best==0
    save "proj1_duplicates_exact.dta", replace
restore
drop if dup_exact>0 & keep_best==0

* Drop the removed duplicates from main analytical dataset
drop if flag_possible_dup==1 & keep_best==0

display "Total obs: " _N
count if flag_possible_dup==1
display "Obs in duplicated subject-date sets: " r(N)

count if dup_exact>0
display "Obs that are exact duplicates: " r(N)

* After dropping, confirm uniqueness at intended grain
isid subject_key test_date


********************************************************************************
// G. Label all variables and values of categorical variables

// Demographics and Administrative Variables

label var subjectnumericrace   "Subject numeric race code (original data field)"
label var dodrace              "Department of Defense race category"
label var ethnicity            "Self-reported ethnicity"
label var gender               "Gender"
label var age                  "Age at time of measurement (years)"
label var component            "Military component (e.g., Regular Army, Reserve)"
label var branch               "Military branch or occupational branch"
label var writingpreference    "Handedness / writing preference"
label var installation         "Installation where measurements were taken"
label var test_date            "Measurement date (string as recorded)"
label var date                 "Measurement date (Stata internal date)"
label var strdate              "Measurement date (cleaned string format)"

// Self-Reported Anthropometrics

label var weightlbs            "Self-reported weight (pounds)"
label var heightin             "Self-reported height (inches)"

// Measured Anthropometrics (recorded in millimeters unless noted)

label var thumbtipreach        "Forward arm reach to thumb tip (mm)"
label var span                 "Arm span (mm)"
label var footlength           "Foot length (mm)"
label var kneeheightmidpatella "Knee height at mid-patella (mm)"
label var waistheightomphalion "Waist height at omphalion (mm)"
label var functionalleglength  "Functional leg length (mm)"
label var cervicaleheight      "Cervicale height (mm)"
label var trochanterionheight  "Trochanterion height (mm)"
label var stature              "Measured stature / standing height (mm)"

label var waistcircumference   "Waist circumference (mm)"
label var chestcircumference   "Chest circumference (mm)"
label var bicristalbreadth     "Bicristal (pelvic) breadth (mm)"
label var hipbreadth           "Hip breadth (mm)"
label var hipbreadthsitting    "Hip breadth, sitting (mm)"

label var weightkg             "Measured weight (kg × 10; divide by 10 for kg)"

// Data Quality Flags and Derived Variables

label var flag_subjectnumericrace_hi ///
    "Flag: subjectnumericrace unusually high (review)"

label var ethnicity_miss ///
    "Missing indicator: ethnicity blank string (.e)"

label var component_miss ///
    "Missing indicator: component blank string (.e)"

label var branch_miss ///
    "Missing indicator: branch blank string (.e)"

label var writingpreference_miss ///
    "Missing indicator: writing preference blank string (.e)"

label var installation_miss ///
    "Missing indicator: installation blank string (.e)"

// Primary Key and Duplicate Management

label var pk_row        "Primary key: unique row identifier"
label var subject_key   "Proxy subject identifier (derived fingerprint key)"
label var dup_subjdate  "Duplicate tag: subject_key × test_date"
label var flag_possible_dup ///
    "Flag: possible duplicate at subject-date level"

label var dup_exact     ///
    "Exact duplicate across subject-date and measurement fields"

label var nmiss_meas    ///
    "Count of missing values across key measurement variables"

label var keep_best     ///
    "Indicator: retained best record within duplicate set"

**************** Label Categorical Values ****************

// Missing, Special, or Null Values

label define miss_ext ///
    .a "Not recorded (-77)" ///
    .b "Refused (-88)" ///
    .c "Unknown missing (-99)" ///
    .d "Invalid/implausible value (recoded)" ///
    .e "Blank string response (string missing indicator)", modify

ds, has(type numeric)
label values `r(varlist)' miss_ext
// dodrace
label define dodrace_lbl ///
    1 "White" ///
    2 "Black" ///
    3 "Hispanic" ///
    4 "Asian" ///
    5 "Native American" ///
    6 "Pacific Islander" ///
    8 "Other" ///
    .a "Not recorded (-77)" ///
    .b "Refused (-88)" ///
    .c "Unknown missing (-99)" ///
    .d "Invalid/implausible value (recoded)" ///
    .e "Blank string response (string missing indicator)", replace

label values dodrace dodrace_lbl
label var dodrace "Department of Defense race category"

**************** Encode Categorical Values ****************
// handedness
encode writingpreference, gen(handedness_cat)
label var handedness_cat "Writing preference / handedness (categorical)"

// gender
encode gender, gen(gender_cat)
label var gender_cat "Gender (categorical)"
label define gender_lbl 1 "Female" 2 "Male", replace
label values gender_cat gender_lbl

********************************************************************************
// H. Convert the measurements to cm from mm
local mmvars thumbtipreach span footlength kneeheightmidpatella ///
             waistheightomphalion functionalleglength ///
             cervicaleheight trochanterionheight stature ///
             waistcircumference chestcircumference ///
             bicristalbreadth hipbreadth hipbreadthsitting

foreach v of local mmvars {
    replace `v' = `v'/10 if `v' < .
}

********************************************************************************
// I. Create New Variables
// Continuous BMI

gen bmi = .
replace bmi = (weightkg) / ((stature/100)^2) ///
    if weightkg < . & stature < .

label var bmi "Body Mass Index (kg/m^2)"
format bmi %9.2f

// Categorical BMI
gen byte bmi_cat = .
replace bmi_cat = 1 if bmi < 18.5 & bmi < .
replace bmi_cat = 2 if bmi >= 18.5 & bmi < 25
replace bmi_cat = 3 if bmi >= 25 & bmi < 30
replace bmi_cat = 4 if bmi >= 30 & bmi < .

label define bmi_lbl ///
    1 "Underweight (<18.5)" ///
    2 "Normal weight (18.5-24.9)" ///
    3 "Overweight (25.0-29.9)" ///
    4 "Obese (>=30)", replace

label values bmi_cat bmi_lbl
label var bmi_cat "BMI category (standard clinical cutoffs)"

// Season Obs. Recorded

gen month_obs = month(date)

gen byte season = .
replace season = 1 if inlist(month_obs, 12,1,2)
replace season = 2 if inlist(month_obs, 3,4,5)
replace season = 3 if inlist(month_obs, 6,7,8)
replace season = 4 if inlist(month_obs, 9,10,11)

label define season_lbl ///
    1 "Winter" ///
    2 "Spring" ///
    3 "Summer" ///
    4 "Fall", replace

label values season season_lbl
label var season "Season measurement recorded"

// Body Types
/********************************************************************
 Section 2e: Body type classification (sex-specific)
 - Inputs: stature (cm), bmi, waistcircumference (cm)
 - Derived: WHtR (waist-to-height ratio), sex-specific tertiles
 - Output: body_type (1-5) with value labels
********************************************************************/

*------------------------------------------------------------*
* 1) Waist-to-height ratio (central adiposity proxy)         *
*------------------------------------------------------------*
gen whtr = .
replace whtr = waistcircumference / stature if waistcircumference < . & stature < .
label var whtr "Waist-to-height ratio (waist cm / height cm)"
format whtr %6.3f

* Common clinical threshold: WHtR >= 0.50
gen byte central_adip = .
replace central_adip = 1 if whtr >= 0.50 & whtr < .
replace central_adip = 0 if whtr < 0.50
label define central_lbl 0 "No central adiposity (WHtR<0.50)" 1 "Central adiposity (WHtR>=0.50)", replace
label values central_adip central_lbl
label var central_adip "Central adiposity indicator (WHtR>=0.50)"

*------------------------------------------------------------*
* 2) Sex-specific tertiles for stature and BMI                *
*------------------------------------------------------------*
* 1=Low (short/lean), 2=Mid, 3=High (tall/heavy)
egen ht_tertile=xtile(stature), nq(3) by(gender)
egen bmi_tertile=xtile(bmi)if bmi < ., nq(3) by(gender)

label define tert_lbl 1 "Low" 2 "Mid" 3 "High", replace
label values ht_tertile tert_lbl
label values bmi_tertile tert_lbl

label var ht_tertile  "Stature tertile within gender"
label var bmi_tertile "BMI tertile within gender"

*------------------------------------------------------------*
* 3) Map to 5 body types                                      *
*------------------------------------------------------------*
gen byte body_type = .
label var body_type "Body type (gender-specific height/BMI + central adiposity)"

* Core corner types
replace body_type = 1 if ht_tertile==1 & bmi_tertile==1
replace body_type = 2 if ht_tertile==1 & (bmi_tertile==3 | central_adip==1)
replace body_type = 4 if ht_tertile==3 & bmi_tertile==1
replace body_type = 5 if ht_tertile==3 & (bmi_tertile==3 | central_adip==1)

* Everyone else defaults to Average
replace body_type = 3 if missing(body_type) & ht_tertile < . & bmi_tertile < .

label define bodytype_lbl ///
    1 "Small-Lean" ///
    2 "Small-Heavy" ///
    3 "Average" ///
    4 "Tall-Lean" ///
    5 "Tall-Heavy", replace

label values body_type bodytype_lbl

********************************************************************************
// Section 2: Anthropometric Characteristics
********************************************************************************

************************ Summary Table for Entire Sample ***********************
// Summarize sample by body part
* Stats: n_obs, n_missing (%), n_suspicious (%), mean, min, q1, median, q3, max


// Define anthropometric variable groups
* Trunk circumference
local trunk hipbreadthsitting ///
		waistcircumference chestcircumference ///
        bicristalbreadth hipbreadth


* Stature-related measures
local stature_grp span ///
		stature kneeheightmidpatella /// 
        cervicaleheight trochanterionheight ///
        waistheightomphalion functionalleglength ///
        footlength thumbtipreach


// Summary statistics table
dtable `trunk' `stature_grp', ///
    continuous(`trunk' `stature_grp', ///
    stat(n mean sd median min max)) ///
    title("Table 3.1-1. Anthropometric Measures (cm)") ///
	column(summary( N Mean (SD) Median Min Max))

	

// Output to word
collect style putdocx, layout(autofitcontents)
collect export extable2.docx, replace

************************ Hip-Height / Total Height Pct *************************
// Create hip height pct variable
gen pct_hip_height = .
replace pct_hip_height = (trochanterionheight / stature) * 100 ///
    if trochanterionheight < . & stature < .

label var pct_hip_height "% of total height attributable to hip height"
format pct_hip_height %6.2f

// Tabulate by sex
asdoc tabstat pct_hip_height, by(gender) stat(mean sd p25 p50 p75) save(pct_hip_height_tbl.doc)

// Scatter plot by sex
twoway (kdensity pct_hip_height if gender=="Female") ///
       (kdensity pct_hip_height if gender=="Male"), ///
       legend(label(1 "Female") label(2 "Male")) ///
       title("Fig 3.1 Distribution of Hip Height as % of Total Height") ///
       ytitle("Density") xtitle("Percent of Total Height")
	  
********************************************************************************
// 3. Data Quality Assessment
********************************************************************************
********************************************************************************
* 3A. Missingness table (numeric variables only; string missings excluded)
* Output columns (as strings for clean Word export):
*   Total Non-Missing (%), Total Missing (%), .a (%), .b (%), .c (%), .d (%), .e (%)
********************************************************************************

* Identify numeric variables (these can contain .a–.e)
ds, has(type numeric)
local numvars `r(varlist)'

* Total observations in current analytic dataset
quietly count
local N = r(N)

* Build a small results dataset using postfile
tempfile miss_tbl
tempname posth

postfile `posth' ///
    str32 varname ///
    str20 nonmiss str20 miss ///
    str20 miss_a str20 miss_b str20 miss_c str20 miss_d str20 miss_e ///
    using `miss_tbl', replace

foreach v of varlist `numvars' {

    * Counts
    quietly count if !missing(`v')
    local n_non = r(N)

    quietly count if missing(`v')
    local n_mis = r(N)

    quietly count if `v' == .a
    local n_a = r(N)

    quietly count if `v' == .b
    local n_b = r(N)

    quietly count if `v' == .c
    local n_c = r(N)

    quietly count if `v' == .d
    local n_d = r(N)

    quietly count if `v' == .e
    local n_e = r(N)

    * Percents of total N
    local p_non = 100*`n_non'/`N'
    local p_mis = 100*`n_mis'/`N'
    local p_a   = 100*`n_a'/`N'
    local p_b   = 100*`n_b'/`N'
    local p_c   = 100*`n_c'/`N'
    local p_d   = 100*`n_d'/`N'
    local p_e   = 100*`n_e'/`N'

    * Post formatted cells: "count (pct%)"
    post `posth' ///
        ("`v'") ///
        (string(`n_non',"%9.0f")+" ("+string(`p_non',"%4.1f")+"%)") ///
        (string(`n_mis',"%9.0f")+" ("+string(`p_mis',"%4.1f")+"%)") ///
        (string(`n_a',"%9.0f")+" ("+string(`p_a',  "%4.1f")+"%)") ///
        (string(`n_b',"%9.0f")+" ("+string(`p_b',  "%4.1f")+"%)") ///
        (string(`n_c',"%9.0f")+" ("+string(`p_c',  "%4.1f")+"%)") ///
        (string(`n_d',"%9.0f")+" ("+string(`p_d',  "%4.1f")+"%)") ///
        (string(`n_e',"%9.0f")+" ("+string(`p_e',  "%4.1f")+"%)")
}

postclose `posth'

* Export to Word
preserve
use `miss_tbl', clear

putdocx begin
putdocx paragraph, halign(left)
putdocx text ("Table 3 Missingness by variable")

putdocx table t_miss = data("varname nonmiss miss miss_a miss_b miss_c miss_d miss_e"), varnames
putdocx save "missingness_table.docx", replace
restore

********************************************************************************
* 3B. Flag tallies table
* Output columns:
*   flag_name | sum(flag==1) (% of total N)
********************************************************************************

* Total observations in current analytic dataset
quietly count
local N = r(N)

* Identify flag variables (edit patterns if needed)
ds flag_* dup_* keep_best flag_possible*, has(type numeric)
local flagvars `r(varlist)'

tempfile flag_tbl
tempname postf

postfile `postf' ///
    str32 flag_name ///
    str20 flagged ///
    using `flag_tbl', replace

foreach f of varlist `flagvars' {

    * Count flagged (assumes flags are 0/1; treats missing as not-flagged)
    quietly count if `f' == 1
    local n_flag = r(N)
    local p_flag = 100*`n_flag'/`N'

    post `postf' ///
        ("`f'") ///
        (string(`n_flag',"%9.0f")+" ("+string(`p_flag',"%4.1f")+"%)")
}

postclose `postf'

* Export to Word
preserve
use `flag_tbl', clear

putdocx begin
putdocx paragraph, halign(left)
putdocx text ("Table 3.Y. Data-quality flags (N = `N')")

putdocx table t_flags = data("flag_name flagged"), varnames
putdocx save "flag_tallies.docx", replace
restore
********************************************************************************
// 4. Analysis
********************************************************************************

// 4.1. correlation between  measures of stature 

* Stature-related measures
local stature_grp span ///
		stature kneeheightmidpatella /// 
        cervicaleheight trochanterionheight ///
        waistheightomphalion functionalleglength ///
        footlength thumbtipreach

* Overall Correlations
pwcorr `stature_grp', sig
matrix R = r(C)
matrix P = r(sig)

putexcel set stature_correlations.xlsx, replace
putexcel A1 = matrix(R), names

twoway (scatter stature cervicaleheight, mcolor(navy%40) msize(vsmall)) ///
       (lfit stature cervicaleheight, lcolor(red) lwidth(medium)), ///
       title("Fig 4.1 Relationship Between Stature and Cervicale Height") ///
       xtitle("Cervicale Height (cm)") ///
       ytitle("Stature (cm)") ///
       legend(order(2 "Linear Best Fit")) ///
       graphregion(color(white))
	   
* Correlations Startified by Sex

local stature_grp span stature kneeheightmidpatella cervicaleheight ///
                   trochanterionheight waistheightomphalion functionalleglength ///
                   footlength thumbtipreach

bysort gender: correlate `stature_grp'

* Scatter plot w/ linear lobf by gender
twoway (scatter stature cervicaleheight if gender == "Male", mcolor(ebblue%30) msize(tiny))    ///
       (scatter stature cervicaleheight if gender == "Female", mcolor(red%30) msize(tiny))     ///
       (lfit stature cervicaleheight if gender == "Male", lcolor(dknavy) lpattern(solid) lwidth(medium))       ///
       (lfit stature cervicaleheight if gender == "Female", lcolor(magenta) lpattern(solid) lwidth(medium)),       ///
       title("Fig 4.2 Stature vs. Cervicale Height by Gender")                                  ///
       xtitle("Cervicale Height (cm)")                                                  ///
       ytitle("Stature (cm)")                                                           ///
       legend(order(3 "Male Fit" 1 "Male Data" 4 "Female Fit" 2 "Female Data"))         ///
       graphregion(color(white))

* 2. Summary stats by Gender
asdoc tabstat stature cervicaleheight, by(gender) stat(mean min p25 median p75 max) save(Summary_Table.doc)

// 4.2 Self-Reported vs. Measured 

* Measure differences; positive => over-reporting
gen weight_self = weightlbs*0.45359
gen height_self = heightin*2.54
gen wt_diff = weight_self - weightkg
gen ht_diff = height_self - stature

* Describe distribution of the difference
summarize wt_diff ht_diff, detail

// A. Figure showing overlayed densities of self-reported vs measured weights

twoway ///
    (kdensity weightkg, lpattern(solid)) ///
    (kdensity weight_self, lpattern(dash)), ///
    title("4.3 Measured vs Self-Reported Weight") ///
    xtitle("Weight (kg)") ///
    ytitle("Density") ///
    legend(order(1 "Measured" 2 "Self-reported"))

// B. Figure showing difference in reported vs measured weight by sex
by gender, sort: summarize wt_diff, detail

vioplot wt_diff, over(gender) ///
	title("Fig 4.4 Difference in Reported and Measured Weight by Gender") ///
	ytitle("Weight Difference (kg)")


// C. Figure showing difference in reported vs. measured height
twoway ///
    (kdensity stature, lpattern(solid)) ///
    (kdensity height_self, lpattern(dash)), ///
    title("Fig 4.5 Measured vs Self-Reported Height") ///
    xtitle("Height (cm)") ///
    ytitle("Density") ///
    legend(order(1 "Measured" 2 "Self-reported"))
	
// 4.3. Relationship between derived `body type' and weight/BMI
graph box bmi, over(body_type, label(angle(45))) ///
    title("Fig 4.6 BMI by Body Type") ytitle("BMI")
