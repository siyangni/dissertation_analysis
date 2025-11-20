***10/11/2025****
*** Merging ***
clear

// Go to the working directory
cd "C:\Users\siyan\OneDrive - The Pennsylvania State University\dissertation_working\data\"

// start with wave 1 parent interview 
use "w1\mcs1_parent_interview.dta"

// + wave 1 derived variables
merge 1:1 mcsid using "w1\mcs1_derived_variables.dta"
drop _merge
save "temp/t1"

// + wave 2 child assess
use "w2\mcs2_child_assessment_data.dta", clear
drop if bhcnum00!=1
merge 1:1 mcsid using "temp\t1.dta"
drop _merge

// + wave 2 parent interview
merge 1:1 mcsid using "w2\mcs2_parent_interview.dta"
drop _merge
save "temp\t2"

// + wave 2 child measurement
use "w2\mcs2_child_measurement_data.dta"
drop if bhcnum00!=1
merge 1:1 mcsid using "temp\t2"
drop _merge

// + wave 2 derived variables
merge 1:1 mcsid using "w2\mcs2_derived_variables.dta"
drop _merge
save "temp\t2a"

// + wave 3 child assess
use "w3\mcs3_child_assessment_data.dta", clear
drop if chcnum00!=1
merge 1:1 mcsid using "temp\t2a"
drop _merge

// + wave 3 parent interview
merge 1:1 mcsid using "w3\mcs3_parent_interview.dta"
drop _merge

// + wave 3 derived variables
merge 1:1 mcsid using "w3\mcs3_derived_variables.dta"
drop _merge

// + wave 4 parent interview
merge 1:1 mcsid using "w4\mcs4_parent_interview.dta"
drop _merge
save "temp\t4"

// + wave 4 child assess
use "w4\mcs4_cm_assessment.dta", clear
rename *, lower
drop if dccnum00!=1
merge 1:1 mcsid using "temp\t4"
drop _merge
save "temp\t4a"

// + wave 4 cohort member self completion
use "w4\mcs4_cm_self_completion_final.dta", clear
drop if dccnum00!=1
merge 1:1 mcsid using "temp\t4a"
drop _merge

// + wave 4 derived variables
merge 1:1 mcsid using "w4\mcs4_derived_variables.dta"
drop _merge
save "temp\t4b"

// + wave 4 measurement
use "w4\mcs4_measurement_final.dta"
drop if dccnum00!=1
merge 1:1 mcsid using "temp\t4b"
drop _merge
save "temp\t4c"

// + wave 5 child cognitive assess
use "w5\mcs5_cm_cognitive_assessment.dta", clear
rename *, lower
drop if ecnum00!=1
merge 1:1 mcsid using "temp\t4c"
drop _merge
save "temp\t5"

// + wave 5 child interview
use "w5\mcs5_cm_interview", clear
rename *, lower
drop if ecnum00!=1
merge 1:1 mcsid using "temp\t5"
drop _merge
save "temp\t5a"

// + wave 5 parent_cm interview (main respondent)
use "w5\mcs5_parent_cm_interview", clear
rename *, lower
drop if ecnum00!=1
drop if epnum00!=1
merge 1:1 mcsid using "temp\t5a"
drop _merge
save "temp\t5b"

// + wave 5 cohort member derived
use "w5\mcs5_cm_derived", clear
rename *, lower
drop if ecnum00!=1
merge 1:1 mcsid using "temp\t5b"
drop _merge
save "temp\t5c"

// + wave 5 family derived
use "w5\mcs5_family_derived", clear
rename *, lower
merge 1:1 mcsid using "temp\t5c" 
drop _merge
save "temp\t5d"

// + wave 5 parent derived
use "w5\mcs5_parent_derived", clear
rename *, lower
drop if epnum00!=1
merge 1:1 mcsid using "temp\t5d"
drop _merge
save "temp\t5e"

// + wave 5 parent interview
use "w5\mcs5_parent_interview", clear
rename *, lower
drop if epnum00!=1
merge 1:1 mcsid using "temp\t5e"
drop _merge

// + wave 6 child interview
merge 1:1 mcsid using "w6\mcs6_cm_interview.dta"
drop _merge
save "temp\t6"

// + wave 6 parent_cm interview (main)
use "w6\mcs6_parent_cm_interview.dta", clear
rename *, lower
drop if fpnum00!=1
drop if fcnum00!=1
merge 1:1 mcsid using "temp\t6"
drop _merge
save "temp\t6a"

// + wave 6 parent derived
use "w6\mcs6_parent_derived.dta", clear
drop if fpnum00!=1
merge 1:1 mcsid using "temp\t6a"
drop _merge
save "temp\t6b"

// + wave 6 child derived
use "w6\mcs6_cm_derived.dta", clear
drop if fcnum00!=1
merge 1:1 mcsid using "temp\t6b"
drop _merge
save "temp\t6c"

// + wave 6 child assess
use "w6\mcs6_cm_assessment.dta", clear
rename *, lower
drop if fcnum00!=1
merge 1:1 mcsid using "temp\t6c"
drop _merge
save "temp\t6d"

// + wave 6 parent assess
use "w6\mcs6_parent_assessment.dta", clear
rename *, lower
drop if fpnum00!=1
merge 1:1 mcsid using "temp\t6d"
drop _merge
save "temp\t6e"

// + wave 6 cohort member measurement
use "w6\mcs6_cm_measurement.dta", clear
rename *, lower
drop if fcnum00!=1
merge 1:1 mcsid using "temp\t6e"
drop _merge
save "temp\t6f"

// + wave 6 family derived
use "w6\mcs6_family_derived.dta", clear
rename *, lower
merge 1:1 mcsid using "temp\t6f"
drop _merge
save "temp\t6g"

// + wave 6 parent interview
use "w6\mcs6_parent_interview.dta", clear
drop if fpnum00 != 1
merge 1:1 mcsid using "temp\t6g"
drop _merge
save "temp\t6h"

// + wave 7 CM derived
use "w7\mcs7_cm_derived.dta", clear
drop if gcnum00 != 1
merge 1:1 mcsid using "temp\t6h"
drop _merge
save "temp\t7"

// + wave 7 CM interview
use "w7\mcs7_cm_interview", clear
drop if gcnum00 != 1
merge 1:1 mcsid using "temp\t7" 
drop _merge
save "temp\t7a"

// + wave 7 parent_cm interview
use "w7\mcs7_parent_cm_interview.dta", clear
drop if gpnum00!=1
drop if gcnum00!=1
merge 1:1 mcsid using "temp\t7a"
drop _merge
save "temp\t7b"

// + wave 7 cm cognitive assessment
use "w7\mcs7_cm_cognitive_assessment.dta", clear
drop if gcnum00!=1
merge 1:1 mcsid using "temp\t7b"
drop _merge
save "temp\t7c"

// + wave 7 family derived
use "w7\mcs7_family_derived.dta", clear
merge 1:1 mcsid using "temp\t7c"
drop _merge

// + wave 7 family interview
merge 1:1 mcsid using "w7\mcs7_family_interview.dta"
drop _merge
save "temp\t7d"

// + wave 7 parent interview
use "w7\mcs7_parent_interview", clear
drop if gpnum00 != 1
merge 1:1 mcsid using "temp\t7d"
drop _merge
save "temp\t7e"

// + wave 7 parent derived 
use "w7\mcs7_parent_derived", clear
drop if gpnum00 != 1
merge 1:1 mcsid using "temp\t7e"
drop _merge

// + weights
merge 1:1 mcsid using "mcs_longitudinal_family_file.dta"
drop _merge

***Merge Complete***

// Save the merged data
save "mergedwaves.dta"
clear