* ssc install polychoric
* ssc install omega

clear all
set more off // do not truncate output
set maxvar 30000 // Set max variable number to 30000

// Go to the working directory
cd "C:\Users\siyan\OneDrive - The Pennsylvania State University\dissertation_working\data\"
// Load the recoded data
use "merged_waves_recoded"


*******************************************************
* MCS Sweep 2 (age 3): Bootstrap CIs for ordinal alpha & omega
* - Cluster bootstrap by PSU within strata
* - Uses one-country weight bovwt1 inside each replicate
*******************************************************

* 0) Items (ensure all coded so larger = MORE self-control)
local myvars sc3thac sc3tcom sc3obey sc3dist sc3temp sc3rest sc3fidg

* 1) Convenience: point estimates on the full sample (kept from earlier)
capture drop __*
egen __nmiss = rowmiss(`myvars')
gen  byte __complete = __nmiss==0
drop __nmiss
gen  byte __ok = __complete==1 & !missing(bovwt1, sptn00, pttype2) & bovwt1>0
svyset sptn00 [pweight=bovwt1], strata(pttype2) fpc(nh2) vce(linearized) singleunit(centered)

* (A) Alpha point estimate (weighted polychoric)
polychoric `myvars' if __ok
matrix C = r(R)
local k = colsof(C)
mata:
    C    = st_matrix("C")
    k    = rows(C)
    rbar = (sum(C) - k) / (k*(k-1))
    alpha = (k*rbar) / (1 + (k-1)*rbar)
    st_numscalar("ordinal_alpha", alpha)
end
display as text "POINT: Ordinal alpha = " %6.4f scalar(ordinal_alpha)

* (B) Omega point estimate (survey-weighted CFA)
svy: gsem (FACTOR -> `myvars', oprobit) if __ok, variance(FACTOR@1) nolog
tempname sL sTheta
scalar `sL' = 0
scalar `sTheta' = 0
foreach v of local myvars {
    scalar lam      = _b[`v':FACTOR]
    scalar lam_std  = lam / sqrt(lam^2 + 1)
    scalar theta_sd = 1 - lam_std^2
    scalar `sL'     = `sL'     + lam_std
    scalar `sTheta' = `sTheta' + theta_sd
}
scalar omega_point = (`sL'^2) / (`sL'^2 + `sTheta')
display as text "POINT: Ordinal omega (ω_total) = " %6.4f omega_point



