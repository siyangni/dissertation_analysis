*---------------------------------------------------------------*
* Set up: keep only the identifiers and the five age variables  *
* Replace mcsid with your child identifier if it is different.  *
*---------------------------------------------------------------*
keep mcsid age5 age7 age11 age14 age17

*---------------------------------------------------------------*
* Reshape from wide to long so we have one row per child-wave   *
* The variable 'wave' will take values 5, 7, 11, 14, 17         *
* and 'age' will be the age (in years) measured at that wave.   *
*---------------------------------------------------------------*
reshape long age, i(mcsid) j(wave)

* If age is not already in years, convert it here.
* For example, if you have months: replace age = age/12

* Keep records with observed age
drop if missing(age)

*---------------------------------------------------------------*
* Compute, by wave, the count, mean age, and standard deviation *
*---------------------------------------------------------------*
collapse (count) n=age (mean) mean_age=age (sd) sd_age=age, by(wave)

* Order the waves numerically just in case
sort wave

*---------------------------------------------------------------*
* Compute the gap to the next wave and the ratio SD / gap       *
*---------------------------------------------------------------*
gen  delta_to_next = mean_age[_n+1] - mean_age
replace delta_to_next = . if _n == _N

gen  ratio = sd_age / delta_to_next
replace ratio = . if missing(delta_to_next)

* Pretty formatting and on‑screen display
format mean_age sd_age delta_to_next ratio %9.3f
list wave n mean_age sd_age delta_to_next ratio, noobs sepby(wave)

* Optional: save a CSV you can drop into your manuscript or notes
export delimited using "age_spread_by_wave_wide.csv", replace

restore
