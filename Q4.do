clear all
set more off

use "Question 4\MURDER.dta"

*a)
label variable mrdrte "Murder Rate"
label variable exec "Number of Executions"
label variable unem "Unemployment Rate"
label variable d90 "1990 Dummy"
label variable d93 "1993 Dummy"


reg mrdrte exec unem d90 d93
eststo Q4a

esttab Q4a  using "Q4a.tex", replace ///
		stats( N  r2_a    , fmt(0 3 ) layout("\multicolumn{1}{c}{@}") labels(  `"Observations"' `"Adjusted R-Squared"')) ///
		b(3) se(5) star(* 0.10 ** 0.05 *** .01) /* Keep Betas and Standard Errors with 3 digits and set significance levels for stars*/ ///
		booktabs compress l  wrap fragment varwidth(30) nonumbers nomtitles noobs nonotes  /* Other Details */ 
		
		
*b)
xtset id year 

xtreg mrdrte exec unem d90 d93, fe
eststo Q4b

mat betafe=get(_b)
mat Vfe = get(VCE)

esttab Q4b  using "Q4b.tex", replace ///
		stats( N  r2_a    , fmt(0 3 ) layout("\multicolumn{1}{c}{@}") labels(  `"Observations"' `"Adjusted R-Squared"')) ///
		b(3) se(5) star(* 0.10 ** 0.05 *** .01) /* Keep Betas and Standard Errors with 3 digits and set significance levels for stars*/ ///
		booktabs compress l  wrap fragment varwidth(30) nonumbers nomtitles noobs nonotes  /* Other Details */ 
		

*c)
foreach var of varlist mrdrte exec unem d90 d93 {
	reg `var' i.id
	predict res_`var', residuals
}

label variable res_mrdrte " Res. Murder Rate"
label variable res_exec "Res. Number of Executions"
label variable res_unem "Res. Unemployment Rate"
label variable res_d90 "Res. 1990 Dummy"
label variable res_d93 "Res. 1993 Dummy"

reg res_mrdrte res_exec res_unem res_d90 res_d93, noconstant
eststo Q4c

esttab Q4c  using "Q4c.tex", replace ///
		stats( N  r2_a    , fmt(0 3 ) layout("\multicolumn{1}{c}{@}") labels(  `"Observations"' `"Adjusted R-Squared"')) ///
		b(3) se(5) star(* 0.10 ** 0.05 *** .01) /* Keep Betas and Standard Errors with 3 digits and set significance levels for stars*/ ///
		booktabs compress l  wrap fragment varwidth(30) nonumbers nomtitles noobs nonotes  /* Other Details */ 

		
*d)
xtreg mrdrte exec unem d90 d93
eststo Q4d

mat betare=get(_b)
mat Vre = get(VCE)

esttab Q4d  using "Q4d.tex", replace ///
		stats( N  r2_a    , fmt(0 3 ) layout("\multicolumn{1}{c}{@}") labels(  `"Observations"' `"Adjusted R-Squared"')) ///
		b(3) se(5) star(* 0.10 ** 0.05 *** .01) /* Keep Betas and Standard Errors with 3 digits and set significance levels for stars*/ ///
		booktabs compress l  wrap fragment varwidth(30) nonumbers nomtitles noobs nonotes  /* Other Details */ 
