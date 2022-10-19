*==============================================================================*
*  PS1 
*==============================================================================*
*
*@Daniele Colombo
*
*==============================================================================*
* Preparing workspace - uploading data
*==============================================================================*

	clear all
	
	global in "C:\Users\dcolombo\Desktop\Courses\Econometrics"
	global output "C:\Users\dcolombo\Desktop\Courses\Econometrics"
	
	use "$in\PS1.dta", clear 
	
***1)
	 
	 *generating variables for regressions
	 gen exp = a0 - ed0-6	 
	 gen l_wage = log(w0)
	 gen exp_2 = exp*exp 
	 *run regression
	 reg l_wage ed0 exp exp_2 
	 eststo m1
	   	
***2)
	 
	 predict l_wage_hat 	 
	 reg l_wage l_wage_hat ed0 exp
	 eststo m2
	 drop l_wage_hat
	 
	 *we get a coefficient for l_wage_hat that is close to 1 and the other coefficients that are close to 0 since l_wage_hat contains all the explanatory power of the other coefficients (it's just a linear combination of the other variables and exp_2) In other words, tha X matrix is higher multicollinear. If exp_2 was also in the regression, we would have that the X matrix is not full rank and therefore the regression would not be feasible (coefficients would not be uniquely identified). 

***3) 

	*partial out log(wage)*
	reg l_wage ed0 exp_2 
	predict l_wage_hat_2, resid 
	
	*partial out experience*
	reg exp ed0 exp_2 
	predict exp_hat_2 , resid 
	
	*partialled out log(wage) on partialled out experience*
	reg l_wage_hat_2 exp_hat_2 
	eststo m3
	
	* We are applying partitioned regression theorem: the coefficient and the standard error coincide with the ones estimated at point 1.*
		
		
***4)
 	
	*log(wage) on partialled out experience*
	reg l_wage exp_hat_2 
	eststo m4
		
	*we get the same coefficient, but different standard errors. *
	
*==============================================================================*
* Output
*==============================================================================*
	
	estout m1 m2 m3 m4 , cells(b(star fmt(3)) se(par fmt(3)) ) starlevels(* 0.10 ** 0.05 *** 0.01) ///
	stats(N r2 , fmt(%9.0fc 3 3) l("Observations" "R_squared")) ///
	varlabels( ed0 "education" exp "experience" exp_2 "experience^2" l_wage_hat "pred.ln(wage)" exp_hat_2 "p.o. experience") 
