
global root "D:\Steban Pineda\Documents\DIME\Transportation and health"
global results     "${root}/Results_run_banrep"
global graphs_rips "${root}/outputs/figures/Descriptives_RIPS"
global graphs_pila "${root}/outputs/figures/Descriptives_PILA"

* Results from Jun 06, 2024 (Unbalanced data) **********************************
/*
import excel "${root}\outputs\20240612-pila_est30.xlsx", firstrow clear

gen qtr = quarterly(period,"YQ")
format qtr %tq

foreach control of newlist asalariado edad_PILA estrato_1 estrato_2 genero ibd_salud n_registros sal_dias_cot {

twoway (scatter coef_robust qtr if variable=="`control'", ///
                msize(vsmall) msymbol(O) mcolor(midblue) lwidth(vthin)) ///
       (rcap ci_lower_robust ci_upper_robust qtr if variable=="`control'", ///
	            lcolor(emidblue) lwidth(thick) lwidth(vthin)), ///
       xtitle(Trimestre) ytitle(Coeficiente) ///
       title("Evolution of Quarterly Coefficients") ///
       legend(order(1 "Coefficients" 2 "Confidence Interval")) ///
       graphregion(color(white)) bgcolor(white) ///
       yline(0, lcolor(red) lpattern(dash)) ///
		xline(216, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(224, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(231, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
        xlabel(#20, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, labsize(vsmall)) ///	
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") ///
		name(pila30_`control', replace)	
graph export "${graphs_pila}/pila30_`control'.png", replace			
}


import excel "${root}\outputs\20240612-pila_est40.xlsx", firstrow clear

gen qtr = quarterly(period,"YQ")
format qtr %tq

foreach control of newlist asalariado edad_PILA estrato_1 estrato_2 genero ibd_salud n_registros sal_dias_cot {

twoway (scatter coef_robust qtr if variable=="`control'", ///
                msize(vsmall) msymbol(O) mcolor(midblue) lwidth(vthin)) ///
       (rcap ci_lower_robust ci_upper_robust qtr if variable=="`control'", ///
	            lcolor(emidblue) lwidth(thick) lwidth(vthin)), ///
       xtitle(Trimestre) ytitle(Coeficiente) ///
       title("Evolution of Quarterly Coefficients") ///
       legend(order(1 "Coefficients" 2 "Confidence Interval")) ///
       graphregion(color(white)) bgcolor(white) ///
       yline(0, lcolor(red) lpattern(dash)) ///
		xline(216, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(224, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(231, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
        xlabel(#20, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, labsize(vsmall)) ///	
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") ///
		name(pila40_`control', replace)	
graph export "${graphs_pila}/pila40_`control'.png", replace			
}


import excel "${root}\outputs\20240612-rips_est30.xlsx", firstrow clear

gen qtr = quarterly(period,"YQ")
format qtr %tq

foreach control of newlist edad_RIPS estrato_1 estrato_2 genero n_consultas n_hospitalizaciones n_procedimientos n_urgencias n_visitas c_cancer c_cardio c_prenat c_preven c_respir {
	
twoway (scatter coef_robust qtr if variable=="`control'", ///
                msymbol(O) mcolor(midblue) lwidth(vthin)) ///
       (rcap ci_lower_robust ci_upper_robust qtr if variable=="`control'", ///
	            lcolor(emidblue) lwidth(thick) lwidth(vthin)), ///
       xtitle(Trimestre) ytitle(Coeficiente) ///
       title("Evolution of Quarterly Coefficients") ///
       legend(order(1 "Coefficients" 2 "Confidence Interval")) ///
       graphregion(color(white)) bgcolor(white) ///
       yline(0, lcolor(red) lpattern(dash)) ///
		xline(216, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(224, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(231, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
        xlabel(#20, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, labsize(vsmall)) ///	
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") ///
		name(rips30_`control', replace)		
*graph export "${graphs_rips}/rips30_`control'.png", replace				
}


import excel "${root}\outputs\20240612-rips_est40.xlsx", firstrow clear

gen qtr = quarterly(period,"YQ")
format qtr %tq

foreach control of newlist edad_RIPS estrato_1 estrato_2 genero n_consultas n_hospitalizaciones n_procedimientos n_urgencias n_visitas c_cancer c_cardio c_prenat c_preven c_respir {
	
twoway (scatter coef_robust qtr if variable=="`control'", ///
                msymbol(O) mcolor(midblue) lwidth(vthin)) ///
       (rcap ci_lower_robust ci_upper_robust qtr if variable=="`control'", ///
	            lcolor(emidblue) lwidth(thick) lwidth(vthin)), ///
       xtitle(Trimestre) ytitle(Coeficiente) ///
       title("Evolution of Quarterly Coefficients") ///
       legend(order(1 "Coefficients" 2 "Confidence Interval")) ///
       graphregion(color(white)) bgcolor(white) ///
       yline(0, lcolor(red) lpattern(dash)) ///
		xline(216, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(224, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(231, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
        xlabel(#20, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, labsize(vsmall)) ///	
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") ///
		name(rips40_`control', replace)		
graph export "${graphs_rips}/rips40_`control'.png", replace				
}
*/
* Results from Jun 27, 2024 (Estimations using balanced panel) *****************
/* 1. This estimations has the running variable adjusted to capture the impact on
      treated (individuals under the cutoff.
   2. Graphics with a renewed aesthetic.
  */ 

**** RIPS ----------------------------------------------------------------------  
import excel "${root}\outputs\20240627-rips_est30.xlsx", firstrow clear

gen qtr = quarterly(period,"YQ")
format qtr %tq

foreach control of newlist n_visitas c_preven c_prenat c_cancer c_cardio c_respir n_consultas n_hospitalizaciones n_procedimientos n_urgencias d_visitas d_consultas d_hospitalizaciones d_procedimientos d_urgencias d_cancer d_cardio d_preven d_prenat d_respir {
	
twoway (scatter coef_robust qtr if variable=="`control'", ///
                msize(vsmall) msymbol(O) mcolor(midblue) lwidth(vthin)) ///
       (rcap ci_lower_robust ci_upper_robust qtr if variable=="`control'", ///
	            lcolor(emidblue) lwidth(thick) lwidth(vthin)), ///
       xtitle(Trimestre) ytitle(Coeficiente) ///
       title("") ///
       legend(order(1 "Point Estimate" 2 "95% Confidence Interval")) ///
       graphregion(color(white)) bgcolor(white) ///
       yline(0, lcolor(gray) lpattern(solid) ) ///
		xline(216, lpattern(vshortdash) lwidth(vthin) lcolor(red)) ///
		xline(229, lpattern(vshortdash) lwidth(vthin) lcolor(red)) ///
        xlabel(#32, labsize(vsmall) grid glpattern(tight_dot) glcolor(gray%50) angle(45)) ///
        ylabel(#10, labsize(vsmall)  grid glpattern(tight_dot) glcolor(gray%50) format(%9.3f))  ///	
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") ///
		note("Using 30.56 points as the cutoff to estimate" "Running variable adjusted to capture the impact on treated (individuals under the cutoff)") ///
		name(rips30_`control', replace)			
graph export "${graphs_rips}/20240627-`control'_rips30.png", replace				
}

import excel "${root}\outputs\20240627-rips_est40.xlsx", firstrow clear

gen qtr = quarterly(period,"YQ")
format qtr %tq

foreach control of newlist n_visitas c_preven c_prenat c_cancer c_cardio c_respir n_consultas n_hospitalizaciones n_procedimientos n_urgencias d_visitas d_consultas d_hospitalizaciones d_procedimientos d_urgencias d_cancer d_cardio d_preven d_prenat d_respir {
	
twoway (scatter coef_robust qtr if variable=="`control'", ///
                msize(vsmall) msymbol(O) mcolor(midblue) lwidth(vthin)) ///
       (rcap ci_lower_robust ci_upper_robust qtr if variable=="`control'", ///
	            lcolor(emidblue) lwidth(thick) lwidth(vthin)), ///
       xtitle(Trimestre) ytitle(Coeficiente) ///
       title("") ///
       legend(order(1 "Point Estimate" 2 "95% Confidence Interval")) ///
       graphregion(color(white)) bgcolor(white) ///
       yline(0, lcolor(gray) lpattern(solid) ) ///
		xline(216, lpattern(vshortdash) lwidth(vthin) lcolor(red)) ///
		xline(229, lpattern(vshortdash) lwidth(vthin) lcolor(red)) ///
        xlabel(#32, labsize(vsmall) grid glpattern(tight_dot) glcolor(gray%50) angle(45)) ///
        ylabel(#10, labsize(vsmall)  grid glpattern(tight_dot) glcolor(gray%50) format(%9.3f)) ///	
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") ///
		note("Using 40 points as the cutoff to estimate" "Running variable adjusted to capture the impact on treated (individuals under the cutoff)") ///		
		name(rips40_`control', replace)			
graph export "${graphs_rips}/20240627-`control'_rips40.png", replace				
}
  
  
  
**** PILA ----------------------------------------------------------------------  
import excel "${root}\outputs\20240627-pila_est30.xlsx", firstrow clear

gen qtr = quarterly(period,"YQ")
format qtr %tq

foreach control of newlist asalariado ibc_salud n_registros sal_dias_cot {

twoway (scatter coef_robust qtr if variable=="`control'", ///
                msize(vsmall) msymbol(O) mcolor(midblue) lwidth(vthin)) ///
       (rcap ci_lower_robust ci_upper_robust qtr if variable=="`control'", ///
	            lcolor(emidblue) lwidth(thick) lwidth(vthin)), ///
       xtitle(Trimestre) ytitle(Coeficiente) ///
       title("") ///
       legend(order(1 "Point Estimate" 2 "95% Confidence Interval")) ///
       graphregion(color(white)) bgcolor(white) ///
       yline(0, lcolor(gray) lpattern(solid) ) ///
		xline(216, lpattern(vshortdash) lwidth(vthin) lcolor(red)) ///
		xline(229, lpattern(vshortdash) lwidth(vthin) lcolor(red)) ///
        xlabel(#32, labsize(vsmall) grid glpattern(tight_dot) glcolor(gray%50) angle(45)) ///
        ylabel(#10, labsize(vsmall)  grid glpattern(tight_dot) glcolor(gray%50)) ///	
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") ///
		note("Using 30.56 points as the cutoff to estimate" "Running variable adjusted to capture the impact on treated (individuals under the cutoff)") ///
		name(pila30_`control', replace)
graph export "${graphs_pila}/20240627-`control'_pila30.png", replace			
}


import excel "${root}\outputs\20240627-pila_est40.xlsx", firstrow clear

gen qtr = quarterly(period,"YQ")
format qtr %tq

foreach control of newlist asalariado ibc_salud n_registros sal_dias_cot {

twoway (scatter coef_robust qtr if variable=="`control'", ///
                msize(vsmall) msymbol(O) mcolor(midblue) lwidth(vthin)) ///
       (rcap ci_lower_robust ci_upper_robust qtr if variable=="`control'", ///
	            lcolor(emidblue) lwidth(thick) lwidth(vthin)), ///
       xtitle(Trimestre) ytitle(Coeficiente) ///
       title("") ///
       legend(order(1 "Point Estimate" 2 "95% Confidence Interval")) ///
       graphregion(color(white)) bgcolor(white) ///
       yline(0, lcolor(gray) lpattern(solid) ) ///
		xline(216, lpattern(vshortdash) lwidth(vthin) lcolor(red)) ///
		xline(229, lpattern(vshortdash) lwidth(vthin) lcolor(red)) ///
        xlabel(#32, labsize(vsmall) grid glpattern(tight_dot) glcolor(gray%50) angle(45)) ///
        ylabel(#10, labsize(vsmall)  grid glpattern(tight_dot) glcolor(gray%50)) ///	
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("")  ///
		note("Using 40 points as the cutoff to estimate" "Running variable adjusted to capture the impact on treated (individuals under the cutoff)") ///
		name(pila40_`control', replace)
graph export "${graphs_pila}/20240627-`control'_pila40.png", replace			
}












