
global root "D:\Steban Pineda\Documents\DIME\Transportation and health"
global results     "${root}/Results_run_banrep"
global figures     "${root}/outputs/figures"
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
/*
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
*/

* Results from Jun 27, 2024 (Estimations using balanced panel) *****************
/* 1. This estimations has the running variable adjusted to capture the impact on
      treated (individuals under the cutoff.
   2. Graphics with a renewed aesthetic.
  */ 

  
/* Control mean
import excel "${root}\outputs\20240723-control_mean.xlsx", firstrow clear
	ren periodo period
	duplicates report variable period
	keep if grupo=="cutoff30"
save "${root}\outputs\20240723-control_mean30", replace

import excel "${root}\outputs\20240723-control_mean.xlsx", firstrow clear
	ren periodo period
	duplicates report variable period
	keep if grupo=="cutoff40"
save "${root}\outputs\20240723-control_mean40", replace  
*/ 
  
  
**** 30pts ----------------------------------------------------------------------  
import excel "${root}\outputs\20240715-est30.xlsx", firstrow clear

gen qtr = quarterly(period,"YQ")
format qtr %tq

foreach control of newlist n_visitas c_preven c_prenat c_cancer c_cardio c_respir n_consultas n_hospitalizaciones n_procedimientos n_urgencias d_visitas d_consultas d_hospitalizaciones d_procedimientos d_urgencias d_cancer d_cardio d_preven d_prenat d_respir pila_depen pila_indep ibc_salud d_registros n_registros sal_dias_cot {
	
twoway (scatter coef_robust qtr if variable=="`control'" & qtr>=219, ///
                msize(vsmall) msymbol(O) mcolor(midblue) lwidth(vthin)) ///
       (rcap ci_lower_robust ci_upper_robust qtr if variable=="`control'" & qtr>=219, ///
	            lcolor(emidblue) lwidth(thick) lwidth(vthin)), ///
       xtitle(Trimestre) ytitle(Coeficiente) ///
       title("") ///
       legend(order(1 "Point Estimate" 2 "95% Confidence Interval")) ///
       graphregion(color(white)) bgcolor(white) ///
       yline(0, lcolor(gray) lpattern(solid) ) ///
		xline(229, lpattern(vshortdash) lwidth(vthin) lcolor(red)) ///
        xlabel(#21, labsize(vsmall) grid glpattern(tight_dot) glcolor(gray%50) angle(45)) ///
        ylabel(#10, labsize(vsmall)  grid glpattern(tight_dot) glcolor(gray%50) format(%9.2f))  ///	
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") ///
		note("Law: After feb-2017 the cutoff was reduced to 30.56 and the discount decreased from 50% to 25%" "Running variable adjusted to capture the impact on treated (individuals under the cutoff - 30.56 points)") ///
		name(score30_`control', replace)	
graph export "${figures}/20240705-results/est30_`control'.png", replace				
}

 
**** 40pts ----------------------------------------------------------------------   
import excel "${root}\outputs\20240715-est40.xlsx", firstrow clear

gen qtr = quarterly(period,"YQ")
format qtr %tq

foreach control of newlist n_visitas c_preven c_prenat c_cancer c_cardio c_respir n_consultas n_hospitalizaciones n_procedimientos n_urgencias d_visitas d_consultas d_hospitalizaciones d_procedimientos d_urgencias d_cancer d_cardio d_preven d_prenat d_respir pila_depen pila_indep ibc_salud d_registros n_registros sal_dias_cot {

twoway (scatter coef_robust qtr if variable=="`control'" & qtr<=224, ///
                msize(vsmall) msymbol(O) mcolor(midblue) lwidth(vthin)) ///
       (rcap ci_lower_robust ci_upper_robust qtr if variable=="`control'" & qtr<=224, ///
	            lcolor(emidblue) lwidth(thick) lwidth(vthin)), ///
       xtitle(Trimestre) ytitle(Coeficiente) ///
       title("") ///
       legend(order(1 "Point Estimate" 2 "95% Confidence Interval")) ///
       graphregion(color(white)) bgcolor(white) ///
       yline(0, lcolor(gray) lpattern(solid) ) ///
		xline(216, lpattern(vshortdash) lwidth(vthin) lcolor(red)) ///
        xlabel(#17, labsize(vsmall) grid glpattern(tight_dot) glcolor(gray%50) angle(45)) ///
        ylabel(#10, labsize(vsmall)  grid glpattern(tight_dot) glcolor(gray%50) format(%9.2f))  ///	
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") ///
		note("First Law: After jan-2014 the cutoff was defined in 40 with a 50% discount in the ticket price" "Running variable adjusted to capture the impact on treated (individuals under the cutoff - 40 points)") ///
		name(score40_`control', replace)	
graph export "${figures}/20240705-results/est40_`control'.png", replace			
}


**** Balance 30pts --------------------------------------------------------------  
import excel "${root}\outputs\20240715-est30.xlsx", firstrow clear
	drop if regexm(variable, "d_") | regexm(variable, "c_") | regexm(variable, "n_")
	drop if variable=="pila_depen"   | variable=="pila_indep" | ///
	        variable=="sal_dias_cot" | variable=="ingresos"
	drop period
	duplicates drop
	tab variable
	format coef se_rob pv_rob %9.3f
	keep variable coef_robust se_rob pv_rob N1 N2
	order variable coef_robust se_rob N1 N2 pv_rob
	edit

import excel "${root}\outputs\20240715-est40.xlsx", firstrow clear
	drop if regexm(variable, "d_") | regexm(variable, "c_") | regexm(variable, "n_")
	drop if variable=="pila_depen"   | variable=="pila_indep" | ///
	        variable=="sal_dias_cot" | variable=="ingresos"
	drop period
	duplicates drop
	tab variable
	format coef se_rob pv_rob %9.3f
	keep variable coef_robust se_rob pv_rob N1 N2
	order variable coef_robust se_rob N1 N2 pv_rob
	edit


import excel "${root}\outputs\20240715-est30.xlsx", firstrow clear
	gen qtr = quarterly(period,"YQ")
	format qtr %tq
	merge 1:1 variable period using "${root}\outputs\20240723-control_mean30", keep(3)


**** New results using specific score ranges in SISBEN -------------------------  

import excel "${root}\outputs\20240801-table_est_v2.xlsx", firstrow clear

gen qtr = quarterly(period,"YQ")
format qtr %tq

tab 	dataset
replace dataset = "verify"  if dataset=="est2535_verification"
replace dataset = "est2040" if regexm(dataset, "est2040")
replace dataset = "est2535" if regexm(dataset, "est2535")
replace dataset = "est3050" if regexm(dataset, "est3050")
replace dataset = "est3545" if regexm(dataset, "est3545")

* Treatment: people under 30.56 points in the group between 20 and 40 points

foreach group of newlist est2040 {

foreach control of newlist n_visitas c_preven c_prenat c_cancer c_cardio c_respir n_consultas n_hospitalizaciones n_procedimientos n_urgencias d_visitas d_consultas d_hospitalizaciones d_procedimientos d_urgencias d_cancer d_cardio d_preven d_prenat d_respir pila_depen pila_indep ibc_salud d_registros n_registros sal_dias_cot  {
    twoway (scatter coef_robust qtr if dataset=="`group'" & variable=="`control'" & qtr <= 216, ///
                    msize(vsmall) msymbol(O) mcolor(midblue) lwidth(vthin)) ///
           (scatter coef_robust qtr if dataset=="`group'" & variable=="`control'" & qtr > 216 & qtr <= 229, ///
                    msize(vsmall) msymbol(O) mcolor(green) lwidth(vthin)) ///
           (scatter coef_robust qtr if dataset=="`group'" & variable=="`control'" & qtr >= 230, ///
                    msize(vsmall) msymbol(O) mcolor(red) lwidth(vthin)) ///
           (rcap ci_lower_robust ci_upper_robust qtr if dataset=="`group'" & variable=="`control'", ///
	                lcolor(emidblue) lwidth(thick) lwidth(vthin)), ///
           xtitle(Trimestre) ytitle(Coeficiente) ///
           title("") ///
           legend(order(1 "No subsidy vs No subsidy" 2 "50% subsidy vs 50% subsidy" 3 "25% subsidy vs No subsidy")) ///
           graphregion(color(white)) bgcolor(white) ///
           yline(0, lcolor(gray) lpattern(solid) ) ///
		   xline(216, lpattern(vshortdash) lwidth(vthin) lcolor(red)) ///	   
		   xline(229, lpattern(vshortdash) lwidth(vthin) lcolor(red)) ///
           xlabel(#32, labsize(vsmall) grid glpattern(tight_dot) glcolor(gray%50) angle(45)) ///
           ylabel(#10, labsize(vsmall) grid glpattern(tight_dot) glcolor(gray%50) format(%9.2f)) ///	
           legend(position(6) col(4)) ///
		   xtitle("") ytitle("") ///
		   note("Running variable adjusted to capture the impact on treated: people under 30 points") ///
		   name(`group'_`control', replace)	

graph export "${figures}/20240801-results/`group'_`control'.png", replace		   

}
}

* Treatment: people under 30.56 points in the group between 25 and 35 points

foreach group of newlist est2535 {

foreach control of newlist n_visitas c_preven c_prenat c_cancer c_cardio c_respir n_consultas n_hospitalizaciones n_procedimientos n_urgencias d_visitas d_consultas d_hospitalizaciones d_procedimientos d_urgencias d_cancer d_cardio d_preven d_prenat d_respir pila_depen pila_indep ibc_salud d_registros n_registros sal_dias_cot  {
    twoway (scatter coef_robust qtr if dataset=="`group'" & variable=="`control'" & qtr <= 216, ///
                    msize(vsmall) msymbol(O) mcolor(midblue) lwidth(vthin)) ///
           (scatter coef_robust qtr if dataset=="`group'" & variable=="`control'" & qtr > 216 & qtr <= 229, ///
                    msize(vsmall) msymbol(O) mcolor(green) lwidth(vthin)) ///
           (scatter coef_robust qtr if dataset=="`group'" & variable=="`control'" & qtr >= 230, ///
                    msize(vsmall) msymbol(O) mcolor(red) lwidth(vthin)) ///
           (rcap ci_lower_robust ci_upper_robust qtr if dataset=="`group'" & variable=="`control'", ///
	                lcolor(emidblue) lwidth(thick) lwidth(vthin)), ///
           xtitle(Trimestre) ytitle(Coeficiente) ///
           title("") ///
           legend(order(1 "No subsidy vs No subsidy" 2 "50% subsidy vs 50% subsidy" 3 "25% subsidy vs No subsidy")) ///
           graphregion(color(white)) bgcolor(white) ///
           yline(0, lcolor(gray) lpattern(solid) ) ///
		   xline(216, lpattern(vshortdash) lwidth(vthin) lcolor(red)) ///	   
		   xline(229, lpattern(vshortdash) lwidth(vthin) lcolor(red)) ///
           xlabel(#32, labsize(vsmall) grid glpattern(tight_dot) glcolor(gray%50) angle(45)) ///
           ylabel(#10, labsize(vsmall) grid glpattern(tight_dot) glcolor(gray%50) format(%9.2f)) ///	
           legend(position(6) col(4)) ///
		   xtitle("") ytitle("") ///
		   note("Running variable adjusted to capture the impact on treated: people under 30 points") ///
		   name(`group'_`control', replace)	

graph export "${figures}/20240801-results/`group'_`control'.png", replace		   
		   
}
}



* Treatment: people under 40 points in the group between 30 and 50 points

foreach group of newlist est3050 {

foreach control of newlist n_visitas c_preven c_prenat c_cancer c_cardio c_respir n_consultas n_hospitalizaciones n_procedimientos n_urgencias d_visitas d_consultas d_hospitalizaciones d_procedimientos d_urgencias d_cancer d_cardio d_preven d_prenat d_respir pila_depen pila_indep ibc_salud d_registros n_registros sal_dias_cot  {
    twoway (scatter coef_robust qtr if dataset=="`group'" & variable=="`control'" & qtr <= 216, ///
                    msize(vsmall) msymbol(O) mcolor(midblue) lwidth(vthin)) ///
           (scatter coef_robust qtr if dataset=="`group'" & variable=="`control'" & qtr > 216 & qtr <= 229, ///
                    msize(vsmall) msymbol(O) mcolor(green) lwidth(vthin)) ///
           (scatter coef_robust qtr if dataset=="`group'" & variable=="`control'" & qtr >= 230, ///
                    msize(vsmall) msymbol(O) mcolor(red) lwidth(vthin)) ///
           (rcap ci_lower_robust ci_upper_robust qtr if dataset=="`group'" & variable=="`control'", ///
	                lcolor(emidblue) lwidth(thick) lwidth(vthin)), ///
           xtitle(Trimestre) ytitle(Coeficiente) ///
           title("") ///
           legend(order(1 "No subsidy vs No subsidy" 2 "50% subsidy vs No subsidy" 3 "No subsidy vs No subsidy")) ///
           graphregion(color(white)) bgcolor(white) ///
           yline(0, lcolor(gray) lpattern(solid) ) ///
		   xline(216, lpattern(vshortdash) lwidth(vthin) lcolor(red)) ///	   
		   xline(229, lpattern(vshortdash) lwidth(vthin) lcolor(red)) ///
           xlabel(#32, labsize(vsmall) grid glpattern(tight_dot) glcolor(gray%50) angle(45)) ///
           ylabel(#10, labsize(vsmall) grid glpattern(tight_dot) glcolor(gray%50) format(%9.2f)) ///	
           legend(position(6) col(4)) ///
		   xtitle("") ytitle("") ///
		   note("Running variable adjusted to capture the impact on treated: people under 40 points") ///
		   name(`group'_`control', replace)	

graph export "${figures}/20240801-results/`group'_`control'.png", replace		   
		   
}
}

* Treatment: people under 40 points in the group between 35 and 45 points

foreach group of newlist est3545 {

foreach control of newlist n_visitas c_preven c_prenat c_cancer c_cardio c_respir n_consultas n_hospitalizaciones n_procedimientos n_urgencias d_visitas d_consultas d_hospitalizaciones d_procedimientos d_urgencias d_cancer d_cardio d_preven d_prenat d_respir pila_depen pila_indep ibc_salud d_registros n_registros sal_dias_cot  {
    twoway (scatter coef_robust qtr if dataset=="`group'" & variable=="`control'" & qtr <= 216, ///
                    msize(vsmall) msymbol(O) mcolor(midblue) lwidth(vthin)) ///
           (scatter coef_robust qtr if dataset=="`group'" & variable=="`control'" & qtr > 216 & qtr <= 229, ///
                    msize(vsmall) msymbol(O) mcolor(green) lwidth(vthin)) ///
           (scatter coef_robust qtr if dataset=="`group'" & variable=="`control'" & qtr >= 230, ///
                    msize(vsmall) msymbol(O) mcolor(red) lwidth(vthin)) ///
           (rcap ci_lower_robust ci_upper_robust qtr if dataset=="`group'" & variable=="`control'", ///
	                lcolor(emidblue) lwidth(thick) lwidth(vthin)), ///
           xtitle(Trimestre) ytitle(Coeficiente) ///
           title("") ///
           legend(order(1 "No subsidy vs No subsidy" 2 "50% subsidy vs No subsidy" 3 "No subsidy vs No subsidy")) ///
           graphregion(color(white)) bgcolor(white) ///
           yline(0, lcolor(gray) lpattern(solid) ) ///
		   xline(216, lpattern(vshortdash) lwidth(vthin) lcolor(red)) ///	   
		   xline(229, lpattern(vshortdash) lwidth(vthin) lcolor(red)) ///
           xlabel(#32, labsize(vsmall) grid glpattern(tight_dot) glcolor(gray%50) angle(45)) ///
           ylabel(#10, labsize(vsmall) grid glpattern(tight_dot) glcolor(gray%50) format(%9.2f)) ///	
           legend(position(6) col(4)) ///
		   xtitle("") ytitle("") ///
		   note("Running variable adjusted to capture the impact on treated: people under 40 points") ///
		   name(`group'_`control', replace)	
		   
graph export "${figures}/20240801-results/`group'_`control'.png", replace		   
		   
}
}

















