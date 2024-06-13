
global root "D:\Steban Pineda\Documents\DIME\Transportation and health"
global results     "${root}/Results_run_banrep"
global graphs_rips "${root}/outputs/figures/Descriptives_RIPS"
global graphs_pila "${root}/outputs/figures/Descriptives_PILA"



import excel "${root}\outputs\20240612-pila_est30.xlsx", firstrow clear

gen qtr = quarterly(period,"YQ")
format qtr %tq

foreach control of newlist asalariado edad_PILA estrato_1 estrato_2 genero ibd_salud n_registros sal_dias_cot {

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
		name(pila30_`control', replace)	
graph export "${graphs_pila}/pila30_`control'.png", replace			
}


import excel "${root}\outputs\20240612-pila_est40.xlsx", firstrow clear

gen qtr = quarterly(period,"YQ")
format qtr %tq

foreach control of newlist asalariado edad_PILA estrato_1 estrato_2 genero ibd_salud n_registros sal_dias_cot {

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
graph export "${graphs_rips}/rips30_`control'.png", replace				
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









