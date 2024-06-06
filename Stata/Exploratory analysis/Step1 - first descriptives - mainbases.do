

global root "D:\Steban Pineda\Documents\DIME\Transportation and health"
global results "${root}/Results_run_banrep"
global graphs "${root}/outputs/figures/Descriptives_RIPS"
global aux_db  "${root}/Results_run_banrep/auxiliar_databases"

foreach x of newlist all main p30 p40 p2545 {
import excel "${root}/Results_run_banrep/20240529-try1_descriptives_rips.xlsx", sheet("`x'_collapse") firstrow clear
	gen month = mofd(month)
	format month %tm
	ren sum_* *
	collapse (sum) c_* h_* p_* u_*, by(year month) 	
	compress
	save "${aux_db}/`x'_collapse_total", replace


import excel "${root}/Results_run_banrep/20240529-try1_descriptives_rips.xlsx", sheet("`x'_collapse") firstrow clear
	gen month = mofd(month)
	format month %tm
	ren sum_* *
	collapse (sum) c_* h_* p_* u_*, by(year month sexo)
 	
	foreach var of varlist c_* h_* p_* u_* {
		
		gen f_`var' = `var' if sexo=="F"
		gen m_`var' = `var' if sexo=="M"
		egen fem_`var' = max(f_`var'), by(month)
		egen mal_`var' = max(m_`var'), by(month)		
		drop f_`var' m_`var'  `var'
	}
	drop sexo
	duplicates drop
	compress
	save "${aux_db}/`x'_gender_collapse_total", replace
		
import excel "${root}/Results_run_banrep/20240529-try1_descriptives_rips.xlsx", sheet("`x'_collapse") firstrow clear
	gen month = mofd(month)
	format month %tm
	ren sum_* *
	collapse (sum) c_* h_* p_* u_*, by(year month grupo_edad)
	drop if grupo_edad == .
 	
	foreach var of varlist c_* h_* p_* u_* {
		
		gen g1_`var' = `var' if grupo_edad==1
		gen g2_`var' = `var' if grupo_edad==2
		gen g3_`var' = `var' if grupo_edad==3		
		egen age1_`var' = max(g1_`var'), by(month)
		egen age2_`var' = max(g2_`var'), by(month)		
		egen age3_`var' = max(g3_`var'), by(month)				
		drop g1* g2* g3*  `var'
	}
	drop grupo_edad
	duplicates drop
	compress
	save "${aux_db}/`x'_age_collapse_total", replace

import excel "${root}/Results_run_banrep/20240529-try1_descriptives_rips.xlsx", sheet("`x'_id_mes") firstrow clear	
	gen month = mofd(month)
	format month %tm
	drop monthly
	sort month
	ren total total_registres
	compress
	save "${aux_db}/`x'_total_registres", replace	
	
import excel "${root}/Results_run_banrep/20240529-try1_descriptives_rips.xlsx", sheet("`x'_id_mes_modulo") firstrow clear
	gen month = mofd(month)
	format month %tm
	sort month
	foreach module of newlist c h p u {		
		gen m`module' = total_persona if MODULE=="`module'"
		egen total_`module' = max(m`module'), by(month)
		drop m`module'
	}
	drop total_persona	
	keep month total*
	duplicates drop 
	compress
	save "${aux_db}/`x'_total_module_registres", replace		
}	

	
	
	
foreach x of newlist all main p30 p40 p2545 {	
	
	use "${aux_db}/`x'_total_registres", replace	
		merge 1:1 month using "${aux_db}/`x'_total_module_registres", nogen	
		merge 1:1 month using "${aux_db}/`x'_collapse_total", nogen	
		merge 1:1 month using "${aux_db}/`x'_gender_collapse_total", nogen
		merge 1:1 month using "${aux_db}/`x'_age_collapse_total", nogen	
		order year month
		compress
	save "${results}/rips_summarise_`x'", replace	
	
}


**********************************************
*   ALL SAMPLE IN RIPS BOGOTA
**********************************************
	
use "${results}/rips_summarise_all", replace		

* Total de registros en los modulos
twoway (line total_c month if inrange(year, 2015, 2018), lcol(edkblue))  ///
       (line total_p month if inrange(year, 2015, 2018), lcol(ebblue))   ///			
       (line total_h month if inrange(year, 2015, 2018), lcol(emidblue) yaxis(2)) ///	   
       (line total_u month if inrange(year, 2015, 2018), lcol(midblue)  yaxis(2)), ///	   
		xline(672, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#15, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, labsize(vsmall)) ///
        ylabel(#6, labsize(vsmall) axis(2)) ///		
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") title("") ///
		legend(label(1 "Consultation")) ///
		legend(label(2 "Procedures")) ///		
		legend(label(3 "Hospitalizations")) ///
		legend(label(4 "Emergencies")) ///
		note("Left axis: Consultation, Procedures" "Right axis: Hospitalizations, Emergencies") ///
		name(all_modules, replace)
graph export "${graphs}/all_modules.png", replace	
	
*** CONSULTAS ------------------------------------------------------------------
* preventivas por género
twoway (line     c_preventivas month if inrange(year, 2015, 2018), lcol(edkblue)) ///
       (line fem_c_preventivas month if inrange(year, 2015, 2018), lcol(ebblue)) ///					   
       (line mal_c_preventivas month if inrange(year, 2015, 2018), lcol(emidblue)), ///  
		xline(672, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#15, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, labsize(vsmall)) ///
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") title("") ///
		legend(label(1 "Total")) ///
		legend(label(2 "Female")) ///		
		legend(label(3 "Male")) ///
		note("Preventive consultations") ///
		name(all_c_preventives, replace)	
graph export "${graphs}/all_c_preventives.png", replace		
	
* prenatales por género
twoway (line     c_prenatales month if inrange(year, 2015, 2018), lcol(edkblue)) ///
       (line fem_c_prenatales month if inrange(year, 2015, 2018), lcol(ebblue)) ///				   
       (line mal_c_prenatales month if inrange(year, 2015, 2018), lcol(emidblue)), ///	   
		xline(672, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#15, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, labsize(vsmall)) ///
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") title("") ///
		legend(label(1 "Total")) ///
		legend(label(2 "Female")) ///		
		legend(label(3 "Male")) ///
		note("Prenatal consultations") ///
		name(all_c_prenatales, replace)		
graph export "${graphs}/all_c_prenatales.png", replace	
		
* Cronicas	
twoway (line c_cron_cancer         month if inrange(year, 2015, 2018), lcol(edkblue))  ///
       (line c_cron_renal          month if inrange(year, 2015, 2018), lcol(ebblue))   ///			
       (line c_cron_respiratoria   month if inrange(year, 2015, 2018), lcol(emidblue)) ///	   
       (line c_cron_cardiovascular month if inrange(year, 2015, 2018), lcol(midblue)   yaxis(2)), ///	   
		xline(672, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#15, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, labsize(vsmall)) ///
        ylabel(#6, labsize(vsmall) axis(2)) ///		
		legend(position(6)  col(4) order(1 2 3 4)) ///
		xtitle("")  ytitle("") ytitle("", axis(2)) title("") ///
		legend(label(1 "Cancer")) ///
		legend(label(2 "Renal")) ///		
		legend(label(3 "Respiratory")) ///
		legend(label(4 "Cardiovascular")) ///
		note("Left axis: Cancer, Renal, Respiratory" "Right axis: Cardiovascular") ///
		name(all_c_cronicas, replace)	
graph export "${graphs}/all_c_cronicas.png", replace	
	
*** PROCEDIMIENTOS -------------------------------------------------------------	
* preventivas por género
twoway (line     p_preventivas month if inrange(year, 2015, 2018), lcol(edkblue)) ///
       (line fem_p_preventivas month if inrange(year, 2015, 2018), lcol(ebblue)) ///					   
       (line mal_p_preventivas month if inrange(year, 2015, 2018), lcol(emidblue)), ///  
		xline(672, lpattern(dasp_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dasp_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#15, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, labsize(vsmall)) ///
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") title("") ///
		legend(label(1 "Total")) ///
		legend(label(2 "Female")) ///		
		legend(label(3 "Male")) ///
		note("Preventive procedures") ///
		name(all_p_preventives, replace)	
graph export "${graphs}/all_p_preventives.png", replace		
	
* prenatales por género
twoway (line     p_prenatales month if inrange(year, 2015, 2018), lcol(edkblue)) ///
       (line fem_p_prenatales month if inrange(year, 2015, 2018), lcol(ebblue)) ///				   
       (line mal_p_prenatales month if inrange(year, 2015, 2018), lcol(emidblue)), ///	   
		xline(672, lpattern(dasp_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dasp_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#15, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, labsize(vsmall)) ///
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") title("") ///
		legend(label(1 "Total")) ///
		legend(label(2 "Female")) ///		
		legend(label(3 "Male")) ///
		note("Prenatal procedures") ///
		name(all_p_prenatales, replace)		
graph export "${graphs}/all_p_prenatales.png", replace	
		
* Cronicas	
twoway (line p_cron_cancer         month if inrange(year, 2015, 2018), lcol(edkblue))  ///
       (line p_cron_renal          month if inrange(year, 2015, 2018), lcol(ebblue))   ///			
       (line p_cron_respiratoria   month if inrange(year, 2015, 2018), lcol(emidblue)) ///	   
       (line p_cron_cardiovascular month if inrange(year, 2015, 2018), lcol(midblue)   yaxis(2)), ///	   
		xline(672, lpattern(dasp_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dasp_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#15, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, labsize(vsmall)) ///
        ylabel(#6, labsize(vsmall) axis(2)) ///		
		legend(position(6)  col(4) order(1 2 3 4)) ///
		xtitle("")  ytitle("") ytitle("", axis(2)) title("") ///
		legend(label(1 "Cancer")) ///
		legend(label(2 "Renal")) ///		
		legend(label(3 "Respiratory")) ///
		legend(label(4 "Cardiovascular")) ///
		note("Left axis: Cancer, Renal, Respiratory" "Right axis: Cardiovascular") ///
		name(all_p_cronicas, replace)		
graph export "${graphs}/all_p_cronicas.png", replace		
	
	
	
**********************************************
*   RIPS BOGOTA WITH BOGOTA
**********************************************
		
use "${results}/rips_summarise_main", replace		

line total_registres month if inrange(year, 2015, 2020), lcol(midblue) ///
		xline(672, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#12, labsize(vsmall) grid angle(45)) ///
        ylabel(#10, labsize(vsmall)) ///
		xtitle("")  ytitle("Total registres") title("") ///
		note("All different personabasicaid by month")
graph export "${graphs}/01_serie.png", replace	
		
* Total de registros en los modulos
twoway (line total_c month if inrange(year, 2015, 2018), lcol(edkblue))  ///
       (line total_p month if inrange(year, 2015, 2018), lcol(ebblue))   ///			
       (line total_h month if inrange(year, 2015, 2018), lcol(emidblue) yaxis(2)) ///	   
       (line total_u month if inrange(year, 2015, 2018), lcol(midblue)  yaxis(2)), ///	   
		xline(672, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#15, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, labsize(vsmall)) ///
        ylabel(#6, labsize(vsmall) axis(2)) ///		
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") title("") ///
		legend(label(1 "Consultation")) ///
		legend(label(2 "Procedures")) ///		
		legend(label(3 "Hospitalizations")) ///
		legend(label(4 "Emergencies")) ///
		note("Left axis: Consultation, Procedures" "Right axis: Hospitalizations, Emergencies") ///
		name(main_modules, replace)
graph export "${graphs}/main_modules.png", replace	
	
*** CONSULTAS ------------------------------------------------------------------
* preventivas por género
twoway (line     c_preventivas month if inrange(year, 2015, 2018), lcol(edkblue)) ///
       (line fem_c_preventivas month if inrange(year, 2015, 2018), lcol(ebblue)) ///					   
       (line mal_c_preventivas month if inrange(year, 2015, 2018), lcol(emidblue)), ///  
		xline(672, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#15, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, labsize(vsmall)) ///
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") title("") ///
		legend(label(1 "Total")) ///
		legend(label(2 "Female")) ///		
		legend(label(3 "Male")) ///
		note("Preventive consultations") ///
		name(main_c_preventives, replace)	
graph export "${graphs}/main_c_preventives.png", replace		
	
* prenatales por género
twoway (line     c_prenatales month if inrange(year, 2015, 2018), lcol(edkblue)) ///
       (line fem_c_prenatales month if inrange(year, 2015, 2018), lcol(ebblue)) ///				   
       (line mal_c_prenatales month if inrange(year, 2015, 2018), lcol(emidblue)), ///	   
		xline(672, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#15, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, labsize(vsmall)) ///
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") title("") ///
		legend(label(1 "Total")) ///
		legend(label(2 "Female")) ///		
		legend(label(3 "Male")) ///
		note("Prenatal consultations") ///
		name(main_c_prenatales, replace)		
graph export "${graphs}/main_c_prenatales.png", replace	
		
* Cronicas	
twoway (line c_cron_cancer         month if inrange(year, 2015, 2018), lcol(edkblue))  ///
       (line c_cron_renal          month if inrange(year, 2015, 2018), lcol(ebblue))   ///			
       (line c_cron_respiratoria   month if inrange(year, 2015, 2018), lcol(emidblue)) ///	   
       (line c_cron_cardiovascular month if inrange(year, 2015, 2018), lcol(midblue)   yaxis(2)), ///	   
		xline(672, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#15, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, labsize(vsmall)) ///
        ylabel(#6, labsize(vsmall) axis(2)) ///		
		legend(position(6)  col(4) order(1 2 3 4)) ///
		xtitle("")  ytitle("") ytitle("", axis(2)) title("") ///
		legend(label(1 "Cancer")) ///
		legend(label(2 "Renal")) ///		
		legend(label(3 "Respiratory")) ///
		legend(label(4 "Cardiovascular")) ///
		note("Left axis: Cancer, Renal, Respiratory" "Right axis: Cardiovascular") ///
		name(main_c_cronicas, replace)	
graph export "${graphs}/main_c_cronicas.png", replace	
	
*** PROCEDIMIENTOS -------------------------------------------------------------	
* preventivas por género
twoway (line     p_preventivas month if inrange(year, 2015, 2018), lcol(edkblue)) ///
       (line fem_p_preventivas month if inrange(year, 2015, 2018), lcol(ebblue)) ///					   
       (line mal_p_preventivas month if inrange(year, 2015, 2018), lcol(emidblue)), ///  
		xline(672, lpattern(dasp_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dasp_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#15, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, labsize(vsmall)) ///
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") title("") ///
		legend(label(1 "Total")) ///
		legend(label(2 "Female")) ///		
		legend(label(3 "Male")) ///
		note("Preventive procedures") ///
		name(main_p_preventives, replace)	
graph export "${graphs}/main_p_preventives.png", replace		
	
* prenatales por género
twoway (line     p_prenatales month if inrange(year, 2015, 2018), lcol(edkblue)) ///
       (line fem_p_prenatales month if inrange(year, 2015, 2018), lcol(ebblue)) ///				   
       (line mal_p_prenatales month if inrange(year, 2015, 2018), lcol(emidblue)), ///	   
		xline(672, lpattern(dasp_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dasp_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#15, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, labsize(vsmall)) ///
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") title("") ///
		legend(label(1 "Total")) ///
		legend(label(2 "Female")) ///		
		legend(label(3 "Male")) ///
		note("Prenatal procedures") ///
		name(main_p_prenatales, replace)		
graph export "${graphs}/main_p_prenatales.png", replace	
		
* Cronicas	
twoway (line p_cron_cancer         month if inrange(year, 2015, 2018), lcol(edkblue))  ///
       (line p_cron_renal          month if inrange(year, 2015, 2018), lcol(ebblue))   ///			
       (line p_cron_respiratoria   month if inrange(year, 2015, 2018), lcol(emidblue)) ///	   
       (line p_cron_cardiovascular month if inrange(year, 2015, 2018), lcol(midblue)   yaxis(2)), ///	   
		xline(672, lpattern(dasp_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dasp_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#15, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, labsize(vsmall)) ///
        ylabel(#6, labsize(vsmall) axis(2)) ///		
		legend(position(6)  col(4) order(1 2 3 4)) ///
		xtitle("")  ytitle("") ytitle("", axis(2)) title("") ///
		legend(label(1 "Cancer")) ///
		legend(label(2 "Renal")) ///		
		legend(label(3 "Respiratory")) ///
		legend(label(4 "Cardiovascular")) ///
		note("Left axis: Cancer, Renal, Respiratory" "Right axis: Cardiovascular") ///
		name(main_p_cronicas, replace)		
graph export "${graphs}/main_p_cronicas.png", replace			
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	

	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	