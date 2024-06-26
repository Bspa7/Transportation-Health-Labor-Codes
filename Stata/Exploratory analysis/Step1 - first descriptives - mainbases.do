

global root "D:\Steban Pineda\Documents\DIME\Transportation and health"
global results "${root}/Results_run_banrep"
global graphs_rips "${root}/outputs/figures/Descriptives_RIPS"
global graphs_pila "${root}/outputs/figures/Descriptives_PILA"
global aux_db  "${root}/Results_run_banrep/auxiliar_databases"


********************************************************************************
*                     ALL SET OF DESCRIPTIVES: RIPS
********************************************************************************

foreach x of newlist all main p30 p40 p2545 {
import excel "${root}/Results_run_banrep/20240604-try2_descriptives_rips.xlsx", sheet("`x'_collapse") firstrow clear
	gen month = mofd(month)
	format month %tm
	ren sum_* *
	collapse (sum) c_* h_* p_* u_*, by(year month) 	
	compress
	save "${aux_db}/`x'_collapse_total", replace


import excel "${root}/Results_run_banrep/20240604-try2_descriptives_rips.xlsx", sheet("`x'_collapse") firstrow clear
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
		
import excel "${root}/Results_run_banrep/20240604-try2_descriptives_rips.xlsx", sheet("`x'_collapse") firstrow clear
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

import excel "${root}/Results_run_banrep/20240604-try2_descriptives_rips.xlsx", sheet("`x'_id_mes") firstrow clear	
	gen month = mofd(month)
	format month %tm
	drop monthly
	sort month
	ren total total_registres
	compress
	save "${aux_db}/`x'_total_registres", replace	
	
import excel "${root}/Results_run_banrep/20240604-try2_descriptives_rips.xlsx", sheet("`x'_id_mes_modulo") firstrow clear
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
*   DESCRIPTIVES IN MAIN SAMPLE
**********************************************
		
use "${results}/rips_summarise_main", replace		

* Total de registros en todo RIPS
line total_registres month if inrange(year, 2013, 2018), lcol(midblue) ///
		xline(648, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#25, labsize(vsmall) grid angle(45)) ///
        ylabel(#10, format(%9.0fc) labsize(vsmall)) ///
		xtitle("")  ytitle("Total registres") title("") ///
		note("All different personabasicaid by month")
graph export "${graphs_rips}/main_01_serie.png", replace	

* Total de registros en los modulos
twoway (line total_c month if inrange(year, 2013, 2018), lcol(edkblue)),  ///
		xline(648, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#25, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, format(%9.0fc) labsize(vsmall)) ///	
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") title("Consultations") ///
		name(all_modules_total_c, replace)
		
twoway (line total_p month if inrange(year, 2013, 2018), lcol(ebblue)),  ///
		xline(648, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#25, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, format(%9.0fc) labsize(vsmall)) ///	
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") title("Procedures") ///
		name(all_modules_total_p, replace)
		
twoway (line total_h month if inrange(year, 2013, 2018), lcol(emidblue)),  ///
		xline(648, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#25, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, format(%9.0fc) labsize(vsmall)) ///	
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") title("Hospitalizations") ///
		name(all_modules_total_h, replace)
		
twoway (line total_u month if inrange(year, 2013, 2018), lcol(midblue)),  ///
		xline(648, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#25, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, format(%9.0fc) labsize(vsmall)) ///	
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") title("Emergencies") ///
		name(all_modules_total_u, replace)		
		
graph combine all_modules_total_c all_modules_total_p ///
			  all_modules_total_h all_modules_total_u, ///
			  c(2)
graph export "${graphs_rips}/main_modules.png", replace	
	
*** CONSULTAS ------------------------------------------------------------------
* preventivas por género
twoway (line     c_preventivas month if inrange(year, 2013, 2018), lcol(edkblue)) ///
       (line fem_c_preventivas month if inrange(year, 2013, 2018), lcol(ebblue)) ///					   
       (line mal_c_preventivas month if inrange(year, 2013, 2018), lcol(emidblue)), ///  
		xline(648, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#25, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, format(%9.0fc) labsize(vsmall)) ///
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") title("") ///
		legend(label(1 "Total")) ///
		legend(label(2 "Female")) ///		
		legend(label(3 "Male")) ///
		note("Preventive consultations") ///
		name(all_c_preventives, replace)	
graph export "${graphs_rips}/main_c_preventives.png", replace		
	
* prenatales por género
twoway (line     c_prenatales month if inrange(year, 2013, 2018), lcol(edkblue)) ///
       (line fem_c_prenatales month if inrange(year, 2013, 2018), lcol(ebblue)) ///				   
       (line mal_c_prenatales month if inrange(year, 2013, 2018), lcol(emidblue)), ///	   
		xline(648, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#25, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, format(%9.0fc) labsize(vsmall)) ///
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") title("") ///
		legend(label(1 "Total")) ///
		legend(label(2 "Female")) ///		
		legend(label(3 "Male")) ///
		note("Prenatal consultations") ///
		name(all_c_prenatales, replace)		
graph export "${graphs_rips}/main_c_prenatales.png", replace	
		
* Cronicas	
twoway (line c_cron_cancer         month if inrange(year, 2013, 2018), lcol(edkblue))  ///
       (line c_cron_renal          month if inrange(year, 2013, 2018), lcol(ebblue))   ///			
       (line c_cron_respiratoria   month if inrange(year, 2013, 2018), lcol(emidblue)) ///	   
       (line c_cron_cardiovascular month if inrange(year, 2013, 2018), lcol(midblue)   yaxis(2)), ///	   
		xline(648, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#25, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, format(%9.0fc) labsize(vsmall)) ///
		ylabel(200(2000)20000, format(%9.0fc) labsize(vsmall) axis(2)) ///
		legend(position(6)  col(4) order(1 2 3 4)) ///
		xtitle("")  ytitle("") ytitle("", axis(2)) title("") ///
		legend(label(1 "Cancer")) ///
		legend(label(2 "Renal")) ///		
		legend(label(3 "Respiratory")) ///
		legend(label(4 "Cardiovascular")) ///
		note("Left axis: Cancer, Renal, Respiratory" "Right axis: Cardiovascular") ///
		name(all_c_cronicas, replace)	
graph export "${graphs_rips}/main_c_cronicas.png", replace	
	
*** PROCEDIMIENTOS -------------------------------------------------------------	
* preventivas por género
twoway (line     p_preventivas month if inrange(year, 2013, 2018), lcol(edkblue)) ///
       (line fem_p_preventivas month if inrange(year, 2013, 2018), lcol(ebblue)) ///					   
       (line mal_p_preventivas month if inrange(year, 2013, 2018), lcol(emidblue)), ///  
		xline(648, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#25, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, format(%9.0fc) labsize(vsmall)) ///
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") title("") ///
		legend(label(1 "Total")) ///
		legend(label(2 "Female")) ///		
		legend(label(3 "Male")) ///
		note("Preventive procedures") ///
		name(all_p_preventives, replace)	
graph export "${graphs_rips}/main_p_preventives.png", replace		
	
* prenatales por género
twoway (line     p_prenatales month if inrange(year, 2013, 2018), lcol(edkblue)) ///
       (line fem_p_prenatales month if inrange(year, 2013, 2018), lcol(ebblue)) ///				   
       (line mal_p_prenatales month if inrange(year, 2013, 2018), lcol(emidblue)), ///	   
		xline(648, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#25, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, format(%9.0fc) labsize(vsmall)) ///
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") title("") ///
		legend(label(1 "Total")) ///
		legend(label(2 "Female")) ///		
		legend(label(3 "Male")) ///
		note("Prenatal procedures") ///
		name(all_p_prenatales, replace)		
graph export "${graphs_rips}/main_p_prenatales.png", replace	
		
* Cronicas	
twoway (line p_cron_cancer         month if inrange(year, 2013, 2018), lcol(edkblue))  ///
       (line p_cron_renal          month if inrange(year, 2013, 2018), lcol(ebblue))   ///			
       (line p_cron_respiratoria   month if inrange(year, 2013, 2018), lcol(emidblue)) ///	   
       (line p_cron_cardiovascular month if inrange(year, 2013, 2018), lcol(midblue)   yaxis(2)), ///	   
		xline(648, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#25, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, format(%9.0fc) labsize(vsmall)) ///
		ylabel(200(2000)15000, format(%9.0fc) labsize(vsmall) axis(2)) ///
		legend(position(6)  col(4) order(1 2 3 4)) ///
		xtitle("")  ytitle("") ytitle("", axis(2)) title("") ///
		legend(label(1 "Cancer")) ///
		legend(label(2 "Renal")) ///		
		legend(label(3 "Respiratory")) ///
		legend(label(4 "Cardiovascular")) ///
		note("Left axis: Cancer, Renal, Respiratory" "Right axis: Cardiovascular") ///
		name(all_p_cronicas, replace)		
graph export "${graphs_rips}/main_p_cronicas.png", replace		



foreach data of newlist p30 p40 p2545 {
	
use "${results}/rips_summarise_`data'", replace		

* Total de registros en todo RIPS
line total_registres month if inrange(year, 2013, 2018), lcol(midblue) ///
		xline(648, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#12, labsize(vsmall) grid angle(45)) ///
        ylabel(#10, format(%9.0fc) labsize(vsmall)) ///
		xtitle("")  ytitle("Total registres") title("") ///
		note("All different personabasicaid by month")
graph export "${graphs_rips}/`data'_01_serie.png", replace	

* Total de registros en los modulos
twoway (line total_c month if inrange(year, 2013, 2018), lcol(edkblue)),  ///
		xline(648, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#25, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, format(%9.0fc) labsize(vsmall)) ///	
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") title("Consultations") ///
		name(`data'_modules_total_c, replace)
		
twoway (line total_p month if inrange(year, 2013, 2018), lcol(ebblue)),  ///
		xline(648, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#25, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, format(%9.0fc) labsize(vsmall)) ///	
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") title("Procedures") ///
		name(`data'_modules_total_p, replace)
		
twoway (line total_h month if inrange(year, 2013, 2018), lcol(emidblue)),  ///
		xline(648, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#25, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, format(%9.0fc) labsize(vsmall)) ///	
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") title("Hospitalizations") ///
		name(`data'_modules_total_h, replace)
		
twoway (line total_u month if inrange(year, 2013, 2018), lcol(midblue)),  ///
		xline(648, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#25, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, format(%9.0fc) labsize(vsmall)) ///	
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") title("Emergencies") ///
		name(`data'_modules_total_u, replace)		
		
graph combine `data'_modules_total_c `data'_modules_total_p ///
			  `data'_modules_total_h `data'_modules_total_u, ///
			  c(2)
graph export "${graphs_rips}/`data'_modules.png", replace	
	
*** CONSULTAS ------------------------------------------------------------------
* preventivas por género
twoway (line     c_preventivas month if inrange(year, 2013, 2018), lcol(edkblue)) ///
       (line fem_c_preventivas month if inrange(year, 2013, 2018), lcol(ebblue)) ///					   
       (line mal_c_preventivas month if inrange(year, 2013, 2018), lcol(emidblue)), ///  
		xline(648, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#25, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, format(%9.0fc) labsize(vsmall)) ///
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") title("") ///
		legend(label(1 "Total")) ///
		legend(label(2 "Female")) ///		
		legend(label(3 "Male")) ///
		note("Preventive consultations") ///
		name(`data'_c_preventives, replace)	
graph export "${graphs_rips}/`data'_c_preventives.png", replace		
	
* prenatales por género
twoway (line     c_prenatales month if inrange(year, 2013, 2018), lcol(edkblue)) ///
       (line fem_c_prenatales month if inrange(year, 2013, 2018), lcol(ebblue)) ///				   
       (line mal_c_prenatales month if inrange(year, 2013, 2018), lcol(emidblue)), ///	   
		xline(648, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#25, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, format(%9.0fc) labsize(vsmall)) ///
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") title("") ///
		legend(label(1 "Total")) ///
		legend(label(2 "Female")) ///		
		legend(label(3 "Male")) ///
		note("Prenatal consultations") ///
		name(`data'_c_prenatales, replace)		
graph export "${graphs_rips}/`data'_c_prenatales.png", replace	
		
* Cronicas	
twoway (line c_cron_cancer         month if inrange(year, 2013, 2018), lcol(edkblue))  ///
       (line c_cron_renal          month if inrange(year, 2013, 2018), lcol(ebblue))   ///			
       (line c_cron_respiratoria   month if inrange(year, 2013, 2018), lcol(emidblue)) ///	   
       (line c_cron_cardiovascular month if inrange(year, 2013, 2018), lcol(midblue)   yaxis(2)), ///	   
		xline(648, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#25, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, format(%9.0fc) labsize(vsmall)) ///
		ylabel(#6, format(%9.0fc) labsize(vsmall) axis(2)) ///
		legend(position(6)  col(4) order(1 2 3 4)) ///
		xtitle("")  ytitle("") ytitle("", axis(2)) title("") ///
		legend(label(1 "Cancer")) ///
		legend(label(2 "Renal")) ///		
		legend(label(3 "Respiratory")) ///
		legend(label(4 "Cardiovascular")) ///
		note("Left axis: Cancer, Renal, Respiratory" "Right axis: Cardiovascular") ///
		name(`data'_c_cronicas, replace)	
graph export "${graphs_rips}/`data'_c_cronicas.png", replace	
	
*** PROCEDIMIENTOS -------------------------------------------------------------	
* preventivas por género
twoway (line     p_preventivas month if inrange(year, 2013, 2018), lcol(edkblue)) ///
       (line fem_p_preventivas month if inrange(year, 2013, 2018), lcol(ebblue)) ///					   
       (line mal_p_preventivas month if inrange(year, 2013, 2018), lcol(emidblue)), ///  
		xline(648, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#25, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, format(%9.0fc) labsize(vsmall)) ///
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") title("") ///
		legend(label(1 "Total")) ///
		legend(label(2 "Female")) ///		
		legend(label(3 "Male")) ///
		note("Preventive procedures") ///
		name(`data'_p_preventives, replace)	
graph export "${graphs_rips}/`data'_p_preventives.png", replace		
	
* prenatales por género
twoway (line     p_prenatales month if inrange(year, 2013, 2018), lcol(edkblue)) ///
       (line fem_p_prenatales month if inrange(year, 2013, 2018), lcol(ebblue)) ///				   
       (line mal_p_prenatales month if inrange(year, 2013, 2018), lcol(emidblue)), ///	   
		xline(648, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#25, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, format(%9.0fc) labsize(vsmall)) ///
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") title("") ///
		legend(label(1 "Total")) ///
		legend(label(2 "Female")) ///		
		legend(label(3 "Male")) ///
		note("Prenatal procedures") ///
		name(`data'_p_prenatales, replace)		
graph export "${graphs_rips}/`data'_p_prenatales.png", replace	
		
* Cronicas	
twoway (line p_cron_cancer         month if inrange(year, 2013, 2018), lcol(edkblue))  ///
       (line p_cron_renal          month if inrange(year, 2013, 2018), lcol(ebblue))   ///			
       (line p_cron_respiratoria   month if inrange(year, 2013, 2018), lcol(emidblue)) ///	   
       (line p_cron_cardiovascular month if inrange(year, 2013, 2018), lcol(midblue)   yaxis(2)), ///	   
		xline(648, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#25, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, format(%9.0fc) labsize(vsmall)) ///
		ylabel(#6, format(%9.0fc) labsize(vsmall) axis(2)) ///
		legend(position(6)  col(4) order(1 2 3 4)) ///
		xtitle("")  ytitle("") ytitle("", axis(2)) title("") ///
		legend(label(1 "Cancer")) ///
		legend(label(2 "Renal")) ///		
		legend(label(3 "Respiratory")) ///
		legend(label(4 "Cardiovascular")) ///
		note("Left axis: Cancer, Renal, Respiratory" "Right axis: Cardiovascular") ///
		name(`data'_p_cronicas, replace)		
graph export "${graphs_rips}/`data'_p_cronicas.png", replace			
	
}

********************************************************************************
*                     ALL SET OF DESCRIPTIVES: PILA
********************************************************************************

foreach x of newlist all main p30 p40 p2545 {

import excel "${root}/Results_run_banrep/20240604-try2_descriptives_pila.xlsx", sheet("`x'_pila_by_vars") firstrow clear
	gen year = year(month)
	gen month = mofd(month)
	drop monthly
	order year month
	format month %tm
	
	collapse (sum) total_pers, by(year month sexo)
 	drop if sexomode==.
	
		gen f_registros = total_pers if sexo==0
		gen m_registros = total_pers if sexo==1
		egen fem_registros = max(f_registros), by(month)
		egen mal_registros = max(m_registros), by(month)		
		drop f_registros m_registros  total_pers sexomode
		duplicates drop
		keep if inrange(year, 2009, 2020)
	save "${aux_db}/pila_`x'_gender", replace	
	

import excel "${root}/Results_run_banrep/20240604-try2_descriptives_pila.xlsx", sheet("`x'_pila_by_vars") firstrow clear
	gen year = year(month)
	gen month = mofd(month)
	drop monthly
	order year month
	format month %tm
	
	collapse (sum) total_pers, by(year month tipo_cotiz)
 	drop if tipo_cotiz==.
	
		gen cotizante_t1 = total_pers if tipo_cotiz==1
		gen cotizante_t2 = total_pers if tipo_cotiz==2
		gen cotizante_t3 = total_pers if tipo_cotiz==3
		
		egen tipo_cotizante_t1 = max(cotizante_t1), by(month)
		egen tipo_cotizante_t2 = max(cotizante_t2), by(month)		
		egen tipo_cotizante_t3 = max(cotizante_t3), by(month)				
		drop cotizante_t* total_pers tipo_cotizante
		duplicates drop
		keep if inrange(year, 2009, 2020)
	save "${aux_db}/pila_`x'_ctzte", replace		

	
import excel "${root}/Results_run_banrep/20240604-try2_descriptives_pila.xlsx", sheet("`x'_pila_by_vars") firstrow clear
	gen year = year(month)
	gen month = mofd(month)
	drop monthly
	order year month
	format month %tm
	
	collapse (sum) total_pers, by(year month grupo_edad)
 	drop if grupo_edad==.
	
		gen edad_g1 = total_pers if grupo_edad==1
		gen edad_g2 = total_pers if grupo_edad==2
		gen edad_g3 = total_pers if grupo_edad==3
		
		egen grupo_edad_g1 = max(edad_g1), by(month)
		egen grupo_edad_g2 = max(edad_g2), by(month)		
		egen grupo_edad_g3 = max(edad_g3), by(month)				
		drop edad* total_pers grupo_edad
		duplicates drop
		keep if inrange(year, 2009, 2020)		
	save "${aux_db}/pila_`x'_age", replace		
	
	
import excel "${root}/Results_run_banrep/20240604-try2_descriptives_pila.xlsx", sheet("`x'_pila_by_mes") firstrow clear
	gen year = year(month)
	gen month = mofd(month)
	drop monthly
	order year month
	format month %tm	
	keep if inrange(year, 2009, 2020)			
	
	merge 1:1 month using "${aux_db}/pila_`x'_gender", nogen
	merge 1:1 month using "${aux_db}/pila_`x'_ctzte", nogen	
	merge 1:1 month using "${aux_db}/pila_`x'_age", nogen		
	save "${results}/pila_summarise_`x'", replace		
	
}	
	
	
use "${results}/pila_summarise_all", replace			
	
line total_persona month if inrange(year, 2013, 2018), lcol(midblue) ///
		xline(648, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#25, labsize(vsmall) grid angle(45)) ///
        ylabel(#10, labsize(vsmall) format(%9.0fc)) ///
		xtitle("")  ytitle("") title("") ///
		note("All different personabasicaid by month")	///
		name(all_registers, replace)
graph export "${graphs_pila}/all_01_serie.png", replace		
		
twoway (line fem_registros month if inrange(year, 2013, 2018), lcol(midblue)) ///					   
       (line mal_registros month if inrange(year, 2013, 2018), lcol(emidblue)), ///  
		xline(648, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#25, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, labsize(vsmall) format(%9.0fc)) ///
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") title("") ///
		legend(label(1 "Female")) ///
		legend(label(2 "Male")) ///
		note("") ///
		name(all_gender, replace)
graph export "${graphs_pila}/all_gender.png", replace	
	
	
twoway (line tipo_cotizante_t1 month if inrange(year, 2013, 2018), lcol(red) yaxis(2)) ///
       (line tipo_cotizante_t2 month if inrange(year, 2013, 2018), lcol(ebblue)) ///				   
       (line tipo_cotizante_t3 month if inrange(year, 2013, 2018), lcol(emidblue)), ///	   
		xline(648, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#25, labsize(vsmall) grid angle(45)) ///
        ylabel(30000(5000)75000, labsize(vsmall) format(%9.0fc)) ///
		ylabel(200000(20000)400000, labsize(vsmall) format(%9.0fc) axis(2)) ///		
        legend(position(6)  col(4)) ///		
		xtitle("")  ytitle("") ytitle("", axis(2)) title("") ///
		legend(label(1 "Employees")) ///
		legend(label(2 "Self-employed")) ///		
		legend(label(3 "Other")) ///
		note("Left axis: Employees" "Right axis: Self-employed and others") ///
		name(all_cotizantes, replace)	
graph export "${graphs_pila}/all_employee.png", replace		

twoway (line grupo_edad_g2 month if inrange(year, 2013, 2018), lcol(red) yaxis(2)) ///
	   (line grupo_edad_g1 month if inrange(year, 2013, 2018), lcol(ebblue)) ///				   
       (line grupo_edad_g3 month if inrange(year, 2013, 2018), lcol(emidblue)), ///	   
		xline(648, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#25, labsize(vsmall) grid angle(45)) ///
        ylabel(10000(10000)100000, labsize(vsmall) format(%9.0fc)) ///
		ylabel(300000(10000)400000, labsize(vsmall) format(%9.0fc) axis(2)) ///		
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") ytitle("", axis(2)) title("") ///
		legend(label(1 "28-59 years old")) ///
		legend(label(2 "18-27 years old")) ///		
		legend(label(3 "60 years older")) ///
		legend(order(2 1 3)) ///
		note("") ///
		name(all_edad, replace)	
graph export "${graphs_pila}/all_ages.png", replace		
	
	
	
use "${results}/pila_summarise_main", replace			
	
line total_persona month if inrange(year, 2013, 2018), lcol(midblue) ///
		xline(648, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#25, labsize(vsmall) grid angle(45)) ///
        ylabel(#10, labsize(vsmall) format(%9.0fc)) ///
		xtitle("")  ytitle("") title("") ///
		note("All different personabasicaid by month")	///
		name(main_registers, replace)
graph export "${graphs_pila}/main_01_serie.png", replace		
		
twoway (line fem_registros month if inrange(year, 2013, 2018), lcol(midblue)) ///					   
       (line mal_registros month if inrange(year, 2013, 2018), lcol(emidblue)), ///  
		xline(648, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#25, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, labsize(vsmall) format(%9.0fc)) ///
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") title("") ///
		legend(label(1 "Female")) ///
		legend(label(2 "Male")) ///
		note("") ///
		name(main_gender, replace)
graph export "${graphs_pila}/main_gender.png", replace	
	
	
twoway (line tipo_cotizante_t1 month if inrange(year, 2013, 2018), lcol(red) yaxis(2)) ///
       (line tipo_cotizante_t2 month if inrange(year, 2013, 2018), lcol(ebblue)) ///				   
       (line tipo_cotizante_t3 month if inrange(year, 2013, 2018), lcol(emidblue)), ///	   
		xline(648, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#25, labsize(vsmall) grid angle(45)) ///
        ylabel(30000(5000)75000, labsize(vsmall) format(%9.0fc)) ///
		ylabel(200000(20000)400000, labsize(vsmall) format(%9.0fc) axis(2)) ///		
        legend(position(6)  col(4)) ///		
		xtitle("")  ytitle("") ytitle("", axis(2)) title("") ///
		legend(label(1 "Employees")) ///
		legend(label(2 "Self-employed")) ///		
		legend(label(3 "Other")) ///
		note("Left axis: Employees" "Right axis: Self-employed and others") ///
		name(main_cotizantes, replace)	
graph export "${graphs_pila}/main_employee.png", replace		

twoway (line grupo_edad_g2 month if inrange(year, 2013, 2018), lcol(red) yaxis(2)) ///
	   (line grupo_edad_g1 month if inrange(year, 2013, 2018), lcol(ebblue)) ///				   
       (line grupo_edad_g3 month if inrange(year, 2013, 2018), lcol(emidblue)), ///	   
		xline(648, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#25, labsize(vsmall) grid angle(45)) ///
        ylabel(10000(10000)100000, labsize(vsmall) format(%9.0fc)) ///
		ylabel(300000(10000)400000, labsize(vsmall) format(%9.0fc) axis(2)) ///		
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") ytitle("", axis(2)) title("") ///
		legend(label(1 "28-59 years old")) ///
		legend(label(2 "18-27 years old")) ///		
		legend(label(3 "60 years older")) ///
		legend(order(2 1 3)) ///
		note("") ///
		name(main_edad, replace)	
graph export "${graphs_pila}/main_ages.png", replace			
	
	
foreach data of newlist p30 p40 p2545 {
	dis in red "`data'"

use "${results}/pila_summarise_`data'", replace			
	
line total_persona month if inrange(year, 2013, 2018), lcol(midblue) ///
		xline(648, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#25, labsize(vsmall) grid angle(45)) ///
        ylabel(#10, labsize(vsmall) format(%9.0fc)) ///
		xtitle("")  ytitle("") title("") ///
		note("All different personabasicaid by month")	///
		name(`data'_registers, replace)
graph export "${graphs_pila}/`data'_01_serie.png", replace		
		
twoway (line fem_registros month if inrange(year, 2013, 2018), lcol(midblue)) ///					   
       (line mal_registros month if inrange(year, 2013, 2018), lcol(emidblue)), ///  
		xline(648, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#25, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, labsize(vsmall) format(%9.0fc)) ///
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") title("") ///
		legend(label(1 "Female")) ///
		legend(label(2 "Male")) ///
		note("") ///
		name(`data'_gender, replace)
graph export "${graphs_pila}/`data'_gender.png", replace	
	
	
twoway (line tipo_cotizante_t1 month if inrange(year, 2013, 2018), lcol(red) yaxis(2)) ///
       (line tipo_cotizante_t2 month if inrange(year, 2013, 2018), lcol(ebblue)) ///				   
       (line tipo_cotizante_t3 month if inrange(year, 2013, 2018), lcol(emidblue)), ///	   
		xline(648, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#25, labsize(vsmall) grid angle(45)) ///
        ylabel(#15, labsize(vsmall) format(%9.0fc)) ///
		ylabel(#6, labsize(vsmall) format(%9.0fc) axis(2)) ///		
        legend(position(6)  col(4)) ///		
		xtitle("")  ytitle("") ytitle("", axis(2)) title("") ///
		legend(label(1 "Employees")) ///
		legend(label(2 "Self-employed")) ///		
		legend(label(3 "Other")) ///
		note("Left axis: Employees" "Right axis: Self-employed and others") ///
		name(`data'_cotizantes, replace)	
graph export "${graphs_pila}/`data'_employee.png", replace		

twoway (line grupo_edad_g2 month if inrange(year, 2013, 2018), lcol(red) yaxis(2)) ///
	   (line grupo_edad_g1 month if inrange(year, 2013, 2018), lcol(ebblue)) ///				   
       (line grupo_edad_g3 month if inrange(year, 2013, 2018), lcol(emidblue)), ///	   
		xline(648, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#25, labsize(vsmall) grid angle(45)) ///
        ylabel(#15, labsize(vsmall) format(%9.0fc)) ///
		ylabel(#6, labsize(vsmall) format(%9.0fc) axis(2)) ///		
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") ytitle("", axis(2)) title("") ///
		legend(label(1 "28-59 years old")) ///
		legend(label(2 "18-27 years old")) ///		
		legend(label(3 "60 years older")) ///
		legend(order(2 1 3)) ///
		note("") ///
		name(`data'_edad, replace)	
graph export "${graphs_pila}/`data'_ages.png", replace	
}	
	











