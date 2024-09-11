

global root "D:\Steban Pineda\Documents\DIME\Transportation and health"
global results "${root}/Results_run_banrep"
global graphs_rips "${root}/outputs/figures/Descriptives_RIPS"
global graphs_pila "${root}/outputs/figures/Descriptives_PILA"
global aux_db  "${root}/Results_run_banrep/auxiliar_databases"

**# First set of descriptives
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
		xline(685, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#25, labsize(vsmall) grid angle(45)) ///
        ylabel(#10, format(%9.0fc) labsize(vsmall)) ///
		xtitle("")  ytitle("Total registres") title("") ///
		note("All different personabasicaid by month")
graph export "${graphs_rips}/main_01_serie.png", replace	

* Total de registros en los modulos
twoway (line total_c month if inrange(year, 2013, 2018), lcol(edkblue)),  ///
		xline(648, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(685, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#25, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, format(%9.0fc) labsize(vsmall)) ///	
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") title("Consultations") ///
		name(all_modules_total_c, replace)
		
twoway (line total_p month if inrange(year, 2013, 2018), lcol(ebblue)),  ///
		xline(648, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(685, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#25, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, format(%9.0fc) labsize(vsmall)) ///	
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") title("Procedures") ///
		name(all_modules_total_p, replace)
		
replace total_h=. if year<=2014		
		
twoway (line total_h month if inrange(year, 2013, 2018), lcol(emidblue)),  ///
		xline(648, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(685, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#25, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, format(%9.0fc) labsize(vsmall)) ///	
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") title("Hospitalizations") ///
		name(all_modules_total_h, replace)
		
twoway (line total_u month if inrange(year, 2013, 2018), lcol(midblue)),  ///
		xline(648, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(685, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
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
		xline(685, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
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
		xline(685, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
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
		xline(685, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
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
		xline(685, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
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
		xline(685, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
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
		xline(685, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
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
		xline(685, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#12, labsize(vsmall) grid angle(45)) ///
        ylabel(#10, format(%9.0fc) labsize(vsmall)) ///
		xtitle("")  ytitle("Total registres") title("") ///
		note("All different personabasicaid by month")
graph export "${graphs_rips}/`data'_01_serie.png", replace	

* Total de registros en los modulos
twoway (line total_c month if inrange(year, 2013, 2018), lcol(edkblue)),  ///
		xline(648, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(685, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#25, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, format(%9.0fc) labsize(vsmall)) ///	
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") title("Consultations") ///
		name(`data'_modules_total_c, replace)
		
twoway (line total_p month if inrange(year, 2013, 2018), lcol(ebblue)),  ///
		xline(648, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(685, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#25, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, format(%9.0fc) labsize(vsmall)) ///	
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") title("Procedures") ///
		name(`data'_modules_total_p, replace)
		
twoway (line total_h month if inrange(year, 2013, 2018), lcol(emidblue)),  ///
		xline(648, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(685, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#25, labsize(vsmall) grid angle(45)) ///
        ylabel(#6, format(%9.0fc) labsize(vsmall)) ///	
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") title("Hospitalizations") ///
		name(`data'_modules_total_h, replace)
		
twoway (line total_u month if inrange(year, 2013, 2018), lcol(midblue)),  ///
		xline(648, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(685, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
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
		xline(685, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
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
		xline(685, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
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
		xline(685, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
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
		xline(685, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
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
		xline(685, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
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
		xline(685, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
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
		xline(685, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#25, labsize(vsmall) grid angle(45)) ///
        ylabel(#10, labsize(vsmall) format(%9.0fc)) ///
		xtitle("")  ytitle("") title("") ///
		note("All different personabasicaid by month")	///
		name(all_registers, replace)
graph export "${graphs_pila}/all_01_serie.png", replace		
		
twoway (line fem_registros month if inrange(year, 2013, 2018), lcol(midblue)) ///					   
       (line mal_registros month if inrange(year, 2013, 2018), lcol(emidblue)), ///  
		xline(648, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(685, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
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
		xline(685, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
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
		xline(685, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
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
		xline(685, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#25, labsize(vsmall) grid angle(45)) ///
        ylabel(#10, labsize(vsmall) format(%9.0fc)) ///
		xtitle("")  ytitle("") title("") ///
		note("All different personabasicaid by month")	///
		name(main_registers, replace)
graph export "${graphs_pila}/main_01_serie.png", replace		
		
twoway (line fem_registros month if inrange(year, 2013, 2018), lcol(midblue)) ///					   
       (line mal_registros month if inrange(year, 2013, 2018), lcol(emidblue)), ///  
		xline(648, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(685, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
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
		xline(685, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
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
		xline(685, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
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
		xline(685, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#25, labsize(vsmall) grid angle(45)) ///
        ylabel(#10, labsize(vsmall) format(%9.0fc)) ///
		xtitle("")  ytitle("") title("") ///
		note("All different personabasicaid by month")	///
		name(`data'_registers, replace)
graph export "${graphs_pila}/`data'_01_serie.png", replace		
		
twoway (line fem_registros month if inrange(year, 2013, 2018), lcol(midblue)) ///					   
       (line mal_registros month if inrange(year, 2013, 2018), lcol(emidblue)), ///  
		xline(648, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		xline(685, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
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
		xline(685, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
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
		xline(685, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
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
	

**# Aug-14 | General set of descriptives	
	
********************************************************************************
*  New results: Global descriptives (Comments of Sveta and Guadalupe) Aug-14
********************************************************************************

global figures "${graphs_rips}/20240822-stats"
import excel "${root}\outputs\20240814-gnral_stats.xlsx", firstrow clear

* Correcting and error in the gender variable names
	ren *female *male1
	ren *male *female
	ren *male1 *male
	ren *_f  *_m1
	ren *_m  *_f
	ren *_m1 *_m

* Creating programs to some figures --------------------------------------------
cap program drop line_plot_total
program define line_plot_total
args variable color titulo

twoway (line `variable' month, lcol(`color')),  ///
        xlabel(#35, labsize(vsmall) grid angle(45)) ///
        ylabel(#10, labsize(vsmall) format(%9.0fc)) ///
        xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") title("`titulo'") ///
		note("") name(`variable'_t, replace)
	
twoway (line `variable' month if inrange(month, 675,699), lcol(`color')),  ///
        xlabel(#25, labsize(vsmall) grid angle(45)) ///
        ylabel(#10, labsize(vsmall) format(%9.0fc)) ///
        xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") title("`titulo'") ///
		note("") name(`variable'_z, replace)
	
	
end

cap program drop line_gender_total
program define line_gender_total
args var_female var_male name titulo	
twoway (line `var_female' month, lcol(midblue)) ///					   
       (line `var_male'   month, lcol(emidblue)), ///  
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#30, labsize(vsmall) grid angle(45)) ///
        ylabel(#10, labsize(vsmall) format(%9.2fc)) ///
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("Percentage (%)") title("`titulo'") ///
		legend(label(1 "Female")) ///
		legend(label(2 "Male")) ///
		note("") name(`name'_t, replace)	
		
twoway (line `var_female' month if inrange(month, 675,699), lcol(midblue)) ///					   
       (line `var_male'   month if inrange(month, 675,699), lcol(emidblue)), ///  
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#25, labsize(vsmall) grid angle(45)) ///
        ylabel(#10, labsize(vsmall) format(%9.2fc)) ///
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("Percentage (%)") title("`titulo'") ///
		legend(label(1 "Female")) ///
		legend(label(2 "Male")) ///
		note("") name(`name'_z, replace)		
		
end	
		
cap program drop line_plot_score_zoom
program define line_plot_score_zoom
args variable color titulo

twoway (line `variable' month if inrange(month, 675,699), lcol(`color')),  ///
        xlabel(#8, /*labsize(vsmall)*/ grid angle(45)) ///
        ylabel(#10, /*labsize(vsmall)*/ format(%9.3fc)) ///
        xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") title("Score range: `titulo'") ///
		note("") name(`variable'_z, replace)
	
end
	
	
* Generating variables that I'll use -------------------------------------------
* Monthly and auxiliar variables
	ren *4245* *4045*
	gen    month = mofd(monthly_date)
	format month %tm
	order  month, after(monthly_date)
	gen x = month

* Main variables by gender	
foreach gender of newlist female male {

dis in red "Creating variables for: `gender'"
	gen p_visitas_`gender'= (n_visitas_rips_`gender'/n_visitas_rips_total)*100      
	gen p_consul_`gender'= (n_consultas_`gender'/n_consultas_total)*100         
	gen p_hospit_`gender'= (n_hospitalizaciones_`gender'/n_hospitalizaciones_total)*100 
	gen p_proced_`gender'= (n_procedimientos_`gender'/n_procedimientos_total)*100    
	gen p_urgenc_`gender'= (n_urgencias_`gender'/n_urgencias_total)*100         
	gen p_preven_`gender'= (c_preven_`gender'/c_preven_total)*100            
	gen p_prenat_`gender'= (c_prenat_`gender'/c_prenat_total)*100         
	
}
* All Variables by score
foreach score of numlist 1520 2025 2530 3035 3540 4045 {	

dis in red " Working on: `score'"
	gen visita_rips_s`score'   = (n_visitas_rips_s`score'/n_visitas_rips_total)
	gen visita_rips_m_s`score' = (n_visitas_rips_s`score'_m/n_visitas_rips_total)                         
	gen visita_rips_f_s`score' = (n_visitas_rips_s`score'_f/n_visitas_rips_total)                        
	gen consul_s`score'        = (n_consultas_s`score'/n_consultas_total)                 
	gen consul_m_s`score'      = (n_consultas_s`score'_m/n_consultas_total)                  
	gen consul_f_s`score'      = (n_consultas_s`score'_f/n_consultas_total)                
	gen hospit_s`score'        = (n_hospitalizaciones_s`score'/n_hospitalizaciones_total)    
	gen hospit_m_s`score'      = (n_hospitalizaciones_s`score'_m/n_hospitalizaciones_total)  
	gen hospit_f_s`score'      = (n_hospitalizaciones_s`score'_f/n_hospitalizaciones_total)  
	gen proced_s`score'        = (n_procedimientos_s`score'/n_procedimientos_total)       
	gen proced_m_s`score'      = (n_procedimientos_s`score'_m/n_procedimientos_total)      
	gen proced_f_s`score'      = (n_procedimientos_s`score'_f/n_procedimientos_total)     
	gen urgenc_s`score'        = (n_urgencias_s`score'/n_urgencias_total)                 
	gen urgenc_m_s`score'      = (n_urgencias_s`score'_m/n_urgencias_total)               
	gen urgenc_f_s`score'      = (n_urgencias_s`score'_f/n_urgencias_total)                 
	gen preven_s`score'        = (c_preven_s`score'/c_preven_total)                          
	gen preven_m_s`score'      = (c_preven_s`score'_m/c_preven_total)                        
	gen preven_f_s`score'      = (c_preven_s`score'_f/c_preven_total)                        
	gen prenat_s`score'        = (c_prenat_s`score'/c_prenat_total)                             
	gen prenat_m_s`score'      = (c_prenat_s`score'_m/c_prenat_total)                           
	gen prenat_f_s`score'      = (c_prenat_s`score'_f/c_prenat_total)                           
}	

foreach var of newlist visita_rips_s visita_rips_m_s visita_rips_f_s consul_s consul_m_s consul_f_s hospit_s hospit_m_s hospit_f_s proced_s proced_m_s proced_f_s urgenc_s urgenc_m_s urgenc_f_s preven_s preven_m_s preven_f_s prenat_s prenat_m_s prenat_f_s {

dis in red "`var'"	
	gen p1_`var' = `var'1520
	gen p2_`var' = `var'1520+`var'2025	
	gen p3_`var' = `var'1520+`var'2025+`var'2530	
	gen p4_`var' = `var'1520+`var'2025+`var'2530+`var'3035	
	gen p5_`var' = `var'1520+`var'2025+`var'2530+`var'3035+`var'3540
	gen p6_`var' = `var'1520+`var'2025+`var'2530+`var'3035+`var'3540+`var'4045	

	
}
gen zero = 0


* Making figures ---------------------------------------------------------------

* General series
	line_plot_total n_visitas_rips_total      blue
	line_plot_total n_consultas_total         blue Consultations
	line_plot_total n_hospitalizaciones_total blue Hospitalizations
	line_plot_total n_procedimientos_total    blue Procedures
	line_plot_total n_urgencias_total         blue Emergencies
	line_plot_total c_preven_total            blue Preventive
	line_plot_total c_prenat_total            blue Prenatal

	graph combine n_consultas_total_t n_hospitalizaciones_total_t ///
	      n_procedimientos_total_t n_urgencias_total_t, name(total_modules, replace)
	graph combine c_preven_total_t c_prenat_total_t, c(1) name(total_consultations, replace)
		
	graph combine n_consultas_total_z n_hospitalizaciones_total_z ///
	      n_procedimientos_total_z n_urgencias_total_z, name(zoom_modules, replace)
	graph combine c_preven_total_z c_prenat_total_z, c(1) name(zoom_consultations, replace)

	graph export "${figures}/total_rips.png",         name(n_visitas_rips_total_t) replace
	graph export "${figures}/total_rips_zoom.png",    name(n_visitas_rips_total_z) replace		
	graph export "${figures}/total_modules.png",      name(total_modules) replace	
	graph export "${figures}/total_modules_zoom.png", name(zoom_modules) replace	
	graph export "${figures}/total_consultations.png", name(total_consultations) replace
	graph export "${figures}/total_consultations_zoom.png", name(zoom_consultations)  replace	
	
	graph close *	
	

* General series by gender percentage
	line_gender_total p_visitas_female p_visitas_male gender_total
	line_gender_total p_consul_female  p_consul_male  gender_consul Consultations   	
	line_gender_total p_hospit_female  p_hospit_male  gender_hospit Hospitalizations	
	line_gender_total p_proced_female  p_proced_male  gender_proced Procedures      	
	line_gender_total p_urgenc_female  p_urgenc_male  gender_urgenc Emergencies     	
	line_gender_total p_preven_female  p_preven_male  gender_preven Preventive      
	line_plot_total p_prenat_female  midblue
	
	graph combine gender_consul_t gender_hospit_t gender_proced_t ///
	              gender_urgenc_t, name(total_g_modules, replace)
	graph combine gender_consul_z gender_hospit_z gender_proced_z ///
	              gender_urgenc_z, name(zoom_g_modules, replace)
								  
	graph close   gender_consul_t gender_hospit_t gender_proced_t /// 
	              gender_urgenc_t gender_consul_z gender_hospit_z ///
				  gender_proced_z gender_urgenc_z 
				  
	graph export "${figures}/gender_total.png" ,name(gender_total_t)
	graph export "${figures}/gender_zoom.png" ,name(gender_total_z)
	graph export "${figures}/gender_modules.png" ,name(total_g_modules)
	graph export "${figures}/gender_modules_zoom.png" ,name(zoom_g_modules)  
	graph export "${figures}/gender_preven_total.png" ,name(gender_preven_t)
	graph export "${figures}/gender_prenat_total.png" ,name(p_prenat_female_t)
	graph export "${figures}/gender_preven_zoom.png" ,name(gender_preven_z)
	graph export "${figures}/gender_prenat_zoom.png" ,name(p_prenat_female_z)

	graph close *	
				
				
/*
twoway rarea zero p_consul_female month, fcolor(midblue%50) lcolor(gray%1)   /// 
    || rarea p_consul_female yy month, fcolor(emidblue%50) lcolor(gray%1) ///
    ,  legend(order(1 "{fontface Times New Roman: Female}" ///
	                 2 "{fontface Times New Roman: Male}")) /// 
    ytitle("{fontface Times New Roman: Percentage (%)}") ///
    xtitle("{fontface Times New Roman: }") ///	
	xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
	legend(pos(6) c(2))	///			 
	graphr(c(white))				  
*/	

		
* General series by score percentage using time lines		
foreach var of newlist visita_rips visita_rips_m visita_rips_f consul consul_m consul_f hospit hospit_m hospit_f proced proced_m proced_f urgenc urgenc_m urgenc_f preven preven_m preven_f prenat prenat_m prenat_f {
	
	line_plot_score_zoom `var'_s1520 blue 15-20
	line_plot_score_zoom `var'_s2025 blue 20-25
	line_plot_score_zoom `var'_s2530 blue 25-30.56
	line_plot_score_zoom `var'_s3035 blue 30.56-35
	line_plot_score_zoom `var'_s3540 blue 35-40
	line_plot_score_zoom `var'_s4045 blue 40-45
		
	graph combine `var'_s1520_z `var'_s2025_z `var'_s2530_z `var'_s3035_z /// 
				  `var'_s3540_z `var'_s4045_z, name(`var', replace)		
	graph close `var'_s1520_z `var'_s2025_z `var'_s2530_z `var'_s3035_z  ///
				`var'_s3540_z `var'_s4045_z			
				
	graph export "${figures}/score_lines_`var'.png" ,name(`var') replace				
	graph close `var'				
}

foreach var of newlist visita_rips visita_rips_m visita_rips_f consul consul_m consul_f hospit hospit_m hospit_f proced proced_m proced_f urgenc urgenc_m urgenc_f preven preven_m preven_f prenat prenat_m prenat_f {
	
*	line_plot_score_zoom `var'_s1520 blue 15-20
	line_plot_score_zoom `var'_s2025 blue 20-25
	line_plot_score_zoom `var'_s2530 blue 25-30.56
	line_plot_score_zoom `var'_s3035 blue 30.56-35
	line_plot_score_zoom `var'_s3540 blue 35-40
	line_plot_score_zoom `var'_s4045 blue 40-45
		
	graph combine  `var'_s2025_z `var'_s2530_z `var'_s3035_z /// 
				  `var'_s3540_z `var'_s4045_z, name(`var', replace)	ycommon	
	graph close  `var'_s2025_z `var'_s2530_z `var'_s3035_z  ///
				`var'_s3540_z `var'_s4045_z			
				
	graph export "${figures}/yc_score_lines_`var'.png" ,name(`var') replace				
	graph close `var'				
}






* General series by score percentage using bar figures		

foreach var of newlist visita_rips visita_rips_m visita_rips_f consul consul_m consul_f hospit hospit_m hospit_f proced proced_m proced_f urgenc urgenc_m urgenc_f preven preven_m preven_f prenat prenat_m prenat_f {
			
twoway (bar p6_`var'_s month if inrange(month, 675,699), barwidth(0.7) color(ebg)) ///
       (bar p5_`var'_s month if inrange(month, 675,699), barwidth(0.7) color(ebblue)) ///
       (bar p4_`var'_s month if inrange(month, 675,699), barwidth(0.7) color(edkblue)) ///
       (bar p3_`var'_s month if inrange(month, 675,699), barwidth(0.7) color(eltgreen)) ///
       (bar p2_`var'_s month if inrange(month, 675,699), barwidth(0.7) color(olive_teal)) ///
       (bar p1_`var'_s month if inrange(month, 675,699), barwidth(0.7) color(ltblue)), ///
       ytitle("{fontface Times New Roman: Proportion Relative to Total by Score}") ///
       xtitle("{fontface Times New Roman: }") ///
       legend(order(6 "{fontface Times New Roman: Score range: 15-20}" ///
                    5 "{fontface Times New Roman: Score range: 20-25}" ///
                    4 "{fontface Times New Roman: Score range: 25-30}" ///
                    3 "{fontface Times New Roman: Score range: 30-35}" ///
                    2 "{fontface Times New Roman: Score range: 35-40}" ///
                    1 "{fontface Times New Roman: Score range: 40-45}")) ///
	   xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
	   ylabel(#10, format(%9.3fc)) ///
	   xlabel(#25, labsize(vsmall) grid angle(45)) ///		
	   legend(pos(6) c(3))	///	
	   graphr(c(white))	///
	   name(`var', replace)		
	   
	graph export "${figures}/score_bar_`var'.png" ,name(`var')				
	graph close `var'				
	   
	   
}		
		
		
		
**# Sep-09 | Descriptives	
	
********************************************************************************
*  New results: Global descriptives (Comments of Sveta and Guadalupe) Aug-14
********************************************************************************
global figures "${graphs_rips}/20240822-stats"
import excel "${root}\outputs\20240905-stats_set1_step5.parquet.xlsx", firstrow clear		

		
* Creating programs to some figures --------------------------------------------
cap program drop line_plot_total
program define line_plot_total
args variable color titulo

twoway (line `variable' month, lcol(`color')),  ///
        xlabel(#13, labsize(small) grid angle(45)) ///
        ylabel(#10, labsize(small) format(%9.0fc)) ///
        xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") title("`titulo'") ///
		note("") name(`variable'_t, replace)
	
twoway (line `variable' month if inrange(month, 675,699), lcol(`color')),  ///
        xlabel(#8, /*labsize(vsmall)*/ grid angle(45)) ///
        ylabel(#10, labsize(small) format(%9.0fc)) ///
        xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") title("`titulo'") ///
		note("") name(`variable'_z, replace)
	
	
end

cap program drop line_gender_total
program define line_gender_total
args var_female var_male name titulo	
twoway (line `var_female' month, lcol(midblue)) ///					   
       (line `var_male'   month, lcol(emidblue)), ///  
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#13, labsize(small) grid angle(45)) ///
        ylabel(#10, labsize(small) format(%9.2fc)) ///
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("Percentage (%)") title("`titulo'") ///
		legend(label(1 "Female")) ///
		legend(label(2 "Male")) ///
		note("") name(`name'_t, replace)	
		
twoway (line `var_female' month if inrange(month, 675,699), lcol(midblue)) ///					   
       (line `var_male'   month if inrange(month, 675,699), lcol(emidblue)), ///  
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#8, /*labsize(vsmall)*/ grid angle(45)) ///
        ylabel(#10, labsize(small) format(%9.2fc)) ///
        legend(position(6)  col(4)) ///
		xtitle("")  ytitle("Percentage (%)") title("`titulo'") ///
		legend(label(1 "Female")) ///
		legend(label(2 "Male")) ///
		note("") name(`name'_z, replace)		
		
end	
		
cap program drop line_plot_score_zoom
program define line_plot_score_zoom
args variable color titulo

twoway (line `variable' month if inrange(month, 675,699), lcol(`color')),  ///
        xlabel(#8, labsize(small) grid angle(45)) ///
        ylabel(#10, /*labsize(vsmall)*/ format(%9.3fc)) ///
        xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black)) ///
		legend(position(6)  col(4)) ///
		xtitle("")  ytitle("") title("Score range: `titulo'") ///
		note("") name(`variable'_z, replace)
	
end
	
	
* Generating variables that I'll use -------------------------------------------
* Monthly and auxiliar variables
	ren *4245* *4045*
	gen    month = mofd(monthly_date)
	format month %tm
	order  month, after(monthly_date)
	gen x = month

* Main variables by gender	
foreach gender of newlist female male {

dis in red "Creating variables for: `gender'"
	gen p_visitas_`gender'= (n_visitas_rips_`gender'/n_visitas_rips_total)*100      
	gen p_consul_`gender'= (n_consultas_`gender'/n_consultas_total)*100         
	gen p_hospit_`gender'= (n_hospitalizaciones_`gender'/n_hospitalizaciones_total)*100 
	gen p_proced_`gender'= (n_procedimientos_`gender'/n_procedimientos_total)*100    
	gen p_urgenc_`gender'= (n_urgencias_`gender'/n_urgencias_total)*100         
	gen p_preven_`gender'= (c_preven_`gender'/c_preven_total)*100            
	gen p_prenat_`gender'= (c_prenat_`gender'/c_prenat_total)*100         
	
}
* All Variables by score
foreach score of numlist 1520 2025 2530 3035 3540 4045 {	

dis in red " Working on: `score'"
	gen visita_rips_s`score'   = (n_visitas_rips_s`score'/n_visitas_rips_total)
	gen visita_rips_m_s`score' = (n_visitas_rips_s`score'_m/n_visitas_rips_total)                         
	gen visita_rips_f_s`score' = (n_visitas_rips_s`score'_f/n_visitas_rips_total)                        
	gen consul_s`score'        = (n_consultas_s`score'/n_consultas_total)                 
	gen consul_m_s`score'      = (n_consultas_s`score'_m/n_consultas_total)                  
	gen consul_f_s`score'      = (n_consultas_s`score'_f/n_consultas_total)                
	gen hospit_s`score'        = (n_hospitalizaciones_s`score'/n_hospitalizaciones_total)    
	gen hospit_m_s`score'      = (n_hospitalizaciones_s`score'_m/n_hospitalizaciones_total)  
	gen hospit_f_s`score'      = (n_hospitalizaciones_s`score'_f/n_hospitalizaciones_total)  
	gen proced_s`score'        = (n_procedimientos_s`score'/n_procedimientos_total)       
	gen proced_m_s`score'      = (n_procedimientos_s`score'_m/n_procedimientos_total)      
	gen proced_f_s`score'      = (n_procedimientos_s`score'_f/n_procedimientos_total)     
	gen urgenc_s`score'        = (n_urgencias_s`score'/n_urgencias_total)                 
	gen urgenc_m_s`score'      = (n_urgencias_s`score'_m/n_urgencias_total)               
	gen urgenc_f_s`score'      = (n_urgencias_s`score'_f/n_urgencias_total)                 
	gen preven_s`score'        = (c_preven_s`score'/c_preven_total)                          
	gen preven_m_s`score'      = (c_preven_s`score'_m/c_preven_total)                        
	gen preven_f_s`score'      = (c_preven_s`score'_f/c_preven_total)                        
	gen prenat_s`score'        = (c_prenat_s`score'/c_prenat_total)                             
	gen prenat_m_s`score'      = (c_prenat_s`score'_m/c_prenat_total)                           
	gen prenat_f_s`score'      = (c_prenat_s`score'_f/c_prenat_total)                           
}	

foreach var of newlist visita_rips_s visita_rips_m_s visita_rips_f_s consul_s consul_m_s consul_f_s hospit_s hospit_m_s hospit_f_s proced_s proced_m_s proced_f_s urgenc_s urgenc_m_s urgenc_f_s preven_s preven_m_s preven_f_s prenat_s prenat_m_s prenat_f_s {

dis in red "`var'"	
	gen p1_`var' = `var'1520
	gen p2_`var' = `var'1520+`var'2025	
	gen p3_`var' = `var'1520+`var'2025+`var'2530	
	gen p4_`var' = `var'1520+`var'2025+`var'2530+`var'3035	
	gen p5_`var' = `var'1520+`var'2025+`var'2530+`var'3035+`var'3540
	gen p6_`var' = `var'1520+`var'2025+`var'2530+`var'3035+`var'3540+`var'4045	

	
}
gen zero = 0


* Making figures ---------------------------------------------------------------

* General series
	line_plot_total n_visitas_rips_total      blue
	line_plot_total n_consultas_total         blue Consultations
	line_plot_total n_hospitalizaciones_total blue Hospitalizations
	line_plot_total n_procedimientos_total    blue Procedures
	line_plot_total n_urgencias_total         blue Emergencies
	line_plot_total c_preven_total            blue Preventive
	line_plot_total c_prenat_total            blue Prenatal

	graph combine n_consultas_total_t n_hospitalizaciones_total_t ///
	      n_procedimientos_total_t n_urgencias_total_t, name(total_modules, replace)
	graph combine c_preven_total_t c_prenat_total_t, c(1) name(total_consultations, replace)
		
	graph combine n_consultas_total_z n_hospitalizaciones_total_z ///
	      n_procedimientos_total_z n_urgencias_total_z, name(zoom_modules, replace)
	graph combine c_preven_total_z c_prenat_total_z, c(1) name(zoom_consultations, replace)

	graph export "${figures}/total_rips.png",         name(n_visitas_rips_total_t) replace
	graph export "${figures}/total_rips_zoom.png",    name(n_visitas_rips_total_z) replace		
	graph export "${figures}/total_modules.png",      name(total_modules) replace	
	graph export "${figures}/total_modules_zoom.png", name(zoom_modules) replace	
	graph export "${figures}/total_consultations.png", name(total_consultations) replace
	graph export "${figures}/total_consultations_zoom.png", name(zoom_consultations)  replace	
	
	graph close *	
	

* General series by gender percentage
	line_gender_total p_visitas_female p_visitas_male gender_total
	line_gender_total p_consul_female  p_consul_male  gender_consul Consultations   	
	line_gender_total p_hospit_female  p_hospit_male  gender_hospit Hospitalizations	
	line_gender_total p_proced_female  p_proced_male  gender_proced Procedures      	
	line_gender_total p_urgenc_female  p_urgenc_male  gender_urgenc Emergencies     	
	line_gender_total p_preven_female  p_preven_male  gender_preven Preventive      
	line_plot_total p_prenat_female  midblue
	
	graph combine gender_consul_t gender_hospit_t gender_proced_t ///
	              gender_urgenc_t, name(total_g_modules, replace)
	graph combine gender_consul_z gender_hospit_z gender_proced_z ///
	              gender_urgenc_z, name(zoom_g_modules, replace)
								  
	graph close   gender_consul_t gender_hospit_t gender_proced_t /// 
	              gender_urgenc_t gender_consul_z gender_hospit_z ///
				  gender_proced_z gender_urgenc_z 
				  
	graph export "${figures}/gender_total.png" ,name(gender_total_t) replace
	graph export "${figures}/gender_zoom.png" ,name(gender_total_z) replace
	graph export "${figures}/gender_modules.png" ,name(total_g_modules) replace
	graph export "${figures}/gender_modules_zoom.png" ,name(zoom_g_modules)  replace
	graph export "${figures}/gender_preven_total.png" ,name(gender_preven_t) replace
	graph export "${figures}/gender_prenat_total.png" ,name(p_prenat_female_t) replace
	graph export "${figures}/gender_preven_zoom.png" ,name(gender_preven_z) replace
	graph export "${figures}/gender_prenat_zoom.png" ,name(p_prenat_female_z) replace

	graph close *	
				
				
/*
twoway rarea zero p_consul_female month, fcolor(midblue%50) lcolor(gray%1)   /// 
    || rarea p_consul_female yy month, fcolor(emidblue%50) lcolor(gray%1) ///
    ,  legend(order(1 "{fontface Times New Roman: Female}" ///
	                 2 "{fontface Times New Roman: Male}")) /// 
    ytitle("{fontface Times New Roman: Percentage (%)}") ///
    xtitle("{fontface Times New Roman: }") ///	
	xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
	legend(pos(6) c(2))	///			 
	graphr(c(white))				  
*/	

		
* General series by score percentage using time lines		
foreach var of newlist visita_rips visita_rips_m visita_rips_f consul consul_m consul_f hospit hospit_m hospit_f proced proced_m proced_f urgenc urgenc_m urgenc_f preven preven_m preven_f prenat prenat_m prenat_f {
	
	line_plot_score_zoom `var'_s1520 blue 15-20
	line_plot_score_zoom `var'_s2025 blue 20-25
	line_plot_score_zoom `var'_s2530 blue 25-30.56
	line_plot_score_zoom `var'_s3035 blue 30.56-35
	line_plot_score_zoom `var'_s3540 blue 35-40
	line_plot_score_zoom `var'_s4045 blue 40-45
		
	graph combine `var'_s1520_z `var'_s2025_z `var'_s2530_z `var'_s3035_z /// 
				  `var'_s3540_z `var'_s4045_z, name(`var', replace)		
	graph close `var'_s1520_z `var'_s2025_z `var'_s2530_z `var'_s3035_z  ///
				`var'_s3540_z `var'_s4045_z			
				
	graph export "${figures}/score_lines_`var'.png" ,name(`var') replace				
	graph close `var'				
}

foreach var of newlist visita_rips visita_rips_m visita_rips_f consul consul_m consul_f hospit hospit_m hospit_f proced proced_m proced_f urgenc urgenc_m urgenc_f preven preven_m preven_f prenat prenat_m prenat_f {
	
*	line_plot_score_zoom `var'_s1520 blue 15-20
	line_plot_score_zoom `var'_s2025 blue 20-25
	line_plot_score_zoom `var'_s2530 blue 25-30.56
	line_plot_score_zoom `var'_s3035 blue 30.56-35
	line_plot_score_zoom `var'_s3540 blue 35-40
	line_plot_score_zoom `var'_s4045 blue 40-45
		
	graph combine  `var'_s2025_z `var'_s2530_z `var'_s3035_z /// 
				  `var'_s3540_z `var'_s4045_z, name(`var', replace)	ycommon	
	graph close  `var'_s2025_z `var'_s2530_z `var'_s3035_z  ///
				`var'_s3540_z `var'_s4045_z			
				
	graph export "${figures}/yc_score_lines_`var'.png" ,name(`var') replace				
	graph close `var'				
}






* General series by score percentage using bar figures		

foreach var of newlist visita_rips visita_rips_m visita_rips_f consul consul_m consul_f hospit hospit_m hospit_f proced proced_m proced_f urgenc urgenc_m urgenc_f preven preven_m preven_f prenat prenat_m prenat_f {
			
twoway (bar p6_`var'_s month if inrange(month, 675,699), barwidth(0.7) color(ebg)) ///
       (bar p5_`var'_s month if inrange(month, 675,699), barwidth(0.7) color(ebblue)) ///
       (bar p4_`var'_s month if inrange(month, 675,699), barwidth(0.7) color(edkblue)) ///
       (bar p3_`var'_s month if inrange(month, 675,699), barwidth(0.7) color(eltgreen)) ///
       (bar p2_`var'_s month if inrange(month, 675,699), barwidth(0.7) color(olive_teal)) ///
       (bar p1_`var'_s month if inrange(month, 675,699), barwidth(0.7) color(ltblue)), ///
       ytitle("{fontface Times New Roman: Proportion Relative to Total by Score}") ///
       xtitle("{fontface Times New Roman: }") ///
       legend(order(6 "{fontface Times New Roman: Score range: 15-20}" ///
                    5 "{fontface Times New Roman: Score range: 20-25}" ///
                    4 "{fontface Times New Roman: Score range: 25-30}" ///
                    3 "{fontface Times New Roman: Score range: 30-35}" ///
                    2 "{fontface Times New Roman: Score range: 35-40}" ///
                    1 "{fontface Times New Roman: Score range: 40-45}")) ///
	   xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
	   ylabel(#10, format(%9.3fc)) ///
	   xlabel(#25, labsize(vsmall) grid angle(45)) ///		
	   legend(pos(6) c(3))	///	
	   graphr(c(white))	///
	   name(`var', replace)		
	   
	graph export "${figures}/score_bar_`var'.png" ,name(`var') replace				
	graph close `var'				
	   
	   
}		
				
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		

