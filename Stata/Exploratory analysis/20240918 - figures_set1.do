
global root "D:\Steban Pineda\Documents\DIME\Transportation and health"
global results     "${root}/Results_run_banrep"
global figures     "${root}/outputs/figures/20240925-descriptive_analysis_step1"
global graphs_rips "${root}/outputs/figures/Descriptives_RIPS"
global graphs_pila "${root}/outputs/figures/Descriptives_PILA"
global aux_db      "${root}/Results_run_banrep/auxiliar_databases"

import excel "${root}/outputs/20240918-descriptives_set1_raw.xlsx", sheet("Dummys_vars") firstrow clear
	tsset monthly
	ren *4245* *4045*

	gen month = mofd(monthly_date)
	format month %tm
		
* Individuals per system with at least one register between 2010-2019 ----------
	gen n_rips_m1019 = 174874
	gen n_pila_m1019 = 183298

* Individuals per system with at least one register between 2015-2019 ----------
	gen n_rips_m1519 = 152595
	gen n_pila_m1519 = 153972

* Individuals per system and score with at least one register between 2010-2019
	gen n_rips_m1019_s2025 = 27236
	gen n_rips_m1019_s2530 = 46659
	gen n_rips_m1019_s3035 = 31303
	gen n_rips_m1019_s3540 = 32050
	gen n_rips_m1019_s4045 = 37626

	gen n_pila_m1019_s2025 = 28643
	gen n_pila_m1019_s2530 = 49153
	gen n_pila_m1019_s3035 = 32803
	gen n_pila_m1019_s3540 = 33510
	gen n_pila_m1019_s4045 = 39189

* Individuals per system and score with at least one register between 2015-2019
	gen n_rips_m1519_s2025 = 23664
	gen n_rips_m1519_s2530 = 40509
	gen n_rips_m1519_s3035 = 27162
	gen n_rips_m1519_s3540 = 28097
	gen n_rips_m1519_s4045 = 33163

	gen n_pila_m1519_s2025 = 23905
	gen n_pila_m1519_s2530 = 41416
	gen n_pila_m1519_s3035 = 27461
	gen n_pila_m1519_s3540 = 28148
	gen n_pila_m1519_s4045 = 33042


foreach score of numlist  2025 2530 3035 3540 4045 {	
dis in red " Working on: `score'"

	gen n_consul_s`score'_t15  = (n_consultas_s`score'/n_rips_m1519_s`score')                 
	gen d_consul_s`score'_t15  = (un_consultas_score_`score'/n_rips_m1519_s`score')	
		
	gen n_proced_s`score'_t15  = (n_procedimientos_s`score'/n_rips_m1519_s`score')                 
	gen d_proced_s`score'_t15  = (un_procedimientos_score_`score'/n_rips_m1519_s`score')		
	
	gen n_hosp_s`score'_t15    = (n_hospitalizaciones_s`score'/n_rips_m1519_s`score')                 
	gen d_hosp_s`score'_t15    = (un_hospitalizaciones_score_`score'/n_rips_m1519_s`score')		
	
	gen n_emerg_s`score'_t15   = (n_urgencias_s`score'/n_rips_m1519_s`score')                 
	gen d_emerg_s`score'_t15   = (un_urgencias_score_`score'/n_rips_m1519_s`score')	
	
	gen n_preven_s`score'_t15  = (c_preven_s`score'/n_rips_m1519_s`score')                 
	gen d_preven_s`score'_t15  = (un_preven_score_`score'/n_rips_m1519_s`score')	
		
	gen n_prenat_s`score'_t15  = (c_prenat_s`score'/n_rips_m1519_s`score')                 
	gen d_prenat_s`score'_t15  = (un_prenat_score_`score'/n_rips_m1519_s`score')		
	
}	

gen x = month
*br month x

		
		
/*
twoway (connected n_consul_s2025_t15 month if inrange(month, 677, 701),  symbol(triangle_hollow) mcolor(black%50) lpattern(dash) lcol(black%50)) ///		
       (connected n_consul_s2530_t15 month if inrange(month, 677, 701),  symbol(smx)             mcolor(black%50) lpattern(solid) lcol(black%50)) ///   
       (connected n_consul_s4045_t15 month if inrange(month, 677, 701),  symbol(smcircle)        mcolor(black%50) lpattern(vshortdash) lcol(back%50)), ///  
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#24, labsize(vsmall) grid angle(90)) ///
        ylabel(#10, labsize(small) format(%9.2fc)) ///
        legend(position(6)  col(5)) ///
		title("Consultations per capita, by SISBEN score group", size(small)) ///
		subtitle("{bf:Elegible at a lower discount (<=30.56) vs. Not eligible (>40)}", size(small)) ///
		xtitle("")  ytitle("No. of consultations") ///
		legend(label(1 "Score 20-25")) ///
		legend(label(2 "Score 25-30.56")) ///
		legend(label(3 "Score 40-45")) ///		
		note("Notes: Calculated as the number of consultations by socore group divided by total persons by score group in the matched sample") name(n_consul_under_t, replace)
*/
		
/*		
twoway (connected n_visitas_rips_total month if inrange(month, 660, 719), $op_s2 ),  /// 
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#30, labsize(vsmall) grid angle(90)) ///
        ylabel(#10, labsize(small) format(%9.0fc)) ///
        legend(position(6)  col(5)) ///
		title("{bf:RIPS}", size(small)) ///
		subtitle("", size(small)) ///
		xtitle("")  ytitle("", size(small)) ///
		note("") name(serie, replace)
*/		
		

global op_s1 "symbol(triangle_hollow) mcolor(red%50)   lpattern(dash)       lcol(red%50)"      
global op_s2 "symbol(smx)             mcolor(green%50) lpattern(solid)      lcol(green%50)"   
global op_s3 "symbol(smcircle)        mcolor(blue%50)  lpattern(vshortdash) lcol(blue%50)"		
		
******************************** CONSULTATIONS *********************************-

* Average ----------------------------------------------------------------------

twoway (connected n_consul_s2025_t15 month if inrange(month, 677, 701), $op_s1 )   ///		
       (connected n_consul_s2530_t15 month if inrange(month, 677, 701), $op_s2 )   ///   
       (connected n_consul_s4045_t15 month if inrange(month, 677, 701), $op_s3 ),  /// 
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#24, labsize(vsmall) grid angle(90)) ///
        ylabel(#10, labsize(small) format(%9.2fc)) ///
        legend(position(6)  col(5)) ///
		title("Consultations per capita, by SISBEN score group", size(small)) ///
		subtitle("{bf:Elegible at a lower discount (<=30.56) vs. Not eligible (>40)}", size(small)) ///
		xtitle("")  ytitle("No. of consultations", size(small)) ///
		legend(label(1 "Score 20-25")) ///
		legend(label(2 "Score 25-30.56")) ///
		legend(label(3 "Score 40-45")) ///		
		note("Notes: Calculated as the number of consultations by score group divided by total persons by score group in the matched sample") name(n_consul_g1, replace)
	   graph export "${figures}/20240925_n_consultations_g1.png", replace		   		
		
twoway (connected n_consul_s3035_t15 month if inrange(month, 677, 701), $op_s1 )   ///		
       (connected n_consul_s3540_t15 month if inrange(month, 677, 701), $op_s2 )   ///  
       (connected n_consul_s4045_t15 month if inrange(month, 677, 701), $op_s3 ),  /// 
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#24, labsize(vsmall) grid angle(90)) ///
        ylabel(#10, labsize(small) format(%9.2fc)) ///
        legend(position(6)  col(5)) ///
		title("Consultations per capita, by SISBEN score group", size(small)) ///
		subtitle("{bf:Lost eligibility (30.56-40] vs. Not eligible (>40) }", size(small)) ///
		xtitle("")  ytitle("No. of consultations", size(small)) ///
		legend(label(1 "Score 30.56-35")) ///
		legend(label(2 "Score 35-40")) ///
		legend(label(3 "Score 40-45")) ///		
		note("Notes: Calculated as the number of consultations by score group divided by total persons by score group in the matched sample") name(n_consul_g2, replace)
	   graph export "${figures}/20240925_n_consultations_g2.png", replace		   		

		
* Percentage --------------------------------------------------------------------	
twoway (connected d_consul_s2025_t15 month if inrange(month, 677, 701), $op_s1 )   ///		
       (connected d_consul_s2530_t15 month if inrange(month, 677, 701), $op_s2 )   ///   
       (connected d_consul_s4045_t15 month if inrange(month, 677, 701), $op_s3 ),  /// 
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#24, labsize(vsmall) grid angle(90)) ///
        ylabel(#10, labsize(small) format(%9.2fc)) ///
        legend(position(6)  col(5)) ///
		title("Percentage of people with at least one consultation, by SISBEN score group", size(small)) ///
		subtitle("{bf:Elegible at a lower discount (<=30.56) vs. Not eligible (>40)}", size(small)) ///
		xtitle("")  ytitle("Percentage by SISBEN score groups", size(small)) ///
		legend(label(1 "Score 20-25")) ///
		legend(label(2 "Score 25-30.56")) ///
		legend(label(3 "Score 40-45")) ///		
		note("Notes: Calculated as the number of individuals who consulted at least once in a given month divided by total persons by" "score group in the matched sample") name(d_consul_g1, replace)
	   graph export "${figures}/20240925_d_consultations_g1.png", replace	
		
twoway (connected d_consul_s3035_t15 month if inrange(month, 677, 701), $op_s1 )   ///		
       (connected d_consul_s3540_t15 month if inrange(month, 677, 701), $op_s2 )   ///  
       (connected d_consul_s4045_t15 month if inrange(month, 677, 701), $op_s3 ),  /// 
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#24, labsize(vsmall) grid angle(90)) ///
        ylabel(#10, labsize(small) format(%9.2fc)) ///
        legend(position(6)  col(5)) ///
		title("Percentage of people with at least one consultation, by SISBEN score group", size(small)) ///
		subtitle("{bf:Lost eligibility (30.56-40] vs. Not eligible (>40) }", size(small)) ///
		xtitle("")  ytitle("Percentage by SISBEN score groups", size(small)) ///
		legend(label(1 "Score 30.56-35")) ///
		legend(label(2 "Score 35-40")) ///
		legend(label(3 "Score 40-45")) ///		
		note("Notes: Calculated as the number of individuals who consulted at least once in a given month divided by total persons by" "score group in the matched sample") name(d_consul_g2, replace)
	   graph export "${figures}/20240925_d_consultations_g2.png", replace	
		
		
		
******************************** PRODECURES ************************************

* Average ----------------------------------------------------------------------

twoway (connected n_proced_s2025_t15 month if inrange(month, 677, 701), $op_s1 )   ///		
       (connected n_proced_s2530_t15 month if inrange(month, 677, 701), $op_s2 )   ///   
       (connected n_proced_s4045_t15 month if inrange(month, 677, 701), $op_s3 ),  /// 
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#24, labsize(vsmall) grid angle(90)) ///
        ylabel(#10, labsize(small) format(%9.2fc)) ///
        legend(position(6)  col(5)) ///
		title("Procedures per capita, by SISBEN score group", size(small)) ///
		subtitle("{bf:Elegible at a lower discount (<=30.56) vs. Not eligible (>40)}", size(small)) ///
		xtitle("")  ytitle("No. of consultations", size(small)) ///
		legend(label(1 "Score 20-25")) ///
		legend(label(2 "Score 25-30.56")) ///
		legend(label(3 "Score 40-45")) ///		
		note("Notes: Calculated as the number of procedures by score group divided by total persons by score group in the matched sample") name(n_proced_g1, replace)
	   graph export "${figures}/20240925_n_procedures_g1.png", replace	
		
twoway (connected n_proced_s3035_t15 month if inrange(month, 677, 701), $op_s1 )   ///		
       (connected n_proced_s3540_t15 month if inrange(month, 677, 701), $op_s2 )   ///  
       (connected n_proced_s4045_t15 month if inrange(month, 677, 701), $op_s3 ),  /// 
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#24, labsize(vsmall) grid angle(90)) ///
        ylabel(#10, labsize(small) format(%9.2fc)) ///
        legend(position(6)  col(5)) ///
		title("Procedures per capita, by SISBEN score group", size(small)) ///
		subtitle("{bf:Lost eligibility (30.56-40] vs. Not eligible (>40) }", size(small)) ///
		xtitle("")  ytitle("No. of consultations", size(small)) ///
		legend(label(1 "Score 30.56-35")) ///
		legend(label(2 "Score 35-40")) ///
		legend(label(3 "Score 40-45")) ///		
		note("Notes: Calculated as the number of procedures by score group divided by total persons by score group in the matched sample") name(n_proced_g2, replace)
	   graph export "${figures}/20240925_n_procedures_g2.png", replace	

		
* Percentage --------------------------------------------------------------------	
twoway (connected d_proced_s2025_t15 month if inrange(month, 677, 701), $op_s1 )   ///		
       (connected d_proced_s2530_t15 month if inrange(month, 677, 701), $op_s2 )   ///   
       (connected d_proced_s4045_t15 month if inrange(month, 677, 701), $op_s3 ),  /// 
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#24, labsize(vsmall) grid angle(90)) ///
        ylabel(#10, labsize(small) format(%9.2fc)) ///
        legend(position(6)  col(5)) ///
		title("Percentage of people with at least one procedure, by SISBEN score group", size(small)) ///
		subtitle("{bf:Elegible at a lower discount (<=30.56) vs. Not eligible (>40)}", size(small)) ///
		xtitle("")  ytitle("Percentage by SISBEN score groups", size(small)) ///
		legend(label(1 "Score 20-25")) ///
		legend(label(2 "Score 25-30.56")) ///
		legend(label(3 "Score 40-45")) ///		
		note("Notes: Calculated as the number of individuals who register at least one procedure in a given month divided by total persons by" "score group in the matched sample") name(d_proced_g1, replace)
	   graph export "${figures}/20240925_d_procedures_g1.png", replace	
		
twoway (connected d_proced_s3035_t15 month if inrange(month, 677, 701), $op_s1 )   ///		
       (connected d_proced_s3540_t15 month if inrange(month, 677, 701), $op_s2 )   ///  
       (connected d_proced_s4045_t15 month if inrange(month, 677, 701), $op_s3 ),  /// 
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#24, labsize(vsmall) grid angle(90)) ///
        ylabel(#10, labsize(small) format(%9.2fc)) ///
        legend(position(6)  col(5)) ///
		title("Percentage of people with at least one procedure, by SISBEN score group", size(small)) ///
		subtitle("{bf:Lost eligibility (30.56-40] vs. Not eligible (>40) }", size(small)) ///
		xtitle("")  ytitle("Percentage by SISBEN score groups", size(small)) ///
		legend(label(1 "Score 30.56-35")) ///
		legend(label(2 "Score 35-40")) ///
		legend(label(3 "Score 40-45")) ///		
		note("Notes: Calculated as the number of individuals who register at least one procedure in a given month divided by total persons by" "score group in the matched sample") name(d_proced_g2, replace)
	   graph export "${figures}/20240925_d_procedures_g2.png", replace	
		
		
******************************** EMERGENCIES ***********************************

* Average ----------------------------------------------------------------------

twoway (connected n_emerg_s2025_t15 month if inrange(month, 677, 701), $op_s1 )   ///		
       (connected n_emerg_s2530_t15 month if inrange(month, 677, 701), $op_s2 )   ///   
       (connected n_emerg_s4045_t15 month if inrange(month, 677, 701), $op_s3 ),  /// 
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#24, labsize(vsmall) grid angle(90)) ///
        ylabel(#10, labsize(small) format(%9.2fc)) ///
        legend(position(6)  col(5)) ///
		title("Emergencies per capita, by SISBEN score group", size(small)) ///
		subtitle("{bf:Elegible at a lower discount (<=30.56) vs. Not eligible (>40)}", size(small)) ///
		xtitle("")  ytitle("No. of consultations", size(small)) ///
		legend(label(1 "Score 20-25")) ///
		legend(label(2 "Score 25-30.56")) ///
		legend(label(3 "Score 40-45")) ///		
		note("Notes: Calculated as the number of emergencies by score group divided by total persons by score group in the matched sample") name(n_emerg_g1, replace)
	   graph export "${figures}/20240925_n_emergencies_g1.png", replace	
		
twoway (connected n_emerg_s3035_t15 month if inrange(month, 677, 701), $op_s1 )   ///		
       (connected n_emerg_s3540_t15 month if inrange(month, 677, 701), $op_s2 )   ///  
       (connected n_emerg_s4045_t15 month if inrange(month, 677, 701), $op_s3 ),  /// 
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#24, labsize(vsmall) grid angle(90)) ///
        ylabel(#10, labsize(small) format(%9.2fc)) ///
        legend(position(6)  col(5)) ///
		title("Emergencies per capita, by SISBEN score group", size(small)) ///
		subtitle("{bf:Lost eligibility (30.56-40] vs. Not eligible (>40) }", size(small)) ///
		xtitle("")  ytitle("No. of consultations", size(small)) ///
		legend(label(1 "Score 30.56-35")) ///
		legend(label(2 "Score 35-40")) ///
		legend(label(3 "Score 40-45")) ///		
		note("Notes: Calculated as the number of emergencies by score group divided by total persons by score group in the matched sample") name(n_emerg_g2, replace)
	   graph export "${figures}/20240925_n_emergencies_g2.png", replace	

		
* Percentage --------------------------------------------------------------------	
twoway (connected d_emerg_s2025_t15 month if inrange(month, 677, 701), $op_s1 )   ///		
       (connected d_emerg_s2530_t15 month if inrange(month, 677, 701), $op_s2 )   ///   
       (connected d_emerg_s4045_t15 month if inrange(month, 677, 701), $op_s3 ),  /// 
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#24, labsize(vsmall) grid angle(90)) ///
        ylabel(#10, labsize(small) format(%9.2fc)) ///
        legend(position(6)  col(5)) ///
		title("Percentage of people with at least one emergency, by SISBEN score group", size(small)) ///
		subtitle("{bf:Elegible at a lower discount (<=30.56) vs. Not eligible (>40)}", size(small)) ///
		xtitle("")  ytitle("Percentage by SISBEN score groups", size(small)) ///
		legend(label(1 "Score 20-25")) ///
		legend(label(2 "Score 25-30.56")) ///
		legend(label(3 "Score 40-45")) ///		
		note("Notes: Calculated as the number of individuals who register at least one emergency in a given month divided by total persons by" "score group in the matched sample") name(d_emerg_g1, replace)
	   graph export "${figures}/20240925_d_emergencies_g1.png", replace	
		
twoway (connected d_emerg_s3035_t15 month if inrange(month, 677, 701), $op_s1 )   ///		
       (connected d_emerg_s3540_t15 month if inrange(month, 677, 701), $op_s2 )   ///  
       (connected d_emerg_s4045_t15 month if inrange(month, 677, 701), $op_s3 ),  /// 
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#24, labsize(vsmall) grid angle(90)) ///
        ylabel(#10, labsize(small) format(%9.2fc)) ///
        legend(position(6)  col(5)) ///
		title("Percentage of people with at least one emergency, by SISBEN score group", size(small)) ///
		subtitle("{bf:Lost eligibility (30.56-40] vs. Not eligible (>40) }", size(small)) ///
		xtitle("")  ytitle("Percentage by SISBEN score groups", size(small)) ///
		legend(label(1 "Score 30.56-35")) ///
		legend(label(2 "Score 35-40")) ///
		legend(label(3 "Score 40-45")) ///		
		note("Notes: Calculated as the number of individuals who register at least one emergency in a given month divided by total persons by" "score group in the matched sample") name(d_emerg_g2, replace)
	   graph export "${figures}/20240925_d_emergencies_g2.png", replace	
		
				
		
**************************** HOSPITALIZATIONS **********************************

* Average ----------------------------------------------------------------------

twoway (connected n_hosp_s2025_t15 month if inrange(month, 677, 701), $op_s1 )   ///		
       (connected n_hosp_s2530_t15 month if inrange(month, 677, 701), $op_s2 )   ///   
       (connected n_hosp_s4045_t15 month if inrange(month, 677, 701), $op_s3 ),  /// 
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#24, labsize(vsmall) grid angle(90)) ///
        ylabel(#10, labsize(small) format(%9.2fc)) ///
        legend(position(6)  col(5)) ///
		title("Hospitalizations per capita, by SISBEN score group", size(small)) ///
		subtitle("{bf:Elegible at a lower discount (<=30.56) vs. Not eligible (>40)}", size(small)) ///
		xtitle("")  ytitle("No. of consultations", size(small)) ///
		legend(label(1 "Score 20-25")) ///
		legend(label(2 "Score 25-30.56")) ///
		legend(label(3 "Score 40-45")) ///		
		note("Notes: Calculated as the number of hospitalizations by score group divided by total persons by score group in the matched sample") name(n_hosp_g1, replace)
	   graph export "${figures}/20240925_n_hospitalizations_g1.png", replace	
		
twoway (connected n_hosp_s3035_t15 month if inrange(month, 677, 701), $op_s1 )   ///		
       (connected n_hosp_s3540_t15 month if inrange(month, 677, 701), $op_s2 )   ///  
       (connected n_hosp_s4045_t15 month if inrange(month, 677, 701), $op_s3 ),  /// 
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#24, labsize(vsmall) grid angle(90)) ///
        ylabel(#10, labsize(small) format(%9.2fc)) ///
        legend(position(6)  col(5)) ///
		title("Hospitalizations per capita, by SISBEN score group", size(small)) ///
		subtitle("{bf:Lost eligibility (30.56-40] vs. Not eligible (>40) }", size(small)) ///
		xtitle("")  ytitle("No. of consultations", size(small)) ///
		legend(label(1 "Score 30.56-35")) ///
		legend(label(2 "Score 35-40")) ///
		legend(label(3 "Score 40-45")) ///		
		note("Notes: Calculated as the number of hospitalizations by score group divided by total persons by score group in the matched sample") name(n_hosp_g2, replace)
	   graph export "${figures}/20240925_n_hospitalizations_g2.png", replace	

		
* Percentage --------------------------------------------------------------------	
twoway (connected d_hosp_s2025_t15 month if inrange(month, 677, 701), $op_s1 )   ///		
       (connected d_hosp_s2530_t15 month if inrange(month, 677, 701), $op_s2 )   ///   
       (connected d_hosp_s4045_t15 month if inrange(month, 677, 701), $op_s3 ),  /// 
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#24, labsize(vsmall) grid angle(90)) ///
        ylabel(#10, labsize(small) format(%9.2fc)) ///
        legend(position(6)  col(5)) ///
		title("Percentage of people with at least one hospitalization, by SISBEN score group", size(small)) ///
		subtitle("{bf:Elegible at a lower discount (<=30.56) vs. Not eligible (>40)}", size(small)) ///
		xtitle("")  ytitle("Percentage by SISBEN score groups", size(small)) ///
		legend(label(1 "Score 20-25")) ///
		legend(label(2 "Score 25-30.56")) ///
		legend(label(3 "Score 40-45")) ///		
		note("Notes: Calculated as the number of individuals who register at least one hospitalization in a given month divided by total persons by" "score group in the matched sample") name(d_hosp_g1, replace)
	   graph export "${figures}/20240925_d_hospitalizations_g1.png", replace	
		
twoway (connected d_hosp_s3035_t15 month if inrange(month, 677, 701), $op_s1 )   ///		
       (connected d_hosp_s3540_t15 month if inrange(month, 677, 701), $op_s2 )   ///  
       (connected d_hosp_s4045_t15 month if inrange(month, 677, 701), $op_s3 ),  /// 
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#24, labsize(vsmall) grid angle(90)) ///
        ylabel(#10, labsize(small) format(%9.2fc)) ///
        legend(position(6)  col(5)) ///
		title("Percentage of people with at least one hospitalization, by SISBEN score group", size(small)) ///
		subtitle("{bf:Lost eligibility (30.56-40] vs. Not eligible (>40) }", size(small)) ///
		xtitle("")  ytitle("Percentage by SISBEN score groups", size(small)) ///
		legend(label(1 "Score 30.56-35")) ///
		legend(label(2 "Score 35-40")) ///
		legend(label(3 "Score 40-45")) ///		
		note("Notes: Calculated as the number of individuals who register at least one hospitalization in a given month divided by total persons by" "score group in the matched sample") name(d_hosp_g2, replace)
	   graph export "${figures}/20240925_d_hospitalizations_g2.png", replace	
		
				
		
************************* PREVENTIVE CONSULTATIONS *****************************

* Average ----------------------------------------------------------------------

twoway (connected n_preven_s2025_t15 month if inrange(month, 677, 701), $op_s1 )   ///		
       (connected n_preven_s2530_t15 month if inrange(month, 677, 701), $op_s2 )   ///   
       (connected n_preven_s4045_t15 month if inrange(month, 677, 701), $op_s3 ),  /// 
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#24, labsize(vsmall) grid angle(90)) ///
        ylabel(#10, labsize(small) format(%9.2fc)) ///
        legend(position(6)  col(5)) ///
		title("Procedures per capita, by SISBEN score group", size(small)) ///
		subtitle("{bf:Elegible at a lower discount (<=30.56) vs. Not eligible (>40)}", size(small)) ///
		xtitle("")  ytitle("No. of consultations", size(small)) ///
		legend(label(1 "Score 20-25")) ///
		legend(label(2 "Score 25-30.56")) ///
		legend(label(3 "Score 40-45")) ///		
		note("Notes: Calculated as the number of preventinve consultations by score group divided by total persons by score group in the matched sample") name(n_preven_g1, replace)
	   graph export "${figures}/20240925_n_preventive_g1.png", replace	
		
twoway (connected n_preven_s3035_t15 month if inrange(month, 677, 701), $op_s1 )   ///		
       (connected n_preven_s3540_t15 month if inrange(month, 677, 701), $op_s2 )   ///  
       (connected n_preven_s4045_t15 month if inrange(month, 677, 701), $op_s3 ),  /// 
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#24, labsize(vsmall) grid angle(90)) ///
        ylabel(#10, labsize(small) format(%9.2fc)) ///
        legend(position(6)  col(5)) ///
		title("Procedures per capita, by SISBEN score group", size(small)) ///
		subtitle("{bf:Lost eligibility (30.56-40] vs. Not eligible (>40) }", size(small)) ///
		xtitle("")  ytitle("No. of consultations", size(small)) ///
		legend(label(1 "Score 30.56-35")) ///
		legend(label(2 "Score 35-40")) ///
		legend(label(3 "Score 40-45")) ///		
		note("Notes: Calculated as the number of preventinve consultations by score group divided by total persons by score group in the matched sample") name(n_preven_g2, replace)
	   graph export "${figures}/20240925_n_preventive_g2.png", replace	

		
* Percentage --------------------------------------------------------------------	
twoway (connected d_preven_s2025_t15 month if inrange(month, 677, 701), $op_s1 )   ///		
       (connected d_preven_s2530_t15 month if inrange(month, 677, 701), $op_s2 )   ///   
       (connected d_preven_s4045_t15 month if inrange(month, 677, 701), $op_s3 ),  /// 
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#24, labsize(vsmall) grid angle(90)) ///
        ylabel(#10, labsize(small) format(%9.2fc)) ///
        legend(position(6)  col(5)) ///
		title("Percentage of people with at least one procedure, by SISBEN score group", size(small)) ///
		subtitle("{bf:Elegible at a lower discount (<=30.56) vs. Not eligible (>40)}", size(small)) ///
		xtitle("")  ytitle("Percentage by SISBEN score groups", size(small)) ///
		legend(label(1 "Score 20-25")) ///
		legend(label(2 "Score 25-30.56")) ///
		legend(label(3 "Score 40-45")) ///		
		note("Notes: Calculated as the number of individuals who consulted at least once in a given month divided by total persons by" "score group in the matched sample") name(d_preven_g1, replace)
	   graph export "${figures}/20240925_d_preventive_g1.png", replace	
		
twoway (connected d_preven_s3035_t15 month if inrange(month, 677, 701), $op_s1 )   ///		
       (connected d_preven_s3540_t15 month if inrange(month, 677, 701), $op_s2 )   ///  
       (connected d_preven_s4045_t15 month if inrange(month, 677, 701), $op_s3 ),  /// 
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#24, labsize(vsmall) grid angle(90)) ///
        ylabel(#10, labsize(small) format(%9.2fc)) ///
        legend(position(6)  col(5)) ///
		title("Percentage of people with at least one procedure, by SISBEN score group", size(small)) ///
		subtitle("{bf:Lost eligibility (30.56-40] vs. Not eligible (>40) }", size(small)) ///
		xtitle("")  ytitle("Percentage by SISBEN score groups", size(small)) ///
		legend(label(1 "Score 30.56-35")) ///
		legend(label(2 "Score 35-40")) ///
		legend(label(3 "Score 40-45")) ///		
		note("Notes: Calculated as the number of individuals who consulted at least once in a given month divided by total persons by" "score group in the matched sample") name(d_preven_g2, replace)
	   graph export "${figures}/20240925_d_preventive_g2.png", replace	
		
				
		
************************** PRENATAL CONSULTATIONS ******************************

* Average ----------------------------------------------------------------------

twoway (connected n_prenat_s2025_t15 month if inrange(month, 677, 701), $op_s1 )   ///		
       (connected n_prenat_s2530_t15 month if inrange(month, 677, 701), $op_s2 )   ///   
       (connected n_prenat_s4045_t15 month if inrange(month, 677, 701), $op_s3 ),  /// 
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#24, labsize(vsmall) grid angle(90)) ///
        ylabel(#10, labsize(small) format(%9.2fc)) ///
        legend(position(6)  col(5)) ///
		title("Procedures per capita, by SISBEN score group", size(small)) ///
		subtitle("{bf:Elegible at a lower discount (<=30.56) vs. Not eligible (>40)}", size(small)) ///
		xtitle("")  ytitle("No. of consultations", size(small)) ///
		legend(label(1 "Score 20-25")) ///
		legend(label(2 "Score 25-30.56")) ///
		legend(label(3 "Score 40-45")) ///		
		note("Notes: Calculated as the number of prenatal consultations by score group divided by total persons by score group in the matched sample") name(n_prenat_g1, replace)
	   graph export "${figures}/20240925_n_prenatal_g1.png", replace	
		
twoway (connected n_prenat_s3035_t15 month if inrange(month, 677, 701), $op_s1 )   ///		
       (connected n_prenat_s3540_t15 month if inrange(month, 677, 701), $op_s2 )   ///  
       (connected n_prenat_s4045_t15 month if inrange(month, 677, 701), $op_s3 ),  /// 
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#24, labsize(vsmall) grid angle(90)) ///
        ylabel(#10, labsize(small) format(%9.2fc)) ///
        legend(position(6)  col(5)) ///
		title("Procedures per capita, by SISBEN score group", size(small)) ///
		subtitle("{bf:Lost eligibility (30.56-40] vs. Not eligible (>40) }", size(small)) ///
		xtitle("")  ytitle("No. of consultations", size(small)) ///
		legend(label(1 "Score 30.56-35")) ///
		legend(label(2 "Score 35-40")) ///
		legend(label(3 "Score 40-45")) ///		
		note("Notes: Calculated as the number of prenatal consultations by score group divided by total persons by score group in the matched sample") name(n_prenat_g2, replace)
	   graph export "${figures}/20240925_n_prenatal_g2.png", replace	

		
* Percentage --------------------------------------------------------------------	
twoway (connected d_prenat_s2025_t15 month if inrange(month, 677, 701), $op_s1 )   ///		
       (connected d_prenat_s2530_t15 month if inrange(month, 677, 701), $op_s2 )   ///   
       (connected d_prenat_s4045_t15 month if inrange(month, 677, 701), $op_s3 ),  /// 
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#24, labsize(vsmall) grid angle(90)) ///
        ylabel(#10, labsize(small) format(%9.2fc)) ///
        legend(position(6)  col(5)) ///
		title("Percentage of people with at least one procedure, by SISBEN score group", size(small)) ///
		subtitle("{bf:Elegible at a lower discount (<=30.56) vs. Not eligible (>40)}", size(small)) ///
		xtitle("")  ytitle("Percentage by SISBEN score groups", size(small)) ///
		legend(label(1 "Score 20-25")) ///
		legend(label(2 "Score 25-30.56")) ///
		legend(label(3 "Score 40-45")) ///		
		note("Notes: Calculated as the number of individuals who consulted at least once in a given month divided by total persons by" "score group in the matched sample") name(d_prenat_g1, replace)
	   graph export "${figures}/20240925_d_prenatal_g1.png", replace	
	   
twoway (connected d_prenat_s3035_t15 month if inrange(month, 677, 701), $op_s1 )   ///		
       (connected d_prenat_s3540_t15 month if inrange(month, 677, 701), $op_s2 )   ///  
       (connected d_prenat_s4045_t15 month if inrange(month, 677, 701), $op_s3 ),  /// 
		xline(687, lpattern(dash_dot) lwidth(vthin) lcolor(black))  ///
        xlabel(#24, labsize(vsmall) grid angle(90)) ///
        ylabel(#10, labsize(small) format(%9.2fc)) ///
        legend(position(6)  col(5)) ///
		title("Percentage of people with at least one procedure, by SISBEN score group", size(small)) ///
		subtitle("{bf:Lost eligibility (30.56-40] vs. Not eligible (>40) }", size(small)) ///
		xtitle("")  ytitle("Percentage by SISBEN score groups", size(small)) ///
		legend(label(1 "Score 30.56-35")) ///
		legend(label(2 "Score 35-40")) ///
		legend(label(3 "Score 40-45")) ///		
		note("Notes: Calculated as the number of individuals who consulted at least once in a given month divided by total persons by" "score group in the matched sample") name(d_prenat_g2, replace)
	   graph export "${figures}/20240925_d_prenatal_g2.png", replace	
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		







		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		