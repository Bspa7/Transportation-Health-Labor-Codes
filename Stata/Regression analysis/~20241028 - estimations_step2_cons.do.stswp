/*
DIFFERENCES IN DIFFERENCES FOR GENERAL OUTCOMES
	Creation date: 28-Oct-2024
	Author: Brayan Pineda
	Last modification: 28-Oct-2024 by Brayan Pineda
	Objective: Estimations to general outcomes and consultations types

In this Dofile you'll find 2 sections:
	Section #1: Full model estimations and table creation -> export results to an excel file 
	Section #2: Results in Graphs, using only the full model
	

The entire sample is filtered to Jan-2016 until Sep-2017.	
	
Definition of treatments and control	
	T1: Sisbén score <=30.56
		Reduction of incentive from 50% to 25% (or price change from 0.5 to 0.75 of full tariff)
		Eligible at a lower incentive

	T2: Sisbén score 30.56-40
		Lost eligibility (or price change from 0.5 to 1 of full tariff  

	C:  Sisbén score >40
		Not eligible in the time frame  	

*/		

*global root "//wmedesrv/GAMMA/Christian Posso/_banrep_research/proyectos/project_transport_health"
global root "Z:/Christian Posso/_banrep_research/proyectos/project_transport_health"
*global root "D:/Steban Pineda/Documents/DIME/Transportation and health"
*global  root "/Users/brayanpineda/Library/CloudStorage/OneDrive-Personal/Trabajo/2024_DIME/COL Health and Public Transport"


global data        "${root}/data"
global results     "${root}/Results_run_banrep"
global tables      "${root}/outputs/tables"
global graphs      "${root}/outputs/figures"

* Define the folder to save the results and redefine the globals for xlsx and png files
cap mkdir "${tables}/20241028 - t_results"
cap mkdir "${graphs}/20241028 - g_results"

global tables      "${tables}/20241028 - t_results"
global graphs      "${graphs}/20241028 - g_results"



******************* PREPARING THE SAMPLE 2016m1 - 2017m9 ***********************

use "${data}/panel_sample1045_t1019.dta", replace
	gen month = mofd(monthly_date)
	format month %tm
	keep if inrange(month, 672, 692) // filtering the sample from 2016m1 to 6 periods after law change
	
	gen fe_month = month(monthly_date) // this is the variable for FE of month
*	br  month*	
	
	
* NECCESARY VARIABLES ----------------------------------------------------------
	gen post_law = (month>=687) // Law came into force on Apr-2017	
	gen treatment_1 = (puntaje<=30.56) 
	gen treatment_2 = (puntaje>30.56 & puntaje<=40)

* Redefining the controls for each treatment: Not eligible in the time frame
	replace treatment_1 = . if treatment_1==0 & puntaje<=40
	replace treatment_2 = . if treatment_2==0 & puntaje<=40
	
* Dummys per each month after the policy
forvalues i = 1/6 {
dis in red "Post law in t+`i'"	

	cap drop       post_law_t`i'
	gen            post_law_t`i' = (month==686+`i') // 687 corresponds to Apr-2017
	label variable post_law_t`i' "Post law dummy month in t+`i'"
		
}   
* Interactions to estimations
* Treatment 1
gen interaction = treatment_1*post_law
forvalues i=1/6 {
	dis in red "creating interaction in t+`i'"
	gen interaction_t`i' = treatment_1*post_law_t`i'
}

* Treatment 2
gen interaction2 = treatment_2*post_law
forvalues i=1/6 {
	dis in red "creating interaction in t+`i'"
	gen interaction2_t`i' = treatment_2*post_law_t`i'
}



********************************************************************************
**# ***************** ESTIMATIONS TREATMENT 1 **********************************
********************************************************************************


global outcomes_gnral "d_consultas d_cons_gral d_cons_esp d_cons_primera d_cons_control n_consultas n_cons_gral n_cons_esp n_cons_primera n_cons_control"

/* Just for Brayan's code
foreach var of global outcomes_gnral {
	replace `var' = min(30, max(1, round(rnormal(10, 5))))
}
	replace n_hospitalizaciones = min(30, max(1, round(rnormal(10, 5))))
	replace d_hospitalizaciones = min(30, max(1, round(rnormal(10, 5))))	
	replace n_urgencias = min(30, max(1, round(rnormal(10, 5))))
	replace d_urgencias = min(30, max(1, round(rnormal(10, 5))))	
*/
	
* Specification #1	
* Full model: Fixed effects for individual and time, clustering by individual
putexcel set "${tables}/20241028-results_step2_t1.xlsx", sheet(spec1_model3, replace) modify
putexcel A1=("Outcome") B1=("Mean") C1=("SD.") D1=("Coeff.") E1=("S.E.") F1=("p-value") G1=("N") H1=("R2")	

local s = 1			
foreach y of global outcomes_gnral {

	local s = `s'+1	
	dis in red "`s' --- `y'"
	reghdfe `y' treatment_1 post_law interaction, absorb(personabasicaid fe_month) vce(cluster personabasicaid)	
			putexcel A`s'=("`y'") 
	
			sum `y' if treatment_1==1 & post_law==0
			putexcel B`s'=(r(mean))	
			putexcel C`s'=(r(sd))			
			
			putexcel D`s'=(_b[interaction])	
			putexcel E`s'=(_se[interaction])			
			test _b[interaction] = 0
			putexcel F`s'=(r(p))
			putexcel G`s'=(e(N))
			putexcel H`s'=(e(r2))	
			
			distinct personabasicaid if treatment_1!=.
			putexcel I`s'=(r(ndistinct))			
			distinct month if treatment_1!=.
			putexcel j`s'=(r(ndistinct))			
	
}

* Specification #2
* Full model: Fixed effects for individual and time, clustering by individual
putexcel set "${tables}/20241028-results_step2_t1.xlsx", sheet(spec2_model3, replace) modify
putexcel A1=("Outcome") B1=("Mean") C1=("SD.") D1=("Coeff.") E1=("S.E.") F1=("p-value") G1=("N") H1=("R2")	
local s = 0			
foreach y of global outcomes_gnral {

    local s = `s'+1
	local h = `s'+(6*(`s'-1))	
	dis in red "`s' --- `y'"
	reghdfe `y' treatment_1 post_law_t* interaction_t*, absorb(personabasicaid fe_month) vce(cluster personabasicaid)	
	forvalues i=1/6 {	
	local p = `h'+`i'		
	dis in red "`s' --- `h' --- `y' --- `p'"
		
			putexcel A`p'=("`y' - interaction_t`i'") 
	
			sum `y' if treatment_1==1 & post_law==0 
			putexcel B`p'=(r(mean))	
			putexcel C`p'=(r(sd))			
			
			putexcel D`p'=(_b[interaction_t`i'])	
			putexcel E`p'=(_se[interaction_t`i'])			
			test _b[interaction_t`i'] = 0
			putexcel F`p'=(r(p))
			putexcel G`p'=(e(N))
			putexcel H`p'=(e(r2))

			distinct personabasicaid if treatment_1!=.
			putexcel I`s'=(r(ndistinct))			
			distinct month if treatment_1!=.
			putexcel j`s'=(r(ndistinct))						
	
	}	
}


* Specification #3
global cond_f "absorb(personabasicaid fe_month) vce(cluster personabasicaid)"


* Average ----------------------------------------------------------------------
* n_consultas
est clear
reghdfe n_consultas treatment_1 post_law_t* interaction_t*, $cond_f
est store est_n_consultas

coefplot ///
 (est_n_consultas, ///
 rename(interaction_t1 = 1 & interaction_t2 = 2 & interaction_t3 = 3 & ///
        interaction_t4 = 4 & interaction_t5 = 5 & interaction_t6 = 6)) , ///
			drop(_cons treatment_1 post_law*) ///
		title("{bf:Consultations}", size(small)) ///
		subtitle("", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#5, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient", size(small)) ///
		xtitle("Months After the Policy Change", size(small) height(4.5)) ///
		note("") name("n_cons", replace)

* n_hospitalizaciones
est clear
reghdfe n_hospitalizaciones treatment_1 post_law_t* interaction_t*, $cond_f
est store est_n_hospitalizaciones

coefplot ///
 (est_n_hospitalizaciones, ///
 rename(interaction_t1 = 1 & interaction_t2 = 2 & interaction_t3 = 3 & ///
        interaction_t4 = 4 & interaction_t5 = 5 & interaction_t6 = 6)) , ///
			drop(_cons treatment_1 post_law*) ///
		title("{bf:Hospitalizations}", size(small)) ///
		subtitle("", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#5, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient", size(small)) ///
		xtitle("Months After the Policy Change", size(small) height(4.5)) ///
		note("") name("n_hosp", replace)

* n_procedimientos
est clear
reghdfe n_procedimientos treatment_1 post_law_t* interaction_t*, $cond_f
est store est_n_procedimientos

coefplot ///
 (est_n_procedimientos, ///
 rename(interaction_t1 = 1 & interaction_t2 = 2 & interaction_t3 = 3 & ///
        interaction_t4 = 4 & interaction_t5 = 5 & interaction_t6 = 6)) , ///
			drop(_cons treatment_1 post_law*) ///
		title("{bf:Procedures}", size(small)) ///
		subtitle("", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#5, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient", size(small)) ///
		xtitle("Months After the Policy Change", size(small) height(4.5)) ///
		note("") name("n_proc", replace)

* n_urgencias
est clear
reghdfe n_urgencias treatment_1 post_law_t* interaction_t*, $cond_f
est store est_n_urgencias

coefplot ///
 (est_n_urgencias, ///
 rename(interaction_t1 = 1 & interaction_t2 = 2 & interaction_t3 = 3 & ///
        interaction_t4 = 4 & interaction_t5 = 5 & interaction_t6 = 6)) , ///
			drop(_cons treatment_1 post_law*) ///
		title("{bf:Emergencies}", size(small)) ///
		subtitle("", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#5, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient", size(small)) ///
		xtitle("Months After the Policy Change", size(small) height(4.5)) ///
		note("") name("n_emer", replace)


graph combine n_cons n_hosp n_proc n_emer, ycommon ///
		title("Impact on Per Capita Outcomes", size(small)) ///
		subtitle("{bf:Elegible at a lower discount (<=30.56) vs. Not eligible (40-45]}", size(small)) ///
		graphr(c(white)m(t+0 l+0)) plotregion(margin(b=0)) ///
	    note("Significance level: 5%") name("fig1", replace)
graph export "${graphs}/20241028 - treatment1_n_outcomes.png", replace


graph drop *		
		
* n_consultas gral
est clear
reghdfe n_cons_gral treatment_1 post_law_t* interaction_t*, $cond_f
est store est_n_cons_gral

coefplot ///
 (est_n_cons_gral, ///
 rename(interaction_t1 = 1 & interaction_t2 = 2 & interaction_t3 = 3 & ///
        interaction_t4 = 4 & interaction_t5 = 5 & interaction_t6 = 6)) , ///
			drop(_cons treatment_1 post_law*) ///
		title("{bf:General consultations}", size(small)) ///
		subtitle("", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#5, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient", size(small)) ///
		xtitle("Months After the Policy Change", size(small) height(4.5)) ///
		name("n_cons_gral", replace)

* n_consultas esp
est clear
reghdfe n_cons_esp treatment_1 post_law_t* interaction_t*, $cond_f
est store est_n_cons_esp

coefplot ///
 (est_n_cons_esp, ///
 rename(interaction_t1 = 1 & interaction_t2 = 2 & interaction_t3 = 3 & ///
        interaction_t4 = 4 & interaction_t5 = 5 & interaction_t6 = 6)) , ///
			drop(_cons treatment_1 post_law*) ///
		title("{bf:Specialist consultation}", size(small)) ///
		subtitle("", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#5, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient", size(small)) ///
		xtitle("Months After the Policy Change", size(small) height(4.5)) ///
		name("n_cons_esp", replace)

* n_consultas primera
est clear
reghdfe n_cons_primera treatment_1 post_law_t* interaction_t*, $cond_f
est store est_n_cons_primera

coefplot ///
 (est_n_cons_primera, ///
 rename(interaction_t1 = 1 & interaction_t2 = 2 & interaction_t3 = 3 & ///
        interaction_t4 = 4 & interaction_t5 = 5 & interaction_t6 = 6)) , ///
			drop(_cons treatment_1 post_law*) ///
		title("{bf:First-time consultation}", size(small)) ///
		subtitle("", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#5, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient", size(small)) ///
		xtitle("Months After the Policy Change", size(small) height(4.5)) ///
		name("n_cons_primera", replace)		
		
* n_consultas control
est clear
reghdfe n_cons_control treatment_1 post_law_t* interaction_t*, $cond_f
est store est_n_cons_control

coefplot ///
 (est_n_cons_control, ///
 rename(interaction_t1 = 1 & interaction_t2 = 2 & interaction_t3 = 3 & ///
        interaction_t4 = 4 & interaction_t5 = 5 & interaction_t6 = 6)) , ///
			drop(_cons treatment_1 post_law*) ///
		title("{bf:Follow-up or control consultation}", size(small)) ///
		subtitle("", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#5, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient", size(small)) ///
		xtitle("Months After the Policy Change", size(small) height(4.5)) ///
		name("n_cons_control", replace)

	
graph combine n_cons_gral n_cons_esp n_cons_primera n_cons_control, ycommon ///
		title("Disaggregated Impacts on Per Capita Consultation Types", size(small)) ///
		subtitle("{bf:Elegible at a lower discount (<=30.56) vs. Not eligible (40-45]}", size(small)) ///
		graphr(c(white)m(t+0 l+0)) plotregion(margin(b=0)) ///
	    note("Significance level: 5%") name("fig1", replace)	
graph export "${graphs}/20241028 - treatment1_n_consultations.png", replace		

graph drop *		
		
* Probability ------------------------------------------------------------------
* d_consultas
est clear
reghdfe d_consultas treatment_1 post_law_t* interaction_t*, $cond_f
est store est_d_consultas

coefplot ///
 (est_d_consultas, ///
 rename(interaction_t1 = 1 & interaction_t2 = 2 & interaction_t3 = 3 & ///
        interaction_t4 = 4 & interaction_t5 = 5 & interaction_t6 = 6)) , ///
			drop(_cons treatment_1 post_law*) ///
		title("{bf:Consultations}", size(small)) ///
		subtitle("", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#5, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient", size(small)) ///
		xtitle("Months After the Policy Change", size(small) height(4.5)) ///
		note("") name("d_cons", replace)

* d_hospitalizaciones
est clear
reghdfe d_hospitalizaciones treatment_1 post_law_t* interaction_t*, $cond_f
est store est_d_hospitalizaciones

coefplot ///
 (est_d_hospitalizaciones, ///
 rename(interaction_t1 = 1 & interaction_t2 = 2 & interaction_t3 = 3 & ///
        interaction_t4 = 4 & interaction_t5 = 5 & interaction_t6 = 6)) , ///
			drop(_cons treatment_1 post_law*) ///
		title("{bf:Hospitalizations}", size(small)) ///
		subtitle("", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#5, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient", size(small)) ///
		xtitle("Months After the Policy Change", size(small) height(4.5)) ///
		note("") name("d_hosp", replace)

* d_procedimientos
est clear
reghdfe d_procedimientos treatment_1 post_law_t* interaction_t*, $cond_f
est store est_d_procedimientos

coefplot ///
 (est_d_procedimientos, ///
 rename(interaction_t1 = 1 & interaction_t2 = 2 & interaction_t3 = 3 & ///
        interaction_t4 = 4 & interaction_t5 = 5 & interaction_t6 = 6)) , ///
			drop(_cons treatment_1 post_law*) ///
		title("{bf:Procedures}", size(small)) ///
		subtitle("", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#5, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient", size(small)) ///
		xtitle("Months After the Policy Change", size(small) height(4.5)) ///
		note("") name("d_proc", replace)

* d_urgencias
est clear
reghdfe d_urgencias treatment_1 post_law_t* interaction_t*, $cond_f
est store est_d_urgencias

coefplot ///
 (est_d_urgencias, ///
 rename(interaction_t1 = 1 & interaction_t2 = 2 & interaction_t3 = 3 & ///
        interaction_t4 = 4 & interaction_t5 = 5 & interaction_t6 = 6)) , ///
			drop(_cons treatment_1 post_law*) ///
		title("{bf:Emergencies}", size(small)) ///
		subtitle("", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#5, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient", size(small)) ///
		xtitle("Months After the Policy Change", size(small) height(4.5)) ///
		note("") name("d_emer", replace)


graph combine d_cons d_hosp d_proc d_emer, ycommon ///
		title("Impact on Probability Outcomes", size(small)) ///
		subtitle("{bf:Elegible at a lower discount (<=30.56) vs. Not eligible (40-45]}", size(small)) ///
		graphr(c(white)m(t+0 l+0)) plotregion(margin(b=0)) ///
	    note("Significance level: 5%") name("fig1", replace)
graph export "${graphs}/20241028 - treatment1_d_outcomes.png", replace


graph drop *		
		
* d_consultas gral
est clear
reghdfe d_cons_gral treatment_1 post_law_t* interaction_t*, $cond_f
est store est_d_cons_gral

coefplot ///
 (est_d_cons_gral, ///
 rename(interaction_t1 = 1 & interaction_t2 = 2 & interaction_t3 = 3 & ///
        interaction_t4 = 4 & interaction_t5 = 5 & interaction_t6 = 6)) , ///
			drop(_cons treatment_1 post_law*) ///
		title("{bf:General consultations}", size(small)) ///
		subtitle("", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#5, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient", size(small)) ///
		xtitle("Months After the Policy Change", size(small) height(4.5)) ///
		name("d_cons_gral", replace)

* d_consultas esp
est clear
reghdfe  d_cons_esp treatment_1 post_law_t* interaction_t*, $cond_f
est store est_d_cons_esp

coefplot ///
 (est_d_cons_esp, ///
 rename(interaction_t1 = 1 & interaction_t2 = 2 & interaction_t3 = 3 & ///
        interaction_t4 = 4 & interaction_t5 = 5 & interaction_t6 = 6)) , ///
			drop(_cons treatment_1 post_law*) ///
		title("{bf:Specialist consultation}", size(small)) ///
		subtitle("", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#5, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient", size(small)) ///
		xtitle("Months After the Policy Change", size(small) height(4.5)) ///
		name("d_cons_esp", replace)

*  d_consultas primera
est clear
reghdfe  d_cons_primera treatment_1 post_law_t* interaction_t*, $cond_f
est store est_d_cons_primera

coefplot ///
 (est_d_cons_primera, ///
 rename(interaction_t1 = 1 & interaction_t2 = 2 & interaction_t3 = 3 & ///
        interaction_t4 = 4 & interaction_t5 = 5 & interaction_t6 = 6)) , ///
			drop(_cons treatment_1 post_law*) ///
		title("{bf:First-time consultation}", size(small)) ///
		subtitle("", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#5, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient", size(small)) ///
		xtitle("Months After the Policy Change", size(small) height(4.5)) ///
		name("d_cons_primera", replace)		
		
*  d_consultas control
est clear
reghdfe  d_cons_control treatment_1 post_law_t* interaction_t*, $cond_f
est store est_d_cons_control

coefplot ///
 (est_d_cons_control, ///
 rename(interaction_t1 = 1 & interaction_t2 = 2 & interaction_t3 = 3 & ///
        interaction_t4 = 4 & interaction_t5 = 5 & interaction_t6 = 6)) , ///
			drop(_cons treatment_1 post_law*) ///
		title("{bf:Follow-up or control consultation}", size(small)) ///
		subtitle("", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#5, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient", size(small)) ///
		xtitle("Months After the Policy Change", size(small) height(4.5)) ///
		name("d_cons_control", replace)

	
graph combine  d_cons_gral  d_cons_esp  d_cons_primera  d_cons_control, ycommon ///
		title("Disaggregated Impacts on Probability Consultation Types", size(small)) ///
		subtitle("{bf:Elegible at a lower discount (<=30.56) vs. Not eligible (40-45]}", size(small)) ///
		graphr(c(white)m(t+0 l+0)) plotregion(margin(b=0)) ///
	    note("Significance level: 5%") name("fig1", replace)	
graph export "${graphs}/20241028 - treatment1_d_consultations.png", replace		

graph drop *	

********************************************************************************
**# ***************** ESTIMATIONS TREATMENT 2 **********************************
********************************************************************************

global outcomes_gnral "d_consultas d_cons_gral d_cons_esp d_cons_primera d_cons_control n_consultas n_cons_gral n_cons_esp n_cons_primera n_cons_control"
	
* Specification #1
* Full model: Fixed effects for individual and time, clustering by individual
putexcel set "${tables}/20241028-results_step2_t2.xlsx", sheet(spec1_model3, replace) modify
putexcel A1=("Outcome") B1=("Mean") C1=("SD.") D1=("Coeff.") E1=("S.E.") F1=("p-value") G1=("N") H1=("R2")	

local s = 1			
foreach y of global outcomes_gnral {

	local s = `s'+1	
	dis in red "`s' --- `y'"
	reghdfe `y' treatment_2 post_law interaction2, absorb(personabasicaid fe_month) vce(cluster personabasicaid)	
			putexcel A`s'=("`y'") 
	
			sum `y' if treatment_2==1 & post_law==0
			putexcel B`s'=(r(mean))	
			putexcel C`s'=(r(sd))			
			
			putexcel D`s'=(_b[interaction2])	
			putexcel E`s'=(_se[interaction2])			
			test _b[interaction2] = 0
			putexcel F`s'=(r(p))
			putexcel G`s'=(e(N))
			putexcel H`s'=(e(r2))	
			
			distinct personabasicaid if treatment_2!=.
			putexcel I`s'=(r(ndistinct))			
			distinct month if treatment_2!=.
			putexcel j`s'=(r(ndistinct))			
	
}

* Specification #2
* Full model: Fixed effects for individual and time, clustering by individual
putexcel set "${tables}/20241028-results_step2_t2.xlsx", sheet(spec2_model3, replace) modify
putexcel A1=("Outcome") B1=("Mean") C1=("SD.") D1=("Coeff.") E1=("S.E.") F1=("p-value") G1=("N") H1=("R2")	
local s = 0			
foreach y of global outcomes_gnral {

    local s = `s'+1
	local h = `s'+(6*(`s'-1))	
	dis in red "`s' --- `y'"
	reghdfe `y' treatment_2 post_law_t* interaction2_t*, absorb(personabasicaid fe_month) vce(cluster personabasicaid)	
	forvalues i=1/6 {	
	local p = `h'+`i'		
	dis in red "`s' --- `h' --- `y' --- `p'"
		
			putexcel A`p'=("`y' - interaction2_t`i'") 
	
			sum `y' if treatment_2==1 & post_law==0 
			putexcel B`p'=(r(mean))	
			putexcel C`p'=(r(sd))			
			
			putexcel D`p'=(_b[interaction2_t`i'])	
			putexcel E`p'=(_se[interaction2_t`i'])			
			test _b[interaction2_t`i'] = 0
			putexcel F`p'=(r(p))
			putexcel G`p'=(e(N))
			putexcel H`p'=(e(r2))

			distinct personabasicaid if treatment_2!=.
			putexcel I`s'=(r(ndistinct))			
			distinct month if treatment_2!=.
			putexcel j`s'=(r(ndistinct))						
	
	}	
}


* Specification #3

global cond_f "absorb(personabasicaid fe_month) vce(cluster personabasicaid)"

* Average ----------------------------------------------------------------------
* n_consultas
est clear
reghdfe n_consultas treatment_2 post_law_t* interaction2_t*, $cond_f
est store est_n_consultas

coefplot ///
 (est_n_consultas, ///
 rename(interaction2_t1 = 1 & interaction2_t2 = 2 & interaction2_t3 = 3 & ///
        interaction2_t4 = 4 & interaction2_t5 = 5 & interaction2_t6 = 6)) , ///
			drop(_cons treatment_2 post_law*) ///
		title("{bf:Consultations}", size(small)) ///
		subtitle("", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#5, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient", size(small)) ///
		xtitle("Months After the Policy Change", size(small) height(4.5)) ///
		note("") name("n_cons", replace)

* n_hospitalizaciones
est clear
reghdfe n_hospitalizaciones treatment_2 post_law_t* interaction2_t*, $cond_f
est store est_n_hospitalizaciones

coefplot ///
 (est_n_hospitalizaciones, ///
 rename(interaction2_t1 = 1 & interaction2_t2 = 2 & interaction2_t3 = 3 & ///
        interaction2_t4 = 4 & interaction2_t5 = 5 & interaction2_t6 = 6)) , ///
			drop(_cons treatment_2 post_law*) ///
		title("{bf:Hospitalizations}", size(small)) ///
		subtitle("", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#5, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient", size(small)) ///
		xtitle("Months After the Policy Change", size(small) height(4.5)) ///
		note("") name("n_hosp", replace)


* n_procedimientos
est clear
reghdfe n_procedimientos treatment_2 post_law_t* interaction2_t*, $cond_f
est store est_n_procedimientos

coefplot ///
 (est_n_procedimientos, ///
 rename(interaction2_t1 = 1 & interaction2_t2 = 2 & interaction2_t3 = 3 & ///
        interaction2_t4 = 4 & interaction2_t5 = 5 & interaction2_t6 = 6)) , ///
			drop(_cons treatment_2 post_law*) ///
		title("{bf:Procedures}", size(small)) ///
		subtitle("", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#5, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient", size(small)) ///
		xtitle("Months After the Policy Change", size(small) height(4.5)) ///
		note("") name("n_proc", replace)

* n_urgencias
est clear
reghdfe n_urgencias treatment_2 post_law_t* interaction2_t*, $cond_f
est store est_n_urgencias

coefplot ///
 (est_n_urgencias, ///
 rename(interaction2_t1 = 1 & interaction2_t2 = 2 & interaction2_t3 = 3 & ///
        interaction2_t4 = 4 & interaction2_t5 = 5 & interaction2_t6 = 6)) , ///
			drop(_cons treatment_2 post_law*) ///
		title("{bf:Emergencies}", size(small)) ///
		subtitle("", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#5, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient", size(small)) ///
		xtitle("Months After the Policy Change", size(small) height(4.5)) ///
		note("") name("n_emer", replace)

graph combine n_cons n_hosp n_proc n_emer, ycommon ///
		title("Impact on Per Capita Outcomes", size(small)) ///
		subtitle("{bf:Lost eligibility (30.56 - 40] vs. Not eligible (40-45]}", size(small)) ///
		graphr(c(white)m(t+0 l+0)) plotregion(margin(b=0)) ///
	    note("Significance level: 5%") name("fig1", replace)
graph export "${graphs}/20241028 - treatment2_n_outcomes.png", replace


graph drop *	

		
* n_consultas gral
est clear
reghdfe n_cons_gral  treatment_2 post_law_t*  interaction2_t*, $cond_f
est store est_n_cons_gral

coefplot ///
 (est_n_cons_gral, ///
 rename( interaction2_t1 = 1 &  interaction2_t2 = 2 &  interaction2_t3 = 3 & ///
         interaction2_t4 = 4 &  interaction2_t5 = 5 &  interaction2_t6 = 6)) , ///
			drop(_cons  treatment_2 post_law*) ///
		title("{bf:General consultations}", size(small)) ///
		subtitle("", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#5, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient", size(small)) ///
		xtitle("Months After the Policy Change", size(small) height(4.5)) ///
		name("n_cons_gral", replace)

* n_consultas esp
est clear
reghdfe n_cons_esp  treatment_2 post_law_t*  interaction2_t*, $cond_f
est store est_n_cons_esp

coefplot ///
 (est_n_cons_esp, ///
 rename( interaction2_t1 = 1 &  interaction2_t2 = 2 &  interaction2_t3 = 3 & ///
         interaction2_t4 = 4 &  interaction2_t5 = 5 &  interaction2_t6 = 6)) , ///
			drop(_cons  treatment_2 post_law*) ///
		title("{bf:Specialist consultation}", size(small)) ///
		subtitle("", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#5, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient", size(small)) ///
		xtitle("Months After the Policy Change", size(small) height(4.5)) ///
		name("n_cons_esp", replace)

* n_consultas primera
est clear
reghdfe n_cons_primera  treatment_2 post_law_t*  interaction2_t*, $cond_f
est store est_n_cons_primera

coefplot ///
 (est_n_cons_primera, ///
 rename( interaction2_t1 = 1 &  interaction2_t2 = 2 &  interaction2_t3 = 3 & ///
         interaction2_t4 = 4 &  interaction2_t5 = 5 &  interaction2_t6 = 6)) , ///
			drop(_cons  treatment_2 post_law*) ///
		title("{bf:First-time consultation}", size(small)) ///
		subtitle("", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#5, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient", size(small)) ///
		xtitle("Months After the Policy Change", size(small) height(4.5)) ///
		name("n_cons_primera", replace)		
		
* n_consultas control
est clear
reghdfe n_cons_control  treatment_2 post_law_t*  interaction2_t*, $cond_f
est store est_n_cons_control

coefplot ///
 (est_n_cons_control, ///
 rename( interaction2_t1 = 1 &  interaction2_t2 = 2 &  interaction2_t3 = 3 & ///
         interaction2_t4 = 4 &  interaction2_t5 = 5 &  interaction2_t6 = 6)) , ///
			drop(_cons  treatment_2 post_law*) ///
		title("{bf:Follow-up or control consultation}", size(small)) ///
		subtitle("", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#5, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient", size(small)) ///
		xtitle("Months After the Policy Change", size(small) height(4.5)) ///
		name("n_cons_control", replace)

	
graph combine n_cons_gral n_cons_esp n_cons_primera n_cons_control, ycommon ///
		title("Disaggregated Impacts on Per Capita Consultation Types", size(small)) ///
		subtitle("{bf:Lost eligibility (30.56 - 40] vs. Not eligible (40-45]}", size(small)) ///
		graphr(c(white)m(t+0 l+0)) plotregion(margin(b=0)) ///
	    note("Significance level: 5%") name("fig1", replace)	
graph export "${graphs}/20241028 - treatment2_n_consultations.png", replace		

graph drop *		

		
* Probability ------------------------------------------------------------------
* d_consultas
est clear
reghdfe d_consultas treatment_2 post_law_t* interaction2_t*,  $cond_f
est store est_d_consultas

coefplot ///
 (est_d_consultas, ///
 rename(interaction2_t1 = 1 & interaction2_t2 = 2 & interaction2_t3 = 3 & ///
        interaction2_t4 = 4 & interaction2_t5 = 5 & interaction2_t6 = 6)) , ///
			drop(_cons treatment_2 post_law*) ///
		title("{bf:Consultations}", size(small)) ///
		subtitle("", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#5, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient", size(small)) ///
		xtitle("Months After the Policy Change", size(small) height(4.5)) ///
		note("") name("d_cons", replace)


* d_hospitalizaciones
est clear
reghdfe d_hospitalizaciones treatment_2 post_law_t* interaction2_t*, $cond_f
est store est_d_hospitalizaciones

coefplot ///
 (est_d_hospitalizaciones, ///
 rename(interaction2_t1 = 1 & interaction2_t2 = 2 & interaction2_t3 = 3 & ///
        interaction2_t4 = 4 & interaction2_t5 = 5 & interaction2_t6 = 6)) , ///
			drop(_cons treatment_2 post_law*) ///
		title("{bf:Hospitalizations}", size(small)) ///
		subtitle("", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#5, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient", size(small)) ///
		xtitle("Months After the Policy Change", size(small) height(4.5)) ///
		note("") name("d_hosp", replace)		

* d_procedimientos
est clear
reghdfe d_procedimientos treatment_2 post_law_t* interaction2_t*, $cond_f
est store est_d_procedimientos

coefplot ///
 (est_d_procedimientos, ///
 rename(interaction2_t1 = 1 & interaction2_t2 = 2 & interaction2_t3 = 3 & ///
        interaction2_t4 = 4 & interaction2_t5 = 5 & interaction2_t6 = 6)) , ///
			drop(_cons treatment_2 post_law*) ///
		title("{bf:Procedures}", size(small)) ///
		subtitle("", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#5, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient", size(small)) ///
		xtitle("Months After the Policy Change", size(small) height(4.5)) ///
		note("") name("d_proc", replace)

* d_urgencias
est clear
reghdfe d_urgencias treatment_2 post_law_t* interaction2_t*, $cond_f
est store est_d_urgencias

coefplot ///
 (est_d_urgencias, ///
 rename(interaction2_t1 = 1 & interaction2_t2 = 2 & interaction2_t3 = 3 & ///
        interaction2_t4 = 4 & interaction2_t5 = 5 & interaction2_t6 = 6)) , ///
			drop(_cons treatment_2 post_law*) ///
		title("{bf:Emergencies}", size(small)) ///
		subtitle("", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#5, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient", size(small)) ///
		xtitle("Months After the Policy Change", size(small) height(4.5)) ///
		note("") name("d_emer", replace)

graph combine d_cons d_hosp d_proc d_emer, ycommon ///
		title("Impact on Probability Outcomes", size(small)) ///
		subtitle("{bf:Lost eligibility (30.56 - 40] vs. Not eligible (40-45]}", size(small)) ///
		graphr(c(white)m(t+0 l+0)) plotregion(margin(b=0)) ///
	    note("Significance level: 5%") name("fig1", replace)
graph export "${graphs}/20241028 - treatment2_d_outcomes.png", replace

graph drop *	



* d_consultas gral
est clear
reghdfe d_cons_gral  treatment_2 post_law_t*  interaction2_t*, $cond_f
est store est_d_cons_gral

coefplot ///
 (est_d_cons_gral, ///
 rename( interaction2_t1 = 1 &  interaction2_t2 = 2 &  interaction2_t3 = 3 & ///
         interaction2_t4 = 4 &  interaction2_t5 = 5 &  interaction2_t6 = 6)) , ///
			drop(_cons  treatment_2 post_law*) ///
		title("{bf:General consultations}", size(small)) ///
		subtitle("", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#5, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient", size(small)) ///
		xtitle("Months After the Policy Change", size(small) height(4.5)) ///
		name(" d_cons_gral", replace)

* d_consultas esp
est clear
reghdfe  d_cons_esp  treatment_2 post_law_t*  interaction2_t*, $cond_f
est store est_d_cons_esp

coefplot ///
 (est_d_cons_esp, ///
 rename( interaction2_t1 = 1 &  interaction2_t2 = 2 &  interaction2_t3 = 3 & ///
         interaction2_t4 = 4 &  interaction2_t5 = 5 &  interaction2_t6 = 6)) , ///
			drop(_cons  treatment_2 post_law*) ///
		title("{bf:Specialist consultation}", size(small)) ///
		subtitle("", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#5, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient", size(small)) ///
		xtitle("Months After the Policy Change", size(small) height(4.5)) ///
		name(" d_cons_esp", replace)

*  d_consultas primera
est clear
reghdfe  d_cons_primera  treatment_2 post_law_t*  interaction2_t*, $cond_f
est store est_d_cons_primera

coefplot ///
 (est_d_cons_primera, ///
 rename( interaction2_t1 = 1 &  interaction2_t2 = 2 &  interaction2_t3 = 3 & ///
         interaction2_t4 = 4 &  interaction2_t5 = 5 &  interaction2_t6 = 6)) , ///
			drop(_cons  treatment_2 post_law*) ///
		title("{bf:First-time consultation}", size(small)) ///
		subtitle("", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#5, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient", size(small)) ///
		xtitle("Months After the Policy Change", size(small) height(4.5)) ///
		name(" d_cons_primera", replace)		
		
*  d_consultas control
est clear
reghdfe  d_cons_control  treatment_2 post_law_t*  interaction2_t*, $cond_f
est store est_d_cons_control

coefplot ///
 (est_d_cons_control, ///
 rename( interaction2_t1 = 1 &  interaction2_t2 = 2 &  interaction2_t3 = 3 & ///
         interaction2_t4 = 4 &  interaction2_t5 = 5 &  interaction2_t6 = 6)) , ///
			drop(_cons  treatment_2 post_law*) ///
		title("{bf:Follow-up or control consultation}", size(small)) ///
		subtitle("", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#5, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient", size(small)) ///
		xtitle("Months After the Policy Change", size(small) height(4.5)) ///
		name(" d_cons_control", replace)

	
graph combine  d_cons_gral  d_cons_esp  d_cons_primera  d_cons_control, ycommon ///
		title("Disaggregated Impacts on Probability Consultation Types", size(small)) ///
		subtitle("{bf:Lost eligibility (30.56 - 40] vs. Not eligible (40-45]}", size(small)) ///
		graphr(c(white)m(t+0 l+0)) plotregion(margin(b=0)) ///
	    note("Significance level: 5%") name("fig1", replace)	
graph export "${graphs}/20241028 - treatment2_d_consultations.png", replace		

graph drop *	



































