/*
DIFFERENCES IN DIFFERENCES FOR GENERAL OUTCOMES
	Creation date: 22-Oct-2024
	Author: Brayan Pineda
	Last modification: 22-Oct-2024 by Brayan Pineda
	Objective: First estimations set for all the general outcomes.

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
cap mkdir "${tables}/20241022 - t_results"
cap mkdir "${graphs}/20241022 - g_results"

global tables      "${tables}/20241022 - t_results"
global graphs      "${graphs}/20241022 - g_results"



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

/*
global outcomes_gnral "n_visitas_rips d_visitas_rips n_consultas d_consultas n_hospitalizaciones d_hospitalizaciones n_procedimientos d_procedimientos n_urgencias d_urgencias"

	
* Specification #1	
* Full model: Fixed effects for individual and time, clustering by individual
putexcel set "${tables}/20241022-results_step1_t1.xlsx", sheet(spec1_model3, replace) modify
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
putexcel set "${tables}/20241022-results_step1_t1.xlsx", sheet(spec2_model3, replace) modify
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

* n_visitas_rips
est clear	
reghdfe n_visitas_rips treatment_1 post_law_t* interaction_t*, $cond_f
est store est_n_visitas_rips		

	coefplot ///
	 (est_n_visitas_rips, ///
 rename(interaction_t1 = 1 & interaction_t2 = 2 & interaction_t3 = 3 & ///
        interaction_t4 = 4 & interaction_t5 = 5 & interaction_t6 = 6)) , ///
			drop(_cons treatment_1 post_law*) ///
		title("Impact on Rips per capita", size(small)) ///
		subtitle("{bf:Elegible at a lower discount (<=30.56) vs. Not eligible (40-45]}", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#10, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient") ///
		xtitle("Months After the Policy Change", height(7.5)) ///
		note("Confidence interval: 5%") name("sc", replace)

graph export "${graphs}/20241022 - treatment1_n_visitas_rips.png", replace

* d_visitas_rips
est clear
reghdfe d_visitas_rips treatment_1 post_law_t* interaction_t*, $cond_f
est store est_d_visitas_rips

coefplot ///
 (est_d_visitas_rips, ///
 rename(interaction_t1 = 1 & interaction_t2 = 2 & interaction_t3 = 3 & ///
        interaction_t4 = 4 & interaction_t5 = 5 & interaction_t6 = 6)) , ///
			drop(_cons treatment_1 post_law*) ///
		title("Impact on Rips probablity", size(small)) ///
		subtitle("{bf:Elegible at a lower discount (<=30.56) vs. Not eligible (40-45]}", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#10, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient") ///
		xtitle("Months After the Policy Change", height(7.5)) ///
		note("Confidence interval: 5%") name("sc", replace)
graph export "${graphs}/20241022 - treatment1_d_visitas_rips.png", replace

* n_consultas
est clear
reghdfe n_consultas treatment_1 post_law_t* interaction_t*, $cond_f
est store est_n_consultas

coefplot ///
 (est_n_consultas, ///
 rename(interaction_t1 = 1 & interaction_t2 = 2 & interaction_t3 = 3 & ///
        interaction_t4 = 4 & interaction_t5 = 5 & interaction_t6 = 6)) , ///
			drop(_cons treatment_1 post_law*) ///
		title("Impact on Consultations per capita", size(small)) ///
		subtitle("{bf:Elegible at a lower discount (<=30.56) vs. Not eligible (40-45]}", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#10, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient") ///
		xtitle("Months After the Policy Change", height(7.5)) ///
		note("Confidence interval: 5%") name("sc", replace)
graph export "${graphs}/20241022 - treatment1_n_consultas.png", replace

* d_consultas
est clear
reghdfe d_consultas treatment_1 post_law_t* interaction_t*,  $cond_f
est store est_d_consultas

coefplot ///
 (est_d_consultas, ///
 rename(interaction_t1 = 1 & interaction_t2 = 2 & interaction_t3 = 3 & ///
        interaction_t4 = 4 & interaction_t5 = 5 & interaction_t6 = 6)) , ///
			drop(_cons treatment_1 post_law*) ///
		title("Impact on Consultations Probability", size(small)) ///
		subtitle("{bf:Elegible at a lower discount (<=30.56) vs. Not eligible (40-45]}", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#10, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient") ///
		xtitle("Months After the Policy Change", height(7.5)) ///
		note("Confidence interval: 5%") name("sc", replace)
graph export "${graphs}/20241022 - treatment1_d_consultas.png", replace

* n_hospitalizaciones
est clear
reghdfe n_hospitalizaciones treatment_1 post_law_t* interaction_t*, $cond_f
est store est_n_hospitalizaciones

coefplot ///
 (est_n_hospitalizaciones, ///
 rename(interaction_t1 = 1 & interaction_t2 = 2 & interaction_t3 = 3 & ///
        interaction_t4 = 4 & interaction_t5 = 5 & interaction_t6 = 6)) , ///
			drop(_cons treatment_1 post_law*) ///
		title("Impact on Hospitalizations per capita", size(small)) ///
		subtitle("{bf:Elegible at a lower discount (<=30.56) vs. Not eligible (40-45]}", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#10, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient") ///
		xtitle("Months After the Policy Change", height(7.5)) ///
		note("Confidence interval: 5%") name("sc", replace)
graph export "${graphs}/20241022 - treatment1_n_hospitalizaciones.png", replace

* d_hospitalizaciones
est clear
reghdfe d_hospitalizaciones treatment_1 post_law_t* interaction_t*, $cond_f
est store est_d_hospitalizaciones

coefplot ///
 (est_d_hospitalizaciones, ///
 rename(interaction_t1 = 1 & interaction_t2 = 2 & interaction_t3 = 3 & ///
        interaction_t4 = 4 & interaction_t5 = 5 & interaction_t6 = 6)) , ///
			drop(_cons treatment_1 post_law*) ///
		title("Impact on Hospitalizations Probability", size(small)) ///
		subtitle("{bf:Elegible at a lower discount (<=30.56) vs. Not eligible (40-45]}", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#10, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient") ///
		xtitle("Months After the Policy Change", height(7.5)) ///
		note("Confidence interval: 5%") name("sc", replace)
graph export "${graphs}/20241022 - treatment1_d_hospitalizaciones.png", replace
		
* n_procedimientos
est clear
reghdfe n_procedimientos treatment_1 post_law_t* interaction_t*, $cond_f
est store est_n_procedimientos

coefplot ///
 (est_n_procedimientos, ///
 rename(interaction_t1 = 1 & interaction_t2 = 2 & interaction_t3 = 3 & ///
        interaction_t4 = 4 & interaction_t5 = 5 & interaction_t6 = 6)) , ///
			drop(_cons treatment_1 post_law*) ///
		title("Impact on Procedures per capita", size(small)) ///
		subtitle("{bf:Elegible at a lower discount (<=30.56) vs. Not eligible (40-45]}", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#10, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient") ///
		xtitle("Months After the Policy Change", height(7.5)) ///
		note("Confidence interval: 5%") name("sc", replace)
graph export "${graphs}/20241022 - treatment1_n_procedimientos.png", replace

* d_procedimientos
est clear
reghdfe d_procedimientos treatment_1 post_law_t* interaction_t*, $cond_f
est store est_d_procedimientos

coefplot ///
 (est_d_procedimientos, ///
 rename(interaction_t1 = 1 & interaction_t2 = 2 & interaction_t3 = 3 & ///
        interaction_t4 = 4 & interaction_t5 = 5 & interaction_t6 = 6)) , ///
			drop(_cons treatment_1 post_law*) ///
		title("Impact on Procedures Probability", size(small)) ///
		subtitle("{bf:Elegible at a lower discount (<=30.56) vs. Not eligible (40-45]}", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#10, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient") ///
		xtitle("Months After the Policy Change", height(7.5)) ///
		note("Confidence interval: 5%") name("sc", replace)
graph export "${graphs}/20241022 - treatment1_d_procedimientos.png", replace

* n_urgencias
est clear
reghdfe n_urgencias treatment_1 post_law_t* interaction_t*, $cond_f
est store est_n_urgencias

coefplot ///
 (est_n_urgencias, ///
 rename(interaction_t1 = 1 & interaction_t2 = 2 & interaction_t3 = 3 & ///
        interaction_t4 = 4 & interaction_t5 = 5 & interaction_t6 = 6)) , ///
			drop(_cons treatment_1 post_law*) ///
		title("Impact on Emergencies per capita", size(small)) ///
		subtitle("{bf:Elegible at a lower discount (<=30.56) vs. Not eligible (40-45]}", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#10, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient") ///
		xtitle("Months After the Policy Change", height(7.5)) ///
		note("Confidence interval: 5%") name("sc", replace)
graph export "${graphs}/20241022 - treatment1_n_urgencias.png", replace

* d_urgencias
est clear
reghdfe d_urgencias treatment_1 post_law_t* interaction_t*, $cond_f
est store est_d_urgencias

coefplot ///
 (est_d_urgencias, ///
 rename(interaction_t1 = 1 & interaction_t2 = 2 & interaction_t3 = 3 & ///
        interaction_t4 = 4 & interaction_t5 = 5 & interaction_t6 = 6)) , ///
			drop(_cons treatment_1 post_law*) ///
		title("Impact on Emergencies Probability", size(small)) ///
		subtitle("{bf:Elegible at a lower discount (<=30.56) vs. Not eligible (40-45]}", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#10, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient") ///
		xtitle("Months After the Policy Change", height(7.5)) ///
		note("Confidence interval: 5%") name("sc", replace)
graph export "${graphs}/20241022 - treatment1_d_urgencias.png", replace
*/


********************************************************************************
**# ***************** ESTIMATIONS TREATMENT 2 **********************************
********************************************************************************

global outcomes_gnral "n_visitas_rips d_visitas_rips n_consultas d_consultas n_hospitalizaciones d_hospitalizaciones n_procedimientos d_procedimientos n_urgencias d_urgencias"
	
* Specification #1
* Full model: Fixed effects for individual and time, clustering by individual
putexcel set "${tables}/20241022-results_step1_t2.xlsx", sheet(spec1_model3, replace) modify
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
putexcel set "${tables}/20241022-results_step1_t2.xlsx", sheet(spec2_model3, replace) modify
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

/*
* Specification #3

global cond_f "absorb(personabasicaid fe_month) vce(cluster personabasicaid)"

* n_visitas_rips
est clear	
reghdfe n_visitas_rips treatment_2 post_law_t* interaction2_t*, $cond_f
est store est_n_visitas_rips		

	coefplot ///
	 (est_n_visitas_rips, ///
 rename(interaction2_t1 = 1 & interaction2_t2 = 2 & interaction2_t3 = 3 & ///
        interaction2_t4 = 4 & interaction2_t5 = 5 & interaction2_t6 = 6)) , ///
			drop(_cons treatment_2 post_law*) ///
		title("Impact on Rips per capita", size(small)) ///
		subtitle("{bf:Lost eligibility (30.56 - 40] vs. Not eligible (40-45]}", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#10, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient") ///
		xtitle("Months After the Policy Change", height(7.5)) ///
		note("Confidence interval: 5%") name("sc", replace)

graph export "${graphs}/20241022 - treatment2_n_visitas_rips.png", replace

* d_visitas_rips
est clear
reghdfe d_visitas_rips treatment_2 post_law_t* interaction2_t*, $cond_f
est store est_d_visitas_rips

coefplot ///
 (est_d_visitas_rips, ///
 rename(interaction2_t1 = 1 & interaction2_t2 = 2 & interaction2_t3 = 3 & ///
        interaction2_t4 = 4 & interaction2_t5 = 5 & interaction2_t6 = 6)) , ///
			drop(_cons treatment_2 post_law*) ///
		title("Impact on Rips probablity", size(small)) ///
		subtitle("{bf:Lost eligibility (30.56 - 40] vs. Not eligible (40-45]}", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#10, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient") ///
		xtitle("Months After the Policy Change", height(7.5)) ///
		note("Confidence interval: 5%") name("sc", replace)
graph export "${graphs}/20241022 - treatment2_d_visitas_rips.png", replace

* n_consultas
est clear
reghdfe n_consultas treatment_2 post_law_t* interaction2_t*, $cond_f
est store est_n_consultas

coefplot ///
 (est_n_consultas, ///
 rename(interaction2_t1 = 1 & interaction2_t2 = 2 & interaction2_t3 = 3 & ///
        interaction2_t4 = 4 & interaction2_t5 = 5 & interaction2_t6 = 6)) , ///
			drop(_cons treatment_2 post_law*) ///
		title("Impact on Consultations per capita", size(small)) ///
		subtitle("{bf:Lost eligibility (30.56 - 40] vs. Not eligible (40-45]}", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#10, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient") ///
		xtitle("Months After the Policy Change", height(7.5)) ///
		note("Confidence interval: 5%") name("sc", replace)
graph export "${graphs}/20241022 - treatment2_n_consultas.png", replace

* d_consultas
est clear
reghdfe d_consultas treatment_2 post_law_t* interaction2_t*,  $cond_f
est store est_d_consultas

coefplot ///
 (est_d_consultas, ///
 rename(interaction2_t1 = 1 & interaction2_t2 = 2 & interaction2_t3 = 3 & ///
        interaction2_t4 = 4 & interaction2_t5 = 5 & interaction2_t6 = 6)) , ///
			drop(_cons treatment_2 post_law*) ///
		title("Impact on Consultations Probability", size(small)) ///
		subtitle("{bf:Lost eligibility (30.56 - 40] vs. Not eligible (40-45]}", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#10, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient") ///
		xtitle("Months After the Policy Change", height(7.5)) ///
		note("Confidence interval: 5%") name("sc", replace)
graph export "${graphs}/20241022 - treatment2_d_consultas.png", replace

* n_hospitalizaciones
est clear
reghdfe n_hospitalizaciones treatment_2 post_law_t* interaction2_t*, $cond_f
est store est_n_hospitalizaciones

coefplot ///
 (est_n_hospitalizaciones, ///
 rename(interaction2_t1 = 1 & interaction2_t2 = 2 & interaction2_t3 = 3 & ///
        interaction2_t4 = 4 & interaction2_t5 = 5 & interaction2_t6 = 6)) , ///
			drop(_cons treatment_2 post_law*) ///
		title("Impact on Hospitalizations per capita", size(small)) ///
		subtitle("{bf:Lost eligibility (30.56 - 40] vs. Not eligible (40-45]}", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#10, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient") ///
		xtitle("Months After the Policy Change", height(7.5)) ///
		note("Confidence interval: 5%") name("sc", replace)
graph export "${graphs}/20241022 - treatment2_n_hospitalizaciones.png", replace

* d_hospitalizaciones
est clear
reghdfe d_hospitalizaciones treatment_2 post_law_t* interaction2_t*, $cond_f
est store est_d_hospitalizaciones

coefplot ///
 (est_d_hospitalizaciones, ///
 rename(interaction2_t1 = 1 & interaction2_t2 = 2 & interaction2_t3 = 3 & ///
        interaction2_t4 = 4 & interaction2_t5 = 5 & interaction2_t6 = 6)) , ///
			drop(_cons treatment_2 post_law*) ///
		title("Impact on Hospitalizations Probability", size(small)) ///
		subtitle("{bf:Lost eligibility (30.56 - 40] vs. Not eligible (40-45]}", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#10, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient") ///
		xtitle("Months After the Policy Change", height(7.5)) ///
		note("Confidence interval: 5%") name("sc", replace)
graph export "${graphs}/20241022 - treatment2_d_hospitalizaciones.png", replace
		
* n_procedimientos
est clear
reghdfe n_procedimientos treatment_2 post_law_t* interaction2_t*, $cond_f
est store est_n_procedimientos

coefplot ///
 (est_n_procedimientos, ///
 rename(interaction2_t1 = 1 & interaction2_t2 = 2 & interaction2_t3 = 3 & ///
        interaction2_t4 = 4 & interaction2_t5 = 5 & interaction2_t6 = 6)) , ///
			drop(_cons treatment_2 post_law*) ///
		title("Impact on Procedures per capita", size(small)) ///
		subtitle("{bf:Lost eligibility (30.56 - 40] vs. Not eligible (40-45]}", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#10, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient") ///
		xtitle("Months After the Policy Change", height(7.5)) ///
		note("Confidence interval: 5%") name("sc", replace)
graph export "${graphs}/20241022 - treatment2_n_procedimientos.png", replace

* d_procedimientos
est clear
reghdfe d_procedimientos treatment_2 post_law_t* interaction2_t*, $cond_f
est store est_d_procedimientos

coefplot ///
 (est_d_procedimientos, ///
 rename(interaction2_t1 = 1 & interaction2_t2 = 2 & interaction2_t3 = 3 & ///
        interaction2_t4 = 4 & interaction2_t5 = 5 & interaction2_t6 = 6)) , ///
			drop(_cons treatment_2 post_law*) ///
		title("Impact on Procedures Probability", size(small)) ///
		subtitle("{bf:Lost eligibility (30.56 - 40] vs. Not eligible (40-45]}", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#10, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient") ///
		xtitle("Months After the Policy Change", height(7.5)) ///
		note("Confidence interval: 5%") name("sc", replace)
graph export "${graphs}/20241022 - treatment2_d_procedimientos.png", replace

* n_urgencias
est clear
reghdfe n_urgencias treatment_2 post_law_t* interaction2_t*, $cond_f
est store est_n_urgencias

coefplot ///
 (est_n_urgencias, ///
 rename(interaction2_t1 = 1 & interaction2_t2 = 2 & interaction2_t3 = 3 & ///
        interaction2_t4 = 4 & interaction2_t5 = 5 & interaction2_t6 = 6)) , ///
			drop(_cons treatment_2 post_law*) ///
		title("Impact on Emergencies per capita", size(small)) ///
		subtitle("{bf:Lost eligibility (30.56 - 40] vs. Not eligible (40-45]}", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#10, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient") ///
		xtitle("Months After the Policy Change", height(7.5)) ///
		note("Confidence interval: 5%") name("sc", replace)
graph export "${graphs}/20241022 - treatment2_n_urgencias.png", replace

* d_urgencias
est clear
reghdfe d_urgencias treatment_2 post_law_t* interaction2_t*, $cond_f
est store est_d_urgencias

coefplot ///
 (est_d_urgencias, ///
 rename(interaction2_t1 = 1 & interaction2_t2 = 2 & interaction2_t3 = 3 & ///
        interaction2_t4 = 4 & interaction2_t5 = 5 & interaction2_t6 = 6)) , ///
			drop(_cons treatment_2 post_law*) ///
		title("Impact on Emergencies Probability", size(small)) ///
		subtitle("{bf:Lost eligibility (30.56 - 40] vs. Not eligible (40-45]}", size(small)) ///
		levels(95) vertical nolabels msize(large) ///
		msymbol(diamond) msize(small) mcolor(ebblue) mlwidth(vvvthin) ///
		graphr(c(white)m(t+0 l+0)) bgcolor(white) plotregion(margin(b=0)) ///
		ciopts(recast(rcap rcap) lwidth(medium) lcolor(ebblue) fcolor(gs10)) ///
		yline(0, lpattern(dash) lcolor(gray%50)) nokey ///
		ylabel(#10, labsize(small) format(%9.3fc)) ///
		xlabel(, labsize(small) grid angle(0)) ///
		ytitle("Estimated Coefficient") ///
		xtitle("Months After the Policy Change", height(7.5)) ///
		note("Confidence interval: 5%") name("sc", replace)
graph export "${graphs}/20241022 - treatment2_d_urgencias.png", replace

*/






























