/*
DIFFERENCES IN DIFFERENCES FOR CONSULTATIONS
	Creation date: 21-Oct-2024
	Author: Brayan Pineda
	Last modification: 21-Oct-2024 by Brayan Pineda
	Objective: Preliminar | First estimations set, only for consultations.

In this Dofile you'll find this 3 categories for the estimations:
	Specification #1: Static model 
	Specification #2: Model over the time, using interactions from t+1 until t+6 	
	Specification #3: Results in Graphs, using only the full model
	
Each specification has 3 models:
	1. Basic model, without fixed effects and without cluster
	2. Model with calendar FE and cluster at person level
	3. Full model: using calendar and person FE, and cluster at person level
	
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

******************* PREPARING THE SAMPLE 2015m1 - 2017m9 ***********************

use "${data}/panel_sample1045_t1019.dta", replace
	gen month = mofd(monthly_date)
	format month %tm
	keep if inrange(month, 660, 692) // filtering the sample from 2015m1 to 6 periods after law change
	
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
********************* ESTIMATIONS TREATMENT 1 **********************************
********************************************************************************


global outcomes_gnral "n_consultas d_consultas"	

	
**# Specification #1
****---------------------- First specification ----------------------------*****
* 1. Basic model: No fixed effects, no clustering
putexcel set "${tables}/20241020-results_v1_cons_t1.xlsx", sheet(spec1_model1, replace) modify
putexcel A1=("Outcome") B1=("Mean") C1=("SD.") D1=("Coeff.") E1=("S.E.") F1=("p-value") G1=("N") H1=("R2")
local s = 1			
foreach y of global outcomes_gnral {

	local s = `s'+1	
	dis in red "`s' --- `y'"
	reghdfe `y' treatment_1 post_law interaction
			putexcel A`s'=("`y'") 
	
			sum `y' if treatment_1==1 & post_law==0 // mean of the treatment before the policy change
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

* 2. Model with fixed effects for time, clustering by individual
putexcel set "${tables}/20241020-results_v1_cons_t1.xlsx", sheet(spec1_model2, replace) modify
putexcel A1=("Outcome") B1=("Mean") C1=("SD.") D1=("Coeff.") E1=("S.E.") F1=("p-value") G1=("N") H1=("R2")			
local s = 1			
foreach y of global outcomes_gnral {

	local s = `s'+1	
	dis in red "`s' --- `y'"			
	
	reghdfe `y' treatment_1 post_law interaction, absorb(fe_month) vce(cluster personabasicaid)	
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
	
* 3. Full model: Fixed effects for individual and time, clustering by individual
putexcel set "${tables}/20241020-results_v1_cons_t1.xlsx", sheet(spec1_model3, replace) modify
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




**# Specification #2
****--------------------- Second specification ----------------------------*****

* 1. Basic model: No fixed effects, no clustering
putexcel set "${tables}/20241020-results_v1_cons_t1.xlsx", sheet(spec2_model1, replace) modify
putexcel A1=("Outcome") B1=("Mean") C1=("SD.") D1=("Coeff.") E1=("S.E.") F1=("p-value") G1=("N") H1=("R2")
local s = 0			
foreach y of global outcomes_gnral {

    local s = `s'+1
	local h = `s'+(6*(`s'-1))	
	dis in red "`s' --- `y'"
	reghdfe `y' treatment_1 post_law_t* interaction_t*
	forvalues i=1/6 { //	
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

* 2. Model with fixed effects for time, clustering by individual
putexcel set "${tables}/20241020-results_v1_cons_t1.xlsx", sheet(spec2_model2, replace) modify
putexcel A1=("Outcome") B1=("Mean") C1=("SD.") D1=("Coeff.") E1=("S.E.") F1=("p-value") G1=("N") H1=("R2")			
local s = 0			
foreach y of global outcomes_gnral {

    local s = `s'+1
	local h = `s'+(6*(`s'-1))	
	dis in red "`s' --- `y'"	
	
	reghdfe `y' treatment_1 post_law_t* interaction_t*, absorb(fe_month) vce(cluster personabasicaid)	
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
	
* 3. Full model: Fixed effects for individual and time, clustering by individual
putexcel set "${tables}/20241020-results_v1_cons_t1.xlsx", sheet(spec2_model3, replace) modify
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


**# Specification #3
****--------------------- Plot the estimations ----------------------------*****

global cond_f "absorb(personabasicaid fe_month) vce(cluster personabasicaid)"


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
		subtitle("{bf:Elegible at a lower discount (<=30.56) vs. Not eligible (40-45)}", size(small)) ///
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
graph export "${graphs}/20241020-n_cons_v1_t1.png", replace

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
		subtitle("{bf:Elegible at a lower discount (<=30.56) vs. Not eligible (40-45)}", size(small)) ///
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
graph export "${graphs}/20241020-d_cons_v1_t1.png", replace



********************************************************************************
********************* ESTIMATIONS TREATMENT 2 **********************************
********************************************************************************


global outcomes_gnral "n_consultas d_consultas"	

	
 **# Specification #1
****---------------------- First specification ----------------------------*****
* 1. Basic model: No fixed effects, no clustering
putexcel set "${tables}/20241020-results_v1_cons_t2.xlsx", sheet(spec1_model1, replace) modify
putexcel A1=("Outcome") B1=("Mean") C1=("SD.") D1=("Coeff.") E1=("S.E.") F1=("p-value") G1=("N") H1=("R2")
local s = 1			
foreach y of global outcomes_gnral {

	local s = `s'+1	
	dis in red "`s' --- `y'"
	reghdfe `y' treatment_2 post_law interaction2
			putexcel A`s'=("`y'") 
	
			sum `y' if treatment_2==1 & post_law==0 // mean of the treatment before the policy change
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

* 2. Model with fixed effects for time, clustering by individual
putexcel set "${tables}/20241020-results_v1_cons_t2.xlsx", sheet(spec1_model2, replace) modify
putexcel A1=("Outcome") B1=("Mean") C1=("SD.") D1=("Coeff.") E1=("S.E.") F1=("p-value") G1=("N") H1=("R2")			
local s = 1			
foreach y of global outcomes_gnral {

	local s = `s'+1	
	dis in red "`s' --- `y'"			
	
	reghdfe `y' treatment_2 post_law interaction2, absorb(fe_month) vce(cluster personabasicaid)	
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
	
* 3. Full model: Fixed effects for individual and time, clustering by individual
putexcel set "${tables}/20241020-results_v1_cons_t2.xlsx", sheet(spec1_model3, replace) modify
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




**# Specification #2
****--------------------- Second specification ----------------------------*****

* 1. Basic model: No fixed effects, no clustering
putexcel set "${tables}/20241020-results_v1_cons_t2.xlsx", sheet(spec2_model1, replace) modify
putexcel A1=("Outcome") B1=("Mean") C1=("SD.") D1=("Coeff.") E1=("S.E.") F1=("p-value") G1=("N") H1=("R2")
local s = 0			
foreach y of global outcomes_gnral {

    local s = `s'+1
	local h = `s'+(6*(`s'-1))	
	dis in red "`s' --- `y'"
	reghdfe `y' treatment_2 post_law_t* interaction2_t*
	forvalues i=1/6 { //	
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

* 2. Model with fixed effects for time, clustering by individual
putexcel set "${tables}/20241020-results_v1_cons_t2.xlsx", sheet(spec2_model2, replace) modify
putexcel A1=("Outcome") B1=("Mean") C1=("SD.") D1=("Coeff.") E1=("S.E.") F1=("p-value") G1=("N") H1=("R2")			
local s = 0			
foreach y of global outcomes_gnral {

    local s = `s'+1
	local h = `s'+(6*(`s'-1))	
	dis in red "`s' --- `y'"	
	
	reghdfe `y' treatment_2 post_law_t* interaction2_t*, absorb(fe_month) vce(cluster personabasicaid)	
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
	
* 3. Full model: Fixed effects for individual and time, clustering by individual
putexcel set "${tables}/20241020-results_v1_cons_t2.xlsx", sheet(spec2_model3, replace) modify
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


**# Specification #3
****--------------------- Plot the estimations ----------------------------*****

global cond_f "absorb(personabasicaid fe_month) vce(cluster personabasicaid)"


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
		subtitle("{bf:Elegible at a lower discount (<=30.56) vs. Not eligible (40-45)}", size(small)) ///
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
graph export "${graphs}/20241020-n_cons_v1_t2.png", replace

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
		subtitle("{bf:Elegible at a lower discount (<=30.56) vs. Not eligible (40-45)}", size(small)) ///
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
graph export "${graphs}/20241020-d_cons_v1_t2.png", replace




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
********************* ESTIMATIONS TREATMENT 1 **********************************
********************************************************************************


global outcomes_gnral "n_consultas d_consultas"	

	
**# Specification #1
****---------------------- First specification ----------------------------*****
* 1. Basic model: No fixed effects, no clustering
putexcel set "${tables}/20241020-results_v2_cons_t1.xlsx", sheet(spec1_model1, replace) modify
putexcel A1=("Outcome") B1=("Mean") C1=("SD.") D1=("Coeff.") E1=("S.E.") F1=("p-value") G1=("N") H1=("R2")
local s = 1			
foreach y of global outcomes_gnral {

	local s = `s'+1	
	dis in red "`s' --- `y'"
	reghdfe `y' treatment_1 post_law interaction
			putexcel A`s'=("`y'") 
	
			sum `y' if treatment_1==1 & post_law==0 // mean of the treatment before the policy change
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

* 2. Model with fixed effects for time, clustering by individual
putexcel set "${tables}/20241020-results_v2_cons_t1.xlsx", sheet(spec1_model2, replace) modify
putexcel A1=("Outcome") B1=("Mean") C1=("SD.") D1=("Coeff.") E1=("S.E.") F1=("p-value") G1=("N") H1=("R2")			
local s = 1			
foreach y of global outcomes_gnral {

	local s = `s'+1	
	dis in red "`s' --- `y'"			
	
	reghdfe `y' treatment_1 post_law interaction, absorb(fe_month) vce(cluster personabasicaid)	
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
	
* 3. Full model: Fixed effects for individual and time, clustering by individual
putexcel set "${tables}/20241020-results_v2_cons_t1.xlsx", sheet(spec1_model3, replace) modify
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




**# Specification #2
****--------------------- Second specification ----------------------------*****

* 1. Basic model: No fixed effects, no clustering
putexcel set "${tables}/20241020-results_v2_cons_t1.xlsx", sheet(spec2_model1, replace) modify
putexcel A1=("Outcome") B1=("Mean") C1=("SD.") D1=("Coeff.") E1=("S.E.") F1=("p-value") G1=("N") H1=("R2")
local s = 0			
foreach y of global outcomes_gnral {

    local s = `s'+1
	local h = `s'+(6*(`s'-1))	
	dis in red "`s' --- `y'"
	reghdfe `y' treatment_1 post_law_t* interaction_t*
	forvalues i=1/6 { //	
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

* 2. Model with fixed effects for time, clustering by individual
putexcel set "${tables}/20241020-results_v2_cons_t1.xlsx", sheet(spec2_model2, replace) modify
putexcel A1=("Outcome") B1=("Mean") C1=("SD.") D1=("Coeff.") E1=("S.E.") F1=("p-value") G1=("N") H1=("R2")			
local s = 0			
foreach y of global outcomes_gnral {

    local s = `s'+1
	local h = `s'+(6*(`s'-1))	
	dis in red "`s' --- `y'"	
	
	reghdfe `y' treatment_1 post_law_t* interaction_t*, absorb(fe_month) vce(cluster personabasicaid)	
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
	
* 3. Full model: Fixed effects for individual and time, clustering by individual
putexcel set "${tables}/20241020-results_v2_cons_t1.xlsx", sheet(spec2_model3, replace) modify
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


**# Specification #3
****--------------------- Plot the estimations ----------------------------*****

global cond_f "absorb(personabasicaid fe_month) vce(cluster personabasicaid)"


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
		subtitle("{bf:Elegible at a lower discount (<=30.56) vs. Not eligible (40-45)}", size(small)) ///
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
graph export "${graphs}/20241020-n_cons_v2_t1.png", replace

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
		subtitle("{bf:Elegible at a lower discount (<=30.56) vs. Not eligible (40-45)}", size(small)) ///
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
graph export "${graphs}/20241020-d_cons_v2_t1.png", replace



********************************************************************************
********************* ESTIMATIONS TREATMENT 2 **********************************
********************************************************************************


global outcomes_gnral "n_consultas d_consultas"	

	
 **# Specification #1
****---------------------- First specification ----------------------------*****
* 1. Basic model: No fixed effects, no clustering
putexcel set "${tables}/20241020-results_v2_cons_t2.xlsx", sheet(spec1_model1, replace) modify
putexcel A1=("Outcome") B1=("Mean") C1=("SD.") D1=("Coeff.") E1=("S.E.") F1=("p-value") G1=("N") H1=("R2")
local s = 1			
foreach y of global outcomes_gnral {

	local s = `s'+1	
	dis in red "`s' --- `y'"
	reghdfe `y' treatment_2 post_law interaction2
			putexcel A`s'=("`y'") 
	
			sum `y' if treatment_2==1 & post_law==0 // mean of the treatment before the policy change
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

* 2. Model with fixed effects for time, clustering by individual
putexcel set "${tables}/20241020-results_v2_cons_t2.xlsx", sheet(spec1_model2, replace) modify
putexcel A1=("Outcome") B1=("Mean") C1=("SD.") D1=("Coeff.") E1=("S.E.") F1=("p-value") G1=("N") H1=("R2")			
local s = 1			
foreach y of global outcomes_gnral {

	local s = `s'+1	
	dis in red "`s' --- `y'"			
	
	reghdfe `y' treatment_2 post_law interaction2, absorb(fe_month) vce(cluster personabasicaid)	
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
	
* 3. Full model: Fixed effects for individual and time, clustering by individual
putexcel set "${tables}/20241020-results_v2_cons_t2.xlsx", sheet(spec1_model3, replace) modify
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




**# Specification #2
****--------------------- Second specification ----------------------------*****

* 1. Basic model: No fixed effects, no clustering
putexcel set "${tables}/20241020-results_v2_cons_t2.xlsx", sheet(spec2_model1, replace) modify
putexcel A1=("Outcome") B1=("Mean") C1=("SD.") D1=("Coeff.") E1=("S.E.") F1=("p-value") G1=("N") H1=("R2")
local s = 0			
foreach y of global outcomes_gnral {

    local s = `s'+1
	local h = `s'+(6*(`s'-1))	
	dis in red "`s' --- `y'"
	reghdfe `y' treatment_2 post_law_t* interaction2_t*
	forvalues i=1/6 { //	
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

* 2. Model with fixed effects for time, clustering by individual
putexcel set "${tables}/20241020-results_v2_cons_t2.xlsx", sheet(spec2_model2, replace) modify
putexcel A1=("Outcome") B1=("Mean") C1=("SD.") D1=("Coeff.") E1=("S.E.") F1=("p-value") G1=("N") H1=("R2")			
local s = 0			
foreach y of global outcomes_gnral {

    local s = `s'+1
	local h = `s'+(6*(`s'-1))	
	dis in red "`s' --- `y'"	
	
	reghdfe `y' treatment_2 post_law_t* interaction2_t*, absorb(fe_month) vce(cluster personabasicaid)	
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
	
* 3. Full model: Fixed effects for individual and time, clustering by individual
putexcel set "${tables}/20241020-results_v2_cons_t2.xlsx", sheet(spec2_model3, replace) modify
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


**# Specification #3
****--------------------- Plot the estimations ----------------------------*****

global cond_f "absorb(personabasicaid fe_month) vce(cluster personabasicaid)"


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
		subtitle("{bf:Elegible at a lower discount (<=30.56) vs. Not eligible (40-45)}", size(small)) ///
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
graph export "${graphs}/20241020-n_cons_v2_t2.png", replace

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
		subtitle("{bf:Elegible at a lower discount (<=30.56) vs. Not eligible (40-45)}", size(small)) ///
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
graph export "${graphs}/20241020-d_cons_v2_t2.png", replace


























