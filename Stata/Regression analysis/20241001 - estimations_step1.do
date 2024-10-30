

/*
This dofile outlines the approach for estimating Difference-in-Differences (DiD) 
on the main outcomes using RIPS data. 
The primary goal is to identify the appropriate fixed effects, clusters, 
and treatment variables needed for the estimations.

The following sections specify the model structure, including the use of individual
 and time fixed effects, and the proper clustering of standard errors.

 I'm using n_visitas_rips as the outcome example, but the idea is estimate the
 results over general variables associated to consultations, procedures, 
 hospitalizations, and emergencies.
 
*/
/*
Definition of treatments and control
	
T1: Sisbén score <=30.56
    Reduction of incentive from 50% to 25% (or price change from 0.5 to 0.75 of full tariff)
    Eligible at a lower incentive

T2: Sisbén score 30.56-40
    Lost eligibility (or price change from 0.5 to 1 of full tariff  

C:  Sisbén score >40
    Not eligible in the time frame  	

*/		

global root "D:/Steban Pineda/Documents/DIME/Transportation and health"
global data        "${root}/data"
global results     "${root}/Results_run_banrep"

use "${data}/panel_sample1045_t1019.dta", replace
	gen month = mofd(monthly_date)
	format month %tm

* Neccesary variables
	gen post_law = (month>=687) // Law came into force on Apr-2017	
	gen treatment_1 = (puntaje<=30.56) 
	gen treatment_2 = (puntaje>30.56 & puntaje<=40)

* Redefining the controls for each treatment: Not eligible in the time frame
	replace treatment_1 = . if treatment_1==0 & puntaje<=40
	replace treatment_2 = . if treatment_2==0 & puntaje<=40
	
* Dummys per each month after the policy
forvalues i = 1/32 {
dis in red "Post law in t+`i'"	

	cap drop       post_law_t`i'
	gen            post_law_t`i' = (month==687+`i') // 687 corresponds to Apr-2017
	label variable post_law_t`i' "Post law dummy month in t+`i'"
		
}   
	
**** First specification: estimation to one period -----------------------------	
	
* This models should be estimated for treatment_1 and treatment_2,  and all outcomes of interest
	
* 1. Basic model: No fixed effects, no clustering
reghdfe n_visitas_rips i.treatment_1##i.post_law

* 2. Model with fixed effects for time, clustering by individual
reghdfe n_visitas_rips i.treatment_1##i.post_law, absorb(monthly_date) vce(cluster personabasicaid)

* 3. Full model: Fixed effects for individual and time, clustering by individual
reghdfe n_visitas_rips i.treatment_1##i.post_law, absorb(personabasicaid monthly_date) vce(cluster personabasicaid)
 
   
**** Second specification: estimation over time --------------------------------   
     
* 1. Basic model: No fixed effects, no clustering
reghdfe n_visitas_rips i.treatment_1##i.post_law*

* 2. Model with fixed effects for time, clustering by individual
reghdfe n_visitas_rips i.treatment_1##i.post_law*, absorb(monthly_date) vce(cluster personabasicaid)

* 3. Full model: Fixed effects for individual and time, clustering by individual
reghdfe n_visitas_rips i.treatment_1##i.post_law*, absorb(personabasicaid monthly_date) vce(cluster personabasicaid)  
  

**** Third specification: estimation over time --------------------------------- 
reghdfe n_visitas_rips pre pos /// 
        pre_law_5 pre_law_4 pre_law_3 pre_law_2 zero t0 ///
		post_law_t1 post_law_t2 post_law_t3 post_law_t4 post_law_t5, ///
		absorb(personabasicaid monthly_date) vce(cluster personabasicaid) 
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   