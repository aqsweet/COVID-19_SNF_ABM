Required R Packages***
	
	-	data.table
	
	
Recommended R Packages***

	-	parallel [For running multiple realizations at once using multiple CPU nodes]
	-	foreach [Also required for running parallel executions, helpful for running through policies/better for loop]
	-	ggplot2	[For rendering plots]
	
*Additional packages may be needed for prepping data, depending on source*


Data Files***
	
	Files from FRED (A Framework for Reconstructing Epidemic Dynamics)
	-   hospitals.txt (only availabe for some counties, can be constructed from other data if needed, for example see Alleghaney, PA FRED data)
	-   households.txt (FRED)
	-   people.txt (FRED)
	-   schools.txt (FRED)
	-   workplaces.txt (FRED)
	-   gq_people.txt (FRED)
	-   gq.txt (FRED)
	[https://fred.publichealth.pitt.edu/syn_pops]

	Files compiled from other data sources
	-   snf.txt (created from LTCFocus data + CMS Staffing Hours)
		[https://ltcfocus.org/data]	
		[https://data.cms.gov/quality-of-care/payroll-based-journal-daily-nurse-staffing]
	-	hospitals.txt (Missouri Department of Health data) [as needed]
		[https://health.mo.gov/safety/healthservregs/pdf/MOhospbyCounty.pdf]
		
	
	All data files used for this model are found in the Inputs folder and seperated by county.
	The counties that are investigated are:
		*Franklin, MO [29071]
		*Jefferson, MO [29099]
		*St. Charles, MO [29183]
		*St. Louis, MO [29189]
		*St. Louis City, MO [29510]
		
	For zip-code level Social Determinant of Health
	-	Shapefiles for zipcodes [https://www2.census.gov/geo/tiger/GENZ2014/shp/]
	-	ZipCode level SDoH data [https://www.ahrq.gov/sdoh/data-analytics/sdoh-data.html]
	
	
Prepping the Data***

	-	**Current version of model loads data already pre-prepared
	
	-	Create SNF Populations.R 
		-	Creates SNF population file based on the snf.txt file information
	-	Redefine Populations.R
		-	Preps the population data for use in the model
		-	Cleans data tables of unnecessary variables
	-	Hospital Preference Table.R
		-	Creates preference tables for hospitals based on distance from households
		-	Seperate tables for SNF and community populations
		-	Can take awhile to run, save to load in future
	
Required Functions***

	-	Found in Functions.R or can be loaded from Functions.Rdata

	-	SNF_worker_assignment_func
		-	Assigns workers to SNF based on total number of hours needed
		-	Number of facilities workers work at is determined based on pto and pay 
		-	Returns population tables
		
	-	rtruncnorm
		-	rnorm function truncated to a high and low
		-	used for assigning ages to prevent unreasonable ages
		
	-	deduct_pto_for_sym_workers
		-	function tht deducts pto when workers take pto for sick leave
		
	-	Modifer Functions [for mortality and hospitalization]
		-	get_age_mod
			-	modifier for age [mortality]
		-	get_SDoH_mod
			-	modifier for SDoH [mortality]
		-	get_state_mod
			-	modifer based on SIRV and PAH state [mortality]
		-	get_state_mod_hosp
			-	modifer based on SIRV state [hospitalization]
		-	get_age_mod_hosp
			-	modifier for age [hospitalization]
		-	get_SDoH_mod_hosp
			-	modifier for SDoH [hospitalization]
			
	-	calculate_hospital_infection_probability
		-	uses information on number of staff and patients along with number of infections to determine the probability of infection among patients in each of the hospitals
		-	see ODD (Overview, Design concepts and Details) for details on the equation used for calculation
		
	-	calculate_snf_infection_probability
		-	uses information on number of staff and residents along with number of infections to determine the probability of infection among residents in each of the SNF
		-	see ODD (Overview, Design concepts and Details) for details on the equation used for calculation
		
	-	assign_hospital
		-	for each individual who needs to move to a hospital at this timestep, their hospital preference table is referenced and it attempts to assign them to their preferred hospitals in order until one is assigned or no hospital beds are available.
		
	-	assign_to_snf
		-	for each individual who needs to move to a SNF at this timestep a fit score is calculated and individuals are assigned to their preferred SNF in order until all are assigned or no beds remain.
		
	-	calculate_fit_score
		-	fit scores are calculated based on race, age, and sex of the individual along with the demographic information on each of the SNFs to keep the demographics as similar to reported values as possible. 
		
	-	update_daily_stats
		-	This function pulls the daily statistics for SNF. These include number of staff, number of residents, number of infected residents, number of symptomatic residents, number of vaccinated residents, number of infected staff, number of symptomatic staff, and number of vaccinated staff by location.
		
	-	update_weekly_stats
		-	This function uses the information from the previous week found with update_daily_stats and finds the mean number for each of the values previous mentioned.

	
	
ABM_SNFdynamics_func ***	

-	For details on the main function please see the ODD (Overview, Design concepts and Details)


