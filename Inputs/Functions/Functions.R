# Define functions for later use #====

## Assign SNF Worker Function ----

SNF_worker_assignment_func <- function(x = community_pop, y = SNF_data, min_age_worker = minimum_age_worker, max_jobs = max_concurrent_facilities_SNF, concurrent_jobs_prop = n_facilities_proportion_SNF_workers, job_hours = n_hours_per_facility_SNF){
  
  
  y <- y[beds > 0, ] #remove any facilities with no beds
  
  chosen_workers <- NULL
  eligible_workers <- x[age >= min_age_worker & altered_work_id == 0, sp_id] #pulls the ids of community members that are eligible to work
  y$worker_hours_needed <- y$beds*y$worker_hours
  
  while(nrow(y) > 0){#this loop will continue until all SNF have the minimum number of workers to cover the minimum hours
    
    total_hours_needed <- sum(y[,worker_hours_needed])#sums the total number of hours needed for workers across all facilities
    main_facility_id <- NA
    scd_facility_id <- NA
    ter_facility_id <- NA
    facility_PTO <- NA
    worker_id <- NA
    multiple_facility_chances <- NA
    
    worker_id <- sample(eligible_workers, 1) #choose a random worker id
    num_fac <- sample(1:nrow(y),1) #pulls a random facility
    main_facility_id <- y[num_fac,snf_id] #pulls the id of the facility
    facility_PTO <- y[num_fac,PTO_rate] #pulls the PTO rate of the facility
    if(identical(facility_PTO, numeric(0)) == FALSE){
      if(facility_PTO == 2){ #sets multiple facility chance based on the facility_total score [how good the combined PTO and payscale are]
        multiple_facility_chances <- 0
      }else if(facility_PTO == 1){
        multiple_facility_chances <- 0.5
      }else{
        multiple_facility_chances <- 0.9
      }
    }
    
    if(runif(1) < multiple_facility_chances){#if the employee will work at multiple facilities
      
      if(nrow(y) > 1){
        if(length(x[sp_id == worker_id, work_id_final_2]) > 1){next}
        if(runif(1) < 0.8 | nrow(y) == 2){
          #2 facilities 
          
          #pull 2nd facility
          scd_facility_id <- sample(y[snf_id !=main_facility_id,snf_id],1)
          x[sp_id == worker_id, `:=` (work_id_final_1 = main_facility_id, work_id_final_2 = scd_facility_id, altered_work_id = 1, work_id_updated_position = "SNF_worker", n_facilities = 2)]
          y[snf_id == main_facility_id, worker_hours_needed := worker_hours_needed - job_hours[[2]]]
          y[snf_id == scd_facility_id, worker_hours_needed := worker_hours_needed - job_hours[[2]]]
          
        }else{
          #3 facilities 
          
          #pull 2nd facility
          scd_facility_id <- sample(y$snf_id[which(y$snf_id != main_facility_id)],1)
          #pull 3rd facility
          ter_facility_id <- sample(y$snf_id[which(y$snf_id != main_facility_id & y$snf_id != scd_facility_id)],1)
          x[sp_id == worker_id, `:=` (work_id_final_1 = main_facility_id, work_id_final_2 = scd_facility_id, work_id_final_3 = ter_facility_id, altered_work_id = 1, work_id_updated_position = "SNF_worker", n_facilities = 3)]
          y[snf_id == main_facility_id, worker_hours_needed := worker_hours_needed - job_hours[[3]]]
          y[snf_id == scd_facility_id, worker_hours_needed := worker_hours_needed - job_hours[[3]]]
          y[snf_id == ter_facility_id, worker_hours_needed := worker_hours_needed - job_hours[[3]]]
        }
      }   
    } else {
      #1 facility
      
      x[sp_id == worker_id, `:=` (work_id_final_1 = main_facility_id, altered_work_id = 1, work_id_updated_position = "SNF_worker", n_facilities = 1)]
      y[snf_id == main_facility_id, worker_hours_needed := worker_hours_needed - job_hours[[1]]]
      
    } 
    chosen_workers <- rbind(chosen_workers,x[sp_id == worker_id, ])
    y <- y[worker_hours_needed > 0, ]
    eligible_workers <- eligible_workers[!eligible_workers %in% worker_id]
  }
  return( #return the updated population.
    x
  )
  
}


## rtruncnorm function ----

rtruncnorm <- function(n, mu, sigma, low, high) {
  # find quantiles that correspond the the given low and high levels.
  p_low <- pnorm(low, mu, sigma)
  p_high <- pnorm(high, mu, sigma)
  
  # draw quantiles uniformly between the limits and pass these
  # to the relevant quantile function.
  qnorm(runif(n, p_low, p_high), mu, sigma)
}
## resample function ----
resample <- function(x, ...) {x[sample.int(length(x), ...)]}

## PTO function ----
#Function for PTO 

deduct_pto_for_sym_workers <- function(data, symptomatic_ids, pto_column) {
  
  if(pto_column == "PTO_1"){
    # Check and filter only symptomatic workers with available PTO
    symptomatic_with_pto <- data[symptomatic_ids][altered_work_id == 1 & PTO_1 > 0, which = TRUE]
    
    # Deduct PTO in a vectorized manner
    data[symptomatic_with_pto, PTO_1 := PTO_1 - 1]
  }else{
    if(pto_column == "PTO_2"){
      # Check and filter only symptomatic workers with available PTO
      symptomatic_with_pto <- data[symptomatic_ids][altered_work_id == 1 & PTO_2 > 0, which = TRUE]
      
      # Deduct PTO in a vectorized manner
      data[symptomatic_with_pto, PTO_2 := PTO_2 - 1]
    }else{
      if(pto_column == "PTO_3"){
        # Check and filter only symptomatic workers with available PTO
        symptomatic_with_pto <- data[symptomatic_ids][altered_work_id == 1 & PTO_3 > 0, which = TRUE]
        
        # Deduct PTO in a vectorized manner
        data[symptomatic_with_pto, PTO_3 := PTO_3 - 1]
        
      }
    }}
  
  
  return(data)
}

# Define Modifier Functions ====
## Mortality Modifiers ====
get_age_mod <- function(age, mortality_AFT_hazard_betas) { unname(mortality_AFT_hazard_betas["beta_age"]) * age } #modifications to mortality based on age
get_SDoH_mod <- function(nSDoH, mortality_AFT_hazard_betas) {
  num_SDoH <- nSDoH
  SDoH_mod <- rep(0,length(num_SDoH))
  for(i in 1:length(num_SDoH)){
  if(num_SDoH[i] == 1){
    SDoH_mod[i] <- unname(mortality_AFT_hazard_betas["beta_SDoH_1"])
  }
  if(num_SDoH[i] == 2){
    SDoH_mod[i] <- unname(mortality_AFT_hazard_betas["beta_SDoH_2"])
  }
  if(num_SDoH[i] == 3){
    SDoH_mod[i] <- unname(mortality_AFT_hazard_betas["beta_SDoH_3"])
  }
  if(num_SDoH[i] == 4){
    SDoH_mod[i] <- unname(mortality_AFT_hazard_betas["beta_SDoH_4"])
  }
  if(num_SDoH[i] == 5){
    SDoH_mod[i] <- unname(mortality_AFT_hazard_betas["beta_SDoH_5"])
  }
  if(num_SDoH[i] > 5){
    SDoH_mod[i] <- unname(mortality_AFT_hazard_betas["beta_SDoH_6plus"])
  }}
  return(SDoH_mod)
} #modifications to mortality based on social determinants of health
get_state_mod <- function(state_SIRV, state_PAH, mortality_AFT_hazard_betas) {
  I_mod <- unname(mortality_AFT_hazard_betas["beta_I"]) * (state_SIRV == "I")
  H_mod <- unname(mortality_AFT_hazard_betas["beta_H"]) * grepl("H", state_PAH)
  A_mod <- unname(mortality_AFT_hazard_betas["beta_A"]) * (state_PAH == "A")
  return(I_mod + H_mod + A_mod)
} #modifications to mortality based on SIRV and PAH state

## Hospitalization Modifiers ====  

get_state_mod_hosp <- function(state_SIRV, hospitalization_AFT_hazard_betas) {
  I_mod <- unname(hospitalization_AFT_hazard_betas["beta_I"]) * (state_SIRV == "I")
  return(I_mod)
} #modifications to mortality based on SIRV and PAH state
get_age_mod_hosp <- function(age, hospitalization_AFT_hazard_betas) { unname(hospitalization_AFT_hazard_betas["beta_age"]) * age} #modifications to mortality based on SDoH
get_SDoH_mod_hosp <- function(nSDoH, hospitalization_AFT_hazard_betas) {
  num_SDoH <- nSDoH
  SDoH_mod <- rep(0,length(num_SDoH))
  for(i in 1:length(num_SDoH)){
  if(num_SDoH[i] == 1){
    SDoH_mod[i] <- unname(hospitalization_AFT_hazard_betas["beta_SDoH_1"])
  }
  if(num_SDoH[i] == 2){
    SDoH_mod[i] <- unname(hospitalization_AFT_hazard_betas["beta_SDoH_2"])
  }
  if(num_SDoH[i] == 3){
    SDoH_mod[i] <- unname(hospitalization_AFT_hazard_betas["beta_SDoH_3"])
  }
  if(num_SDoH[i] == 4){
    SDoH_mod[i] <- unname(hospitalization_AFT_hazard_betas["beta_SDoH_4"])
  }
  if(num_SDoH[i] > 4){
    SDoH_mod[i] <- unname(hospitalization_AFT_hazard_betas["beta_SDoH_5plus"])
  }
} 
  return(SDoH_mod)} #modifications to hospitalization based on social determinants of health


#Hospital Infection Rate Function ----

calculate_hospital_infection_probability <- function(hospital_data, alive_table, infectious_hosts, patient_contacts, hosp_worker_contacts, community_infection_prevalence, mandatory_sickDays, transmission_prob, other_contacts, ratio_other_contacts) {
  # Calculate infection probability for each hospital
  hospital_data[, ':=' (
    num_workers = alive_table[work_id_final_1 == hosp_id, .N],
    num_I_workers = infectious_hosts[work_id_final_1 == hosp_id, .N],
    sym_workers = infectious_hosts[work_id_final_1 == hosp_id & symptoms == "Y", .N]
  ), by = hosp_id]
  
  
  if(mandatory_sickDays == TRUE){
    #modify by PTO
    symptomatic_worker_ids <- infectious_hosts[symptoms == "Y", ]
    
    #deduct for each hospital
    for(h in unique(hospital_data$hosp_id)){
      symptomatic_in_hosp <- symptomatic_worker_ids[work_id_final_1 == h, which = TRUE]
      if(length(symptomatic_in_hosp) > 0){
        workers_to_subtract <- symptomatic_worker_ids[work_id_final_1 == h & PTO_1 > 1, .N]
        alive_table <- deduct_pto_for_sym_workers(alive_table, symptomatic_in_hosp, "PTO_1")
        hospital_data[hosp_id == h, num_I_workers := num_I_workers - workers_to_subtract]
      }}
  }
  hospital_data[, ':=' (
    num_patients = alive_table[current_location_id == hosp_id, .N],
    num_I_patients = infectious_hosts[current_location_id == hosp_id, .N],
    p_pat = num_I_patients / num_patients,
    p_work = num_I_workers / num_workers,
    p_other = community_infection_prevalence,
    total_inf_chance = 1 - ((1 - transmission_prob * p_pat)^patient_contacts * (1 - transmission_prob * p_work)^hosp_worker_contacts * (1 - transmission_prob * p_other)^other_contacts))
    , by = hosp_id]
  
  # Update the infection probability in the alive_table
  alive_table[, infection_probability := hospital_data[.SD, total_inf_chance, on = .(hosp_id = current_location_id)]]
  
  return(list(hospital_data, alive_table))
}



#SNF Infection Rate Function----
# Function to calculate infection probability for each hospital
calculate_snf_infection_probability <- function(SNF_data, alive_table, infectious_hosts, resident_contacts, worker_contacts, community_infection_prevalence, mandatory_sickDays, transmission_prob, other_contacts, ratio_other_contacts) {
  # Calculate infection probability for each hospital
  SNF_data[, ':=' (
    num_workers = alive_table[work_id_final_1 == snf_id | work_id_final_2 == snf_id | work_id_final_3 == snf_id, .N], #15
    num_I_workers = infectious_hosts[work_id_final_1 == snf_id | work_id_final_2 == snf_id | work_id_final_3 == snf_id, .N], #2
    sym_workers = infectious_hosts[(work_id_final_1 == snf_id | work_id_final_2 == snf_id | work_id_final_3 == snf_id) & symptoms == "Y", .N] #1
  ), by = snf_id]
  
  
  if(mandatory_sickDays == TRUE){
    #modify by PTO
    symptomatic_worker_ids <- infectious_hosts[symptoms == "Y", ]
    
    #deduct for each hospital
    for(s in unique(SNF_data$snf_id)){
      symptomatic_in_snf <- symptomatic_worker_ids[work_id_final_1 == s | work_id_final_2 == s | work_id_final_3 == s, which = TRUE]
      if(length(symptomatic_in_snf) > 0){
        workers_to_subtract <- symptomatic_worker_ids[(work_id_final_1 == s & PTO_1 > 1) | (work_id_final_2 == s & PTO_2 > 1) | (work_id_final_3 == s & PTO_3 > 1), .N]
        alive_table <- deduct_pto_for_sym_workers(alive_table, symptomatic_in_snf, "PTO_1")
        alive_table <- deduct_pto_for_sym_workers(alive_table, symptomatic_in_snf, "PTO_2")
        alive_table <- deduct_pto_for_sym_workers(alive_table, symptomatic_in_snf, "PTO_3")
        SNF_data[snf_id == s, num_I_workers := num_I_workers - workers_to_subtract]
      }}
  }
  SNF_data[, ':=' (
    persons = alive_table[current_location_id == snf_id, .N],
    num_I_residents = infectious_hosts[current_location_id == snf_id, .N],
    p_res = num_I_residents / persons,
    p_work = num_I_workers / num_workers,
    p_other = community_infection_prevalence,
    total_inf_chance = 1 - ((1 - transmission_prob * p_res)^resident_contacts * (1 - transmission_prob * p_work)^worker_contacts * (1 - transmission_prob * p_other)^other_contacts))
      , by = snf_id]
  
  # Update the infection probability in the alive_table
  alive_table[, infection_probability := SNF_data[.SD, total_inf_chance, on = .(snf_id = current_location_id)]]
  
  return(list(SNF_data, alive_table))
}

#Assign Hospitals Function----
# Define a function to assign a hospital based on preferences and availability

assign_hospital <- function(preferences, hospital_data, preference_columns){
if(length(preference_columns) > 1){
  # Step 1: Calculate the minimum (smallest) value for each column
  min_values <- sapply(preferences, min)
  
  # Step 2: Order the columns by these minimum values, handle ties by random order
  ordered_columns <- sample(names(min_values)[order(min_values)])
  
  # Step 3: Retrieve ordered column names
  ordered_column_names <- names(min_values)[order(min_values)]
  for(i in ordered_column_names) {
    choice <- sub("hosp_id_", "", i)
    if(hospital_data[hosp_id == choice,beds_available > 0]){
      #If the preferred hospital has beds available
      hospital_data[hosp_id == choice, beds_available := beds_available - 1]
      return(list(hosp_id = choice, hospital_data = hospital_data))
    }
  }
}
  if(length(preference_columns) == 1){
    choice <- sub("hosp_id_", "", preference_columns)
    if(hospital_data[hosp_id == choice, beds_available > 0]){
      #If the preferred hospital has beds available
      hospital_data[hosp_id == choice, beds_available := beds_available - 1]
      return(list(hosp_id = choice, hospital_data = hospital_data))
    }
  }
  return(list(hosp_id = NA, hospital_data = hospital_data))
    
  }

#Assign SNF Function---- 


assign_to_snf <- function(alive_table, SNF_data, needs_to_move) {
  SNFs_with_available_beds <- SNF_data[beds_available > 0, ]
  
  if (nrow(SNFs_with_available_beds) > 0) {
    max_move <- min(sum(SNFs_with_available_beds[,beds_available]), length(needs_to_move))
    snfs_assigned <- numeric(max_move)
    
    for (i in 1:max_move) {
      individual <- alive_table[needs_to_move[i], .(sex, race, age)]
      snfs_matrix <- SNFs_with_available_beds[, .(snf_id, race_b, race_w, race_ns, sex_f, sex_m, avgage)]
      
      # Compute fit scores
      fit_scores <- sapply(seq_len(nrow(snfs_matrix)), function(j) calculate_fit_score(individual, snfs_matrix[j, ]))
      
      # Choose the SNF with the best fit score that still has available beds
      best_snf <- snfs_matrix[max.col(t(fit_scores), ties.method = "random"), snf_id]
      
      # Assign individual to SNF
      snfs_assigned[i] <- best_snf
      
      # Update available beds in SNFs_with_available_beds
      SNFs_with_available_beds[snf_id == best_snf, beds_available := beds_available - 1]
      SNFs_with_available_beds <- SNFs_with_available_beds[beds_available > 0]
      
      # Update SNF_data for each assignment
      SNF_data[snf_id == best_snf, `:=` (
        total_n_residents = total_n_residents + 1,
        beds_available = beds_available - 1
      )]
    }
    # Extract dis and los values for each sampled SNF
    # Map dis and los values to each individual
    mapped_dis <- SNF_data[snf_id %in% snfs_assigned, .(snf_id, dis, los)]
    dis_values <- mapped_dis$dis[match(snfs_assigned, mapped_dis$snf_id)]
    los_values <- mapped_dis$los[match(snfs_assigned, mapped_dis$snf_id)]
    
    # Update alive_table with the assigned SNF information
    alive_table[needs_to_move[1:max_move], `:=` (
      dwelling_id = snfs_assigned,
      population = "SNF",
      current_location_id = fifelse(grepl("H", current_location_id), current_location_id, as.character(snfs_assigned)),
      SNF_days = 0,
      SNF_LOS = fifelse(runif(max_move) < dis_values, round(rlnorm(max_move, log(los_values))), NA_integer_)
    )]
  }else{max_move = 0}
  
  return(list(alive_table = alive_table, SNF_data = SNF_data, num_moved = max_move))
}

# Calculate Fit Score ====

calculate_fit_score <- function(individual,snf) {
  
  # race_b	race_w	race_ns	sex_f	sex_m	avgage
  # 
  # race age sex
  
  age_fit <- ifelse(individual$age == snf$avgage, 1, 1/(abs(snf$avgage - individual$age)))
  
  if(individual$race == 1){
    race_fit <- snf$race_w/100
  }else{
    if(individual$race == 2){
      race_fit <- snf$race_b/100
    }else{
      race_fit <- snf$race_ns/100
    }
  }
  
  sex_fit <- ifelse(individual$sex == "F", snf$sex_f, snf$sex_m)
  
  fit_score <- sum(age_fit, race_fit, sex_fit)
  
  
}


#Calculate SNF Stats Functions----

update_daily_stats <- function(tick, daily_data, alive_table, SNF_data) {
  # Calculate stats for each SNF based on alive_table and SNF_data
  snf_stats <- SNF_data[, .(
    num_staff = nrow(alive_table[work_id_final_1 == snf_id | work_id_final_2 == snf_id | work_id_final_3 == snf_id]),
    num_residents = alive_table[current_location_id == snf_id,.N],
    num_infected_residents = nrow(alive_table[state_SIRV == "I" & current_location_id == snf_id]),
    num_symptomatic_residents = nrow(alive_table[symptoms == "Y" & current_location_id == snf_id]),
    num_vaccinated_residents = nrow(alive_table[state_SIRV == "V" & current_location_id == snf_id]),
    num_infected_staff = nrow(alive_table[state_SIRV == "I" & (work_id_final_1 == snf_id | work_id_final_2 == snf_id | work_id_final_3 == snf_id)]),
    num_symptomatic_staff = nrow(alive_table[symptoms == "Y" & (work_id_final_1 == snf_id | work_id_final_2 == snf_id | work_id_final_3 == snf_id)]),
    num_vaccinated_staff = nrow(alive_table[n_doses_vaccine >= 1 & (work_id_final_1 == snf_id | work_id_final_2 == snf_id | work_id_final_3 == snf_id)])
  ), by = .(location = snf_id)]
  
  # Add the 'day' column to snf_stats
  snf_stats[, day := tick]
  
  # Merge the new stats into daily_data
  # Ensure merge is done based on both 'day' and 'location'
  daily_data[snf_stats, on = .(location, day), `:=` (
    num_staff = i.num_staff,
    num_residents = i.num_residents,
    num_infected_residents = i.num_infected_residents,
    num_symptomatic_residents = i.num_symptomatic_residents,
    num_vaccinated_residents = i.num_vaccinated_residents,
    num_infected_staff = i.num_infected_staff,
    num_symptomatic_staff = i.num_symptomatic_staff,
    num_vaccinated_staff = i.num_vaccinated_staff
  )]
  
  return(daily_data)
}

update_weekly_stats <- function(weekly_data, week, daily_data) {
  week_start <- (week - 1) * 7 + 1
  week_end <- week * 7
  
  # Calculate weekly stats
  current_week_data <- daily_data[day %between% c(week_start, week_end), .(
    mean_num_residents = mean(num_residents, na.rm = TRUE),
    mean_num_staff = mean(num_staff, na.rm = TRUE),
    mean_num_infected_residents = mean(num_infected_residents, na.rm = TRUE),
    mean_num_symptomatic_residents = mean(num_symptomatic_residents, na.rm = TRUE),
    mean_num_infected_staff = mean(num_infected_staff, na.rm = TRUE),
    mean_num_symptomatic_staff = mean(num_symptomatic_staff, na.rm = TRUE),
    mean_num_vaccinated_residents = mean(num_vaccinated_residents, na.rm = TRUE),
    mean_num_vaccinated_staff = mean(num_vaccinated_staff, na.rm = TRUE)
  ), by = location]
  
  # Add 'week' column to current_week_data
  current_week_data[, week := week]
  
  # Merge the new stats into weekly_data
  weekly_data[current_week_data, on = .(location, week), `:=` (
    mean_num_residents = i.mean_num_residents,
    mean_num_staff = i.mean_num_staff,
    mean_num_infected_residents = i.mean_num_infected_residents,
    mean_num_symptomatic_residents = i.mean_num_symptomatic_residents,
    mean_num_infected_staff = i.mean_num_infected_staff,
    mean_num_symptomatic_staff = i.mean_num_symptomatic_staff,
    mean_num_vaccinated_residents = i.mean_num_vaccinated_residents,
    mean_num_vaccinated_staff = i.mean_num_vaccinated_staff
  )]
  
  return(weekly_data)
}


