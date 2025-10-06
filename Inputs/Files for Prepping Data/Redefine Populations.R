
gq_people_merge <- merge(gq_people, gq, by.x = "sp_gq_id", by.y = "sp_id", all.x = TRUE) #merge to add gq characteristics to the gq population 

people_hh_merge <- merge(people, households, by.x = "sp_hh_id", by.y = "sp_id", all.x = TRUE) #merge household characteristics to the population.

otherGroupQuartersPop <- gq_people_merge[gq_type != "N",] #remove the nursing home residents

otherGroupQuarters <- gq[gq_type != "N",] #remove the nursing homes from the group quarters

communityPopulation <- data.table(
  sp_id = c(people_hh_merge$sp_id, otherGroupQuartersPop$sp_id), #unique individual ID
  home_id = c(people_hh_merge$sp_hh_id, otherGroupQuartersPop$sp_gq_id), #unique living quarters ID (regardless of whether this location is a household or a group quarters)
  home_type = c(rep("hh", nrow(people_hh_merge)), otherGroupQuartersPop$gq_type), #reclassify home types (Note that gq_types for the relevant observations remain)
  age = c(people_hh_merge$age, otherGroupQuartersPop$age), #individual age
  sex = c(people_hh_merge$sex, otherGroupQuartersPop$sex), #individual sex
  race = c(people_hh_merge$race, otherGroupQuartersPop$race),
  work_id = c(ifelse(people_hh_merge$work_id != "X", people_hh_merge$work_id, NA), rep(NA, nrow(otherGroupQuartersPop))) #individual work_id (Note that people in group quarters do not have associated work_ids)
)

communityPopulation[race != 1 & race != 2, race := 3]

hospitals[ , beds_filled := ceiling(beds*0.4)] #define how many of the beds in each hospital are filled by patients. For simplicity here, we'll just assume all hospitals are at 40% capacity (rounded up to the nearest whole number). 

nursingFacilityResidents <- snf_people

#Clean up the data frames and remove unneeded variables

nursingfacility <- nursingfacility[ , -c("county id", "county_id" , "sex_f", "sex_m", "race_w", "race_b", "race_ns", "admit_h", "admit_c", "admit_u", "avgage", "workers")]

rm(people_hh_merge, gq, gq_people, gq_people_merge, otherGroupQuartersPop, people, snf_people)

gc()
