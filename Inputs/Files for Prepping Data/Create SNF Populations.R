
snf_people <- NULL #Define an object to hold the nursing facility residents

sp_id_max = max(people$sp_id) #find the maximum id of the community population to ensure no overlap in ids

num_res = nursingfacility[,nresid, by=snf_id] #find the number of residents per facility

snf_people <- data.table(sp_id = sp_id_max + seq(1:sum(num_res[,nresid])), sp_snf_id = rep(nursingfacility[,snf_id],num_res[,nresid])) #create a data table and define the unique ids and snf ids for each individual

snf_people <- merge(snf_people, nursingfacility, by.x="sp_snf_id", by.y = "snf_id") #merge with the nursing facility information

snf_people[, `:=` (age = round(rtruncnorm(1, avgage, 14, -Inf, 100)), sex = ifelse(runif(1) < sex_m, "M", "F"), race = ifelse(runif(1,0,100) < race_w, 1, ifelse(runif(1) < race_b/(race_b + race_ns), 2, 3))), by=sp_id] #add columns for age, sex and race based on the averages for each facility

snf_people <- snf_people[,-c("worker_hours","nresid","beds","latitude","longitude","county id", "county_id" , "sex_f", "sex_m", "race_w", "race_b", "race_ns", "admit_h", "admit_c", "admit_u", "avgage", "workers", "dis", "los")] #remove unneeded columns

rm(sp_id_max, num_res) #clean up variables
gc() #clear memory 

