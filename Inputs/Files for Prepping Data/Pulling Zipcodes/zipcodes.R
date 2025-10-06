library(sp)
library(rgdal)
library(data.table)
library(sf)
library(terra)


v <- data.table(id = hospitals$hosp_id,
                lat = hospitals$latitude,
                long = hospitals$longitude)


w <- data.table(id = otherGroupQuarters$sp_id,
                lat = otherGroupQuarters$latitude,
                long = otherGroupQuarters$longitude)


x <- data.table(id = households$sp_id,
                lat = households$latitude,
                long = households$longitude)

y <- data.table(id = nursingfacility$snf_id,
                lat = nursingfacility$latitude,
                long = nursingfacility$longitude)

z <- rbind(x,y,w,v)


# Load the shapefile
zip_shapefile <- st_read("D:/Code/Pulling Zipcodes/cb_2014_us_zcta510_500k/cb_2014_us_zcta510_500k.shp")
crs_zip_shapefile <- st_crs(zip_shapefile)
# Create SpatialPointsDataFrame
coordinates_df <- SpatialPointsDataFrame(coords = matrix(c(z$long, z$lat), ncol = 2), data = data.frame(z$id), proj4string = CRS("+proj=longlat +datum=WGS84"))

coordinates_df <- st_transform(st_as_sf(coordinates_df), crs_zip_shapefile)



joined_data <- st_join(coordinates_df, zip_shapefile, join = st_intersects)

# Extract the ZIP_CODE column
zip_codes <- joined_data$ZCTA5CE10

z[,zip := zip_codes]

households[,zip:=z[id==sp_id,zip],by=sp_id]

nursingfacility[,zip:=z[id==snf_id,zip],by=snf_id]

otherGroupQuarters[,zip:=z[id==sp_id,zip],by=sp_id]

communityPopulation[,zip:=households[sp_id == home_id, zip], by = home_id]

communityPopulation[,zip:=otherGroupQuarters[sp_id == home_id, zip], by = home_id]

nursingFacilityResidents[,zip:=nursingfacility[snf_id == sp_snf_id, zip], by = sp_snf_id]

hospitals[,zip:=z[id==hosp_id,zip],by=hosp_id]

save(communityPopulation,hospitals,nursingfacility,nursingFacilityResidents, file = "Jefferson.Rdata")

gc()



#######################################


for(i in unique(communityPopulation[,zip])){
  
  num_pop <- nrow(communityPopulation[zip == i,])
  
  per_vet <- social_det[ZIPCODE == i, Vetran_Population] / social_det[ZIPCODE == i, Total_Population]
  
  per_dis <- social_det[ZIPCODE == i, Per_Disabled]
  
  per_lang_barrier <- social_det[ZIPCODE == i, rowSums(.SD), .SDcols = c("Per_No_Eng", "Per_Not_Well_Eng", "Per_Limited_Eng")]
  
  per_unemployed <- social_det[ZIPCODE == i, Per_Unemployeed]
  
  per_income_levels <- social_det[ZIPCODE == i, c(Per_Less10000,Per_10000.14999,Per_15000.24999,Per_25000.49999,Per_50000.99999,Per_More100000)]
  
  per_food_insecurity <- social_det[ZIPCODE == i, Per_Food_Stamps]
  
  per_education_levels <- social_det[ZIPCODE == i, c(Per_Less_HS, Per_HSGrad, Per_Associates, Per_Bachelors, Per_GraduateDeg)]
  
  per_house_insecurity <- social_det[ZIPCODE == i, do.call(pmax, .SD), .SDcols = c("Per_HH_No_Fuel", "Per_HH_Kitchen", "Per_HH_Plumbing", "Per_HH_NoVehicle")]
  
  per_medicaid <- social_det[ZIPCODE == i, Per_Medicaid]
  
  per_noInsurance <- social_det[ZIPCODE == i, Per_Uninsured]
  
  dis_closest_medical <- social_det[ZIPCODE == i, do.call(pmin, .SD), .SDcols = c("Dis_Urgent", "Dis_ED", "Dis_MedSurg_ICU")]
  
  
  communityPopulation[zip == i, `:=` (vetran = ifelse(runif(.N) > per_vet, 0, 1), 
                                      disabled = ifelse(runif(.N) > per_dis, 0, 1),
                                      language_barrier = ifelse(runif(.N) > per_lang_barrier, 0, 1),
                                      umeployed = ifelse(runif(.N) > per_unemployed, 0, 1),
                                      income_level = ifelse(sum(per_income_levels, na.rm = TRUE) <= 0,0,sample(1:6, size = .N, replace = TRUE, prob = per_income_levels)),
                                      edu_level = ifelse(sum(per_education_levels, na.rm = TRUE) <= 0,0,sample(1:5, size = .N, replace = TRUE, prob = per_education_levels)),
                                      food_insecurity = ifelse(runif(.N) > per_food_insecurity, 0, 1),
                                      housing_insecurity = ifelse(runif(.N) > per_house_insecurity, 0, 1),
                                      medicaid = ifelse(runif(.N) > per_medicaid, 0, 1),
                                      noHealthInsurance = ifelse(runif(.N) > per_noInsurance, 0, 1),
                                      distance_closest_medical = dis_closest_medical)]
}

communityPopulation[age < 18, `:=` (vetran = 0,
                                    language_barrier = ifelse(runif(.N) > per_lang_barrier, 0, 1),
                                    umeployed = 0,
                                    edu_level = 0)]

for(i in unique(nursingFacilityResidents[,zip])){
  
  num_pop <- nrow(nursingFacilityResidents[zip == i,])
  
  per_vet <- social_det[ZIPCODE == i, Vetran_Population] / social_det[ZIPCODE == i, Total_Population]
  
  per_dis <- social_det[ZIPCODE == i, Per_Disabled]
  
  per_lang_barrier <- social_det[ZIPCODE == i, rowSums(.SD), .SDcols = c("Per_No_Eng", "Per_Not_Well_Eng", "Per_Limited_Eng")]
  
  per_unemployed <- social_det[ZIPCODE == i, Per_Unemployeed]
  
  per_income_levels <- social_det[ZIPCODE == i, c(Per_Less10000,Per_10000.14999,Per_15000.24999,Per_25000.49999,Per_50000.99999,Per_More100000)]
  
  per_food_insecurity <- social_det[ZIPCODE == i, Per_Food_Stamps]
  
  per_education_levels <- social_det[ZIPCODE == i, c(Per_Less_HS, Per_HSGrad, Per_Associates, Per_Bachelors, Per_GraduateDeg)]
  
  
  per_house_insecurity <- social_det[ZIPCODE == i, do.call(pmax, .SD), .SDcols = c("Per_HH_No_Fuel", "Per_HH_Kitchen", "Per_HH_Plumbing", "Per_HH_NoVehicle")]
  
  per_medicaid <- social_det[ZIPCODE == i, Per_Medicaid]
  
  per_noInsurance <- social_det[ZIPCODE == i, Per_Uninsured]
  
  dis_closest_medical <- social_det[ZIPCODE == i, do.call(pmin, .SD), .SDcols = c("Dis_Urgent", "Dis_ED", "Dis_MedSurg_ICU")]
  
  
  nursingFacilityResidents[zip == i, `:=` (vetran = ifelse(runif(.N) > per_vet, 0, 1), 
                                      disabled = ifelse(runif(.N) > per_dis, 0, 1),
                                      language_barrier = ifelse(runif(.N) > per_lang_barrier, 0, 1),
                                      income_level = ifelse(sum(per_income_levels, na.rm = TRUE) <= 0,0,sample(1:6, size = .N, replace = TRUE, prob = per_income_levels)),
                                      edu_level = ifelse(sum(per_education_levels, na.rm = TRUE) <= 0,0,sample(1:5, size = .N, replace = TRUE, prob = per_education_levels)),
                                      distance_closest_medical = dis_closest_medical)]
  
  
}


save(communityPopulation,hospitals,nursingfacility,nursingFacilityResidents, file = "Inputs/29071/Franklin.Rdata")

gc()
