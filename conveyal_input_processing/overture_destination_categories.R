#goal in this script is to categorize Overture datasets into WSDOT Access to Destination analysis
#this script starts after overture_rename_dedup and utilizes dfs created in that script
#the outputs of this script will contain options to export individual groups of destinations or the combined dataset with a new category
#There is also the option to change the weight of each destination, which can change how much a given destination  impact threshold analyses in Conveyal

#script starts with places_dedup in environment from "~/Projects/R Explore/Overture/output/wa_places_renamed.RData" or running the rename_dedup script

library(tidyverse)
library(httr)
library(jsonlite)
library(sf)

library(mapview)

?sf

getwd()

load("./data/wa_places_dedup_04_26.Rdata")
load("~/Projects/R Explore/Overture/data/official_hospitals.RData")

cleaned_overture_places <- places_dedup |> 
  select(-wikidata, -brand_name, -lon_min, -lon_max, -lat_min, -lat_max)

places_dedup$source <- "Overture"

#medical categories (updated 4.15.26) ----
medical_category <- places_dedup |> 
  filter(sub_category == "pharmacy" | 
         broad_category == "clinic_or_treatment_center" & sub_category == "urgent_care_clinic" |
         broad_category == "primary_care_or_general_clinic" |
         sub_category == "doctor" |
         broad_category == "healthcare_location" & sub_category == "medical_center" |
         broad_category == "healthcare_location" & sub_category == "health_and_medical" |
         sub_category == "general_dentistry" |
         sub_category == "dentist" |
         sub_category == "pediatric_dentist" |
         sub_category == "optometrist" |
         broad_category == "reproductive_perinatal_and_womens_care" & sub_category == "obstetrician_and_gynecologist" |
         broad_category == "healthcare_location" & sub_category == "obstetrician_and_gynecologist" |
         broad_category == "physical_medicine_and_rehabilitation" & sub_category == "physical_therapy" |
         sub_category == "dialysis_clinic" |
         sub_category == "oncologist" |
         sub_category == "occupational_therapy" | 
           broad_category == "behavioral_or_mental_health_clinic" & sub_category == "psychotherapist" |
           broad_category == "behavioral_or_mental_health_clinic" & sub_category == "psychologist" 
         #broad_category == "hospital" & sub_category == "hospital" |
         #broad_category == "childrens_hospital" & sub_category == "childrens_hospital"
         )

place_categories <- places_dedup |> 
  count(broad_category, sub_category)

 #add weighting value to category with below - setting at 0.25, as we're not planning to use current
 medical_category$Weight <- 1
 #add column describing category type - useful 
 medical_category$WSDOT_Category <- "Medical"
 
 str(medical_category)

 pharmacies_med <- medical_category |> 
   select(id, lat, lon, business_name, broad_category, sub_category, source) |>
   filter(sub_category == "pharmacy")
 
 west_pharm <- filter(pharmacies_med, pharmacies_med$lon < -121.07758)
 east_pharm <- filter(pharmacies_med, pharmacies_med$lon > -121.35223)
 
 primary_urgent_family_med <- medical_category |> #coming in a bit low
   select(id, lat, lon, business_name, broad_category, sub_category, source) |>
   filter(broad_category == "clinic_or_treatment_center" & sub_category == "urgent_care_clinic" |
            broad_category == "primary_care_or_general_clinic" |
            sub_category == "doctor" |
            broad_category == "healthcare_location" & sub_category == "medical_center" |
            broad_category == "healthcare_location" & sub_category == "health_and_medical" #removed for now, including full group was causing destination maximum in Conveyal to be exceeded
            )
 west_primary <- filter(primary_urgent_family_med, primary_urgent_family_med$lon < -121.07758)
 east_primary <- filter(primary_urgent_family_med, primary_urgent_family_med$lon > -121.35223)
 
 vision_med <- medical_category |> 
   select(id, lat, lon, business_name, broad_category, sub_category, source) |>
   filter( sub_category == "optometrist")
 
 west_vision <- filter(vision_med, vision_med$lon < -121.07758)
 east_vision <- filter(vision_med, vision_med$lon > -121.35223)
 
 dental_med <- medical_category |> 
   select(id, lat, lon, business_name, broad_category, sub_category, source) |>
   filter( sub_category == "general_dentistry" |
             sub_category == "dentist" |
             sub_category == "pediatric_dentist" )
 

  west_dental <- filter(dental_med, dental_med$lon < -121.07758)
  east_dental <- filter(dental_med, dental_med$lon > -121.35223)
 
  
specialty_med <- medical_category |> 
    select(id, lat, lon, business_name, broad_category, sub_category, source) |>
             filter( broad_category == "reproductive_perinatal_and_womens_care" & sub_category == "obstetrician_and_gynecologist" |
                   broad_category == "healthcare_location" & sub_category == "obstetrician_and_gynecologist" |
                   broad_category == "healthcare_location" & sub_category == "obstetrician_and_gynecologist" |
                   broad_category == "physical_medicine_and_rehabilitation" & sub_category == "physical_therapy" |
                   sub_category == "dialysis_clinic" |
                   sub_category == "oncologist" |
                   sub_category == "occupational_therapy" | 
                   broad_category == "behavioral_or_mental_health_clinic" & sub_category == "psychotherapist" |
                   broad_category == "behavioral_or_mental_health_clinic" & sub_category == "psychologist" )

west_med_spec <- filter(specialty_med, specialty_med$lon < -121.07758)
east_med_spec <- filter(specialty_med, specialty_med$lon > -121.35223)
  
 #save csv for conveyal input
 write.csv(medical_category, file = "./output/overture_other_medical_locales.csv")
 
 write.csv(west_pharm, file = "./output/west_pharmacies.csv")
 write.csv(west_primary, file = "./output/west_primary_urgent_family.csv")
 write.csv(west_vision, file = "./output/west_vision.csv")
 write.csv(west_dental, file = "./output/west_dental.csv")
 write.csv(west_med_spec, file = "./output/west_med_spec.csv")
 
 #east export
 write.csv(east_pharm, file = "./output/east_pharmacies.csv")
 write.csv(east_primary, file = "./output/east_primary_urgent_family.csv")
 write.csv(east_vision, file = "./output/east_vision.csv")
 write.csv(east_dental, file = "./output/east_dental.csv")
 write.csv(east_med_spec, file = "./output/east_med_spec.csv")
 
 
 
 #hospitals - local download/access also an option -----
 official_hospitals <- fromJSON('https://services8.arcgis.com/rGGrs6HCnw87OFOT/arcgis/rest/services/Hospitals/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson')
 
 str(official_hospitals)
 save(official_hospitals, file = "./data/official_hospitals.RData")

#pulls only list of features of from list of 2 - cannot unnest the sub-col/lists prior to this step
unnest_hos <- official_hospitals$features |> 
  unnest_wider(geometry, names_sep = "$") |> 
  unnest_wider(`geometry$coordinates`, names_sep = " ") |>
  unnest_wider(`geometry$coordinates 1`, names_sep = ",") |> 
  unnest_wider(properties, names_sep = "$") |> 
  rename("lon" =`geometry$coordinates 1,1`, 
         "lat" = `geometry$coordinates 1,2`)

colnames(unnest_hos) <- sub("'properties$", "", colnames(unnest_hos)) 

unnest_hos <- unnest_hos |> 
  select(-`properties$PHONE`,
         -`properties$FAX`, -`properties$Weblink`) |> 
  rename(name = `properties$NAME`,
         object_id = `properties$OBJECTID`,
         link = `properties$LINK`, 
         street_num = `properties$ADDRESS`,
         jurisdiction = `properties$CITY`,
         postcode= `properties$ZIP`,
         zip4 = `properties$ZIP4`,
         acute = `properties$ACUTE`,
         ICU = `properties$ICU`,
         CAH = `properties$CAH`,
         heli = `properties$Heli`,
         beds_total = `properties$Beds_Total`,
         beds_total_ICU = `properties$Beds_Total_ICU`,
         updated = `properties$Updated`,
         beds_total_psychiatric = `properties$Beds_Psychiatric`) 
  
unnest_hos$Weight <- 1
unnest_hos$WSDOT_Category <- "Medical"
unnest_hos$source <- "WA_Open_Data_Portal"

str(unnest_hos)

#split destinations by Conveyal Region
west_hospitals <- filter(unnest_hos, unnest_hos$lon < -121.07758)
east_hospitals <- filter(unnest_hos, unnest_hos$lon > -121.35223)

#save west csv for conveyal input
write.csv(west_hospitals |> select(id, lon, lat, name, Weight, source)
  , file = "./output/west_hospitals.csv")

#save east csv for conveyal input
write.csv(east_hospitals |> select(id, lon, lat, name, Weight, source)
          , file = "./output/east_hospitals.csv")
 
 #alternative workflow load in downloaded local version official WA DOH Hospitals 
 official_hospitals <- read.csv('~/Projects/R Explore/Overture/data/Hospitals_WA.csv')
 
 
 
#community support services ------ 
#Need more work on universities/colleges if we want actual geometries
community_support_services_category <- places_dedup |> 
  filter(broad_category == "library" | 
           broad_category == "post_office" & sub_category == "post_office" |
           grepl("bank|bank_credit_union", broad_category) & sub_category == "banks" |
           broad_category == "atm" & sub_category == "atms" |
           broad_category == "community_center" & sub_category == "community_center" |
           broad_category == "college_university" & sub_category == "college_university" |
           broad_category == "social_or_community_service" & sub_category == "senior_citizen_services" |
           broad_category == "family_service" & sub_category == "day_care_preschool" |
           broad_category == "preschool" & sub_category == "preschool" |
           broad_category == "electronics_store" & sub_category == "mobile_phone_store" |
           broad_category == "buddhist_place_of_worship" |
           broad_category == "christian_place_of_worship" & sub_category == "anglican_church" |
           broad_category == "christian_place_of_worship" & sub_category == "baptist_church" |
           broad_category == "christian_place_of_worship" & sub_category == "catholic_church" |
           broad_category == "christian_place_of_worship" & sub_category == "church_cathedral" |
           broad_category == "christian_place_of_worship" & sub_category == "episcopal_church" |
           broad_category == "christian_place_of_worship" & sub_category == "evangelical_church" |
           broad_category == "christian_place_of_worship" & sub_category == "jehovahs_witness_kingdom_hall" |
           broad_category == "christian_place_of_worship" & sub_category == "pentecostal_church" |
           broad_category == "hindu_place_of_worship" |
           broad_category == "jewish_place_of_worship" |
           broad_category == "muslim_place_of_worship" |
           broad_category == "place_of_worship" & sub_category == "sikh_temple" |
           sub_category == "public_and_government_association" |
           sub_category == "electric_utility_provider" & !grepl('Station|Substation|Plant', business_name) | #need to remove sub-stations, but many bill pay destinations
           sub_category == "public_utility_company" & !grepl('Dam|Plant', business_name) | #remove dam
           sub_category == "mobile_phone_store" |
           sub_category == "telecommunications_company" |
           sub_category == "internet_service_provider" |
           sub_category == "gym"
         )    
  #add weighting value to category with below
  ##check on categories - sub_category public_and_government_association,public_service_and_government,public_utility_company, mobile_phone_store
  
 community_support_services_category$Weight <- 1
 #add column describing category type - useful 
 community_support_services_category$WSDOT_Category <- "CSS"
 
library_css <- community_support_services_category |> 
select(id, lat, lon, business_name, broad_category, sub_category, source) |>
filter(broad_category == "library")

west_lib <- filter(library_css, library_css$lon < -121.07758)
east_lib <- filter(library_css, library_css$lon > -121.35223)


banks_css <- community_support_services_category |> 
  select(id, lat, lon, business_name, broad_category, sub_category, source) |>
  filter(grepl("bank|atm|bank_credit_union", broad_category))

west_banks <- filter(banks_css, banks_css$lon < -121.07758)
east_banks <- filter(banks_css, banks_css$lon > -121.35223)


#may want to handle campuses like parks - can use below/continue building filter if not
# Universities_css <- community_support_services_category |> 
#   distinct(street_num, .keep_all = TRUE) |>
#   filter(broad_category == "college_university" & 
#     grepl('University', business_name) & 
#     !grepl('School|Department|Societies|Marine Biology|MFA|Beef|Board|Admissions|Program|College|CoMotion|Computer Science|Gate|Epidemiology|Studies|Zeta', business_name) & 
#     !grepl('25022 104th Ave SE | 3823 172nd St NE', street_num)) |>  #needs to be narrowed - includes multiples schools/buildings with same school. 
#   distinct(business_name, jurisdiction, .keep_all = TRUE) |> 
#   select(id, lat, lon, business_name, broad_category, sub_category, source)
#   
# 
# west_uni <- filter(Universities_css, Universities_css$lon < -121.07758)
# east_uni <- filter(Universities_css, Universities_css$lon > -121.35223)
# 
# 
# college_css <- community_support_services_category |> 
#   select(id, lat, lon, business_name, broad_category, sub_category, source) |>
#   filter(broad_category == "college_university" & grepl('College', business_name) )#needs to be narrowed - includes multiples schools/buildings with same school. 
# 
# west_coll <- filter(college_css, college_css$lon < -121.07758)
# east_coll <- filter(college_css, college_css$lon > -121.35223)




senior_css <- community_support_services_category |> 
  select(id, lat, lon, business_name, broad_category, sub_category, source) |>
  filter(sub_category == "senior_citizen_services")

west_senior <- filter(senior_css, senior_css$lon < -121.07758)
east_senior <- filter(senior_css, senior_css$lon > -121.35223)



childcare_css <- community_support_services_category |> 
  select(id, lat, lon, business_name, broad_category, sub_category, source) |>
  filter(broad_category == "family_service" & sub_category == "day_care_preschool" |
           broad_category == "preschool" & sub_category == "preschool")

west_child <- filter(childcare_css, childcare_css$lon < -121.07758)
east_child <- filter(childcare_css, childcare_css$lon > -121.35223)

POW_css <- community_support_services_category |> 
  select(id, lat, lon, business_name, broad_category, sub_category, source) |>
  filter(broad_category == "buddhist_place_of_worship" |
           broad_category == "christian_place_of_worship" & sub_category == "anglican_church" |
           broad_category == "christian_place_of_worship" & sub_category == "baptist_church" |
           broad_category == "christian_place_of_worship" & sub_category == "catholic_church" |
           broad_category == "christian_place_of_worship" & sub_category == "church_cathedral" |
           broad_category == "christian_place_of_worship" & sub_category == "episcopal_church" |
           broad_category == "christian_place_of_worship" & sub_category == "evangelical_church" |
           broad_category == "christian_place_of_worship" & sub_category == "jehovahs_witness_kingdom_hall" |
           broad_category == "christian_place_of_worship" & sub_category == "pentecostal_church" |
           broad_category == "hindu_place_of_worship" |
           broad_category == "jewish_place_of_worship" |
           broad_category == "muslim_place_of_worship" |
           broad_category == "place_of_worship" & sub_category == "sikh_temple")

west_PoW <- filter(POW_css, POW_css$lon < -121.07758)
east_PoW <- filter(POW_css, POW_css$lon > -121.35223)


commcen_css <- community_support_services_category |> 
  select(id, lat, lon, business_name, broad_category, sub_category, source) |>
  filter(broad_category == "community_center")

west_comcen <- filter(commcen_css, commcen_css$lon < -121.07758)
east_comcen <- filter(commcen_css, commcen_css$lon > -121.35223)


PO_css <- community_support_services_category |> 
  select(id, lat, lon, business_name, broad_category, sub_category, source) |>
  filter(broad_category == "post_office")

west_PO <- filter(PO_css, PO_css$lon < -121.07758)
east_PO <- filter(PO_css, PO_css$lon > -121.35223)

billpay_css <- community_support_services_category |> 
  select(id, lat, lon, business_name, broad_category, sub_category, source) |>
  filter(sub_category == "public_and_government_association" |
           sub_category == "electric_utility_provider" |
           sub_category == "public_utility_company" |
           sub_category == "mobile_phone_store" |
           sub_category == "telecommunications_company" |
           sub_category == "internet_service_provider")

west_bill <- filter(billpay_css, billpay_css$lon < -121.07758)
east_bill <- filter(billpay_css, billpay_css$lon > -121.35223)

#addition of gym -
gym_css <- community_support_services_category |> 
  select(id, lat, lon, business_name, broad_category, sub_category, source) |>
  filter(sub_category == "gym"
           )
  
west_gym <- filter(gym_css, gym_css$lon < -121.07758)
east_gym <- filter(gym_css, gym_css$lon > -121.35223)

#save csv for conveyal input
write.csv(community_support_services_category, file = "./output/CSS/overture_css.csv")

write.csv(west_lib, file = "./output/CSS/west_lib.csv")
write.csv(west_banks, file = "./output/CSS/west_banks.csv")
write.csv(west_senior, file = "./output/CSS/west_senior.csv")
write.csv(west_child, file = "./output/CSS/west_child.csv")
write.csv(west_PoW, file = "./output/CSS/west_PoW.csv")
write.csv(west_comcen, file = "./output/CSS/west_comcen.csv")
write.csv(west_PO, file = "./output/CSS/west_PO.csv")
write.csv(west_bill, file = "./output/CSS/west_bill.csv")
write.csv(west_gym, file = "./output/CSS/west_gym.csv")


#east export
write.csv(east_lib, file = "./output/CSS/east_lib.csv")
write.csv(east_banks, file = "./output/CSS/east_banks.csv")
write.csv(east_senior, file = "./output/CSS/east_senior.csv")
write.csv(east_child, file = "./output/CSS/east_child.csv")
write.csv(east_PoW, file = "./output/CSS/east_PoW.csv")
write.csv(east_comcen, file = "./output/CSS/east_comcen.csv")
write.csv(east_PO, file = "./output/CSS/east_PO.csv")
write.csv(east_bill, file = "./output/CSS/east_bill.csv")
write.csv(east_gym, file = "./output/CSS/east_gym.csv")
 
#food -----

food_category <- places_dedup |> 
  filter(broad_category == "food_and_beverage_store" & sub_category == "grocery_store" |
           broad_category == "food_bank" & sub_category == "food_banks" |
           broad_category == "grocery_store") |> 
  select(id, lat, lon, business_name, broad_category, sub_category, source)

food_category$Weight <- 1
food_category$WSDOT_Category <- "Food"


west_food <- filter(food_category, food_category$lon < -121.07758)
east_food <- filter(food_category, food_category$lon > -121.35223)

write.csv(east_food, file = "./output/east_food.csv")
write.csv(west_food, file = "./output/west_food.csv")




#Parks ----
#Potential to just use what is currently in Conveyal
park_rec_category <- places_dedup |> 
  filter(broad_category == "park" & sub_category == "park"
           ) 

 park_rec_dif <- park_rec_category |>  
  mutate(
    latdif = lat_max - lat_min, 
    londif = lon_max - lon_min
  )

park_rec_category |> plot(lat_min, lat_max)

#leisure ---- 
leis_rec_art_culture_category <- places_dedup |> 
  filter(broad_category == "clothing_store" & sub_category == "clothing_store" |
           broad_category == "cafe" & sub_category == "cafe" |
           broad_category == "restaurant" | #may want to check sub-cats
           broad_category == "pizzaria" & sub_category == "cafe" |
           broad_category == "bar" |
           sub_category =="bar_and_grill_restaurant" | 
           sub_category == "chicken_restaurant" |
           sub_category == "arts_and_entertainment" |
           sub_category == "cultural_center" |
           sub_category == "brewery" |
           sub_category == "winery" |
           broad_category == "music_venue" |
           broad_category == "theatre_venue" |
           broad_category == "performing_arts_venue" |
           sub_category == "sports_and_recreation_venue" |
           broad_category == "gaming_venue" |
           broad_category == "museum" |
           broad_category =="art_gallery"
  ) |> 
  select(1:3, 5, 6, 7, 8, business_name, confidence, broad_category, 4)

leis_cat_sf <- st_as_sf(leis_rec_art_culture_category, coords = c("lon", "lat"), crs = 4326)

# leis_cat_sf$nearest <- leis_cat_sf |> 
#   st_nearest_feature(leis_cat_sf, leis_cat_sf)
  

m_leis <- st_transform(leis_cat_sf, crs = 7801) 

plot(leis_cat_sf)

buff_200 <- st_buffer(m_leis, dist = 200) 
  
buff_200_reproj <- buff_200 |> 
  st_transform(crs = 4326)
  
leisure_recreation_category <- m_leis |> 
  mutate(within_200 = lengths(st_is_within_distance(x = m_leis, 
                                                    y = m_leis, 
                                                    dist = 200))) |>
  filter(within_200 > 2) |> 
  st_transform(crs = 4326) |> 
  select(id, sub_category, geometry) |> 
  mutate(lon = sf::st_coordinates(geometry)[,1],
         lat = sf::st_coordinates(geometry)[,2],) |> 
  st_drop_geometry()

west_leisure <- filter(leisure_recreation_category, leisure_recreation_category$lon < -121.07758)
east_leisure <- filter(leisure_recreation_category, leisure_recreation_category$lon > -121.35223)

write.csv(west_leisure, file = "./output/west_leisure.csv")
write.csv(east_leisure, file = "./output/east_leisure.csv")




