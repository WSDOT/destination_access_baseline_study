#wa_state_places data summary/explore
#Tidy the ploace of interest export - summarize, filter into categories, and export to excel file
#After initial tidy, this workflow is done by loading wa_state_places.RData tables
##just saved wa_tidy_geom, can skip the rm fxn in line 9 now

library(tidyverse)

#remove unnessesary pieces of loaded environment
rm(dfwa, map_themes, wa_places, wa_places_sf, wa_tidy1, manual_bbox, overture_data)


#Prior to beginning load in existing Rdata set from Overture Export----
glimpse(wa_tidy_geom)

#tidy the destination dataset to only include those with WA address & update names 
renamed_places <- wa_tidy_geom |> 
  select(-version, -filename, -theme, -type, , -`brand$wikidata`) |> 
  filter(`addresses,region` == 'WA') |> 
  rename(
    lon_min = `bbox$xmin`, 
    lon_max = `bbox$xmax`,
    lat_min = `bbox$ymin`, 
    lat_max = `bbox$ymax`, 
    business_name = `names$primary`, 
    sub_category = `categories$primary`,
    broad_category = basic_category, 
    brand_name = `brand$names$primary`, 
    street_num = `addresses,freeform`,
    jurisdiction = `addresses,locality`,
    postcode = `addresses,postcode`,
    reg = `addresses,region`,
    country = `addresses,country`
  ) |> 
  relocate(lat, lon, .after = id) |> 
  relocate(confidence, .before = operating_status)
  

#removes exact matches of name + address, retaining higher confidence
places_dedup <- renamed_places |> 
  filter(confidence > 0.85) |> #filters lower confidence score locations
  arrange(desc(confidence)) |> #sorts remaining by confidence
  distinct(business_name, street_num, .keep_all = TRUE) |> #removes identical duplicates, retaining option with higher score
  filter(street_num != 'NA') |> #removes sites without actual street number
  filter(jurisdiction != 'NA') #removes sites without jurisdiction
  #group_by(street_num, sub_category) |> #removes duplicate street address locations with same school category, ie no two elementary schools at same address
  #mutate(dupe = n()>1, 
   #      dup_id = row_number()) |> 
  #filter(dup_id == 1) |> 
  #group_by(business_name, jurisdiction) |> #removes duplicates with same name within same jurisdiction
  #mutate(dup_jur = n()>1, 
   #      dup_jur_id = row_number()) |> 
  #filter(dup_jur_id == 1) |> 
 # select(-dupe, -dup_id)


print(places_dedup)


#get counts of different destination categories by count----
categories_by_count <- renamed_places |> 
  count(broad_category, sub_category) 
  
dedup_catetgories_by_count <- places_dedup |> 
  count(broad_category, sub_category)

#Conveyal export process Filter to specific category types (still need to simplify output file more) ----
restaurants <- places_dedup |> 
  filter(broad_category == "restaurant")

write.csv(places_dedup, file = "./overture_WA_places_dedup_04_26.csv")

save(places_dedup, file = "./data/wa_places_dedup_04_26.Rdata")



       