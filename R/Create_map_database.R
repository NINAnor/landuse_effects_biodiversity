library(tidyverse)
library(googlesheets4)
library(dplyr, warn.conflicts = FALSE)
library(tidygeocoder)
library(rayyanR)

join<-rbind(join_Nor_form_nat[1:53], join_Nor_form_soc[1:53])

names(join)[48]<-"notes"
All_join<-rbind(join,sysRev)
country_centroid<-read.csv("https://raw.githubusercontent.com/gavinr/world-countries-centroids/master/dist/countries.csv")

### Get matched details from Rayyan----
# 
# # read the Rayyan results
# 
rayyan_biblio <- synthesisr::read_ref("data/Extraction/articles.ris")
rayyan_biblio_cleaned <- rayyanR::parse_rayyan(rayyan_df = rayyan_biblio)

All_join$accession_zr<-unlist(All_join$study_id)
 
join_form <- All_join |>
   left_join(rayyan_biblio_cleaned, join_by("accession_zr"))

#join_form$country
 
unnest_country<-
  join_form |> 
  mutate(country=strsplit(country, ",")) |> 
  unnest(country) |> 
    mutate(country=trimws(country)) 


join<-unnest_country |> 
  full_join(country_centroid, by=c("country"="COUNTRY"))   

#join|> view()
doi_list <- strsplit(join$doi, split = " ")
doi_list <- sapply(doi_list, "[[", 1)

doi_list<-paste0("https://doi.org/", doi_list)


join$url<-doi_list

#names(join)

join_db<-join |> 
  select(study_id, study_title, source, abstract,keywords,doi, url, longitude, latitude, country, ecosystem_type_main, research_type) |> 
  mutate(longitude=case_when(
    country=="Norway"~10.421906,
    TRUE~longitude),
    latitude=case_when(
      country=="Norway"~63.446827,
      TRUE~latitude)) |> 
  drop_na(study_title) 

#join_db |> view()

join_db |> 
  write.csv("data/EviAtlas.csv")

join_db |> 
  select(study_title, source,keywords,doi, url, longitude, latitude, country, ecosystem_type_main, research_type) |> 
  mutate(longitude=case_when(
    country=="Norway"~10.421906,
    TRUE~longitude),
    latitude=case_when(
      country=="Norway"~63.446827,
      TRUE~latitude)) |> 
  drop_na(study_title) |> 
  distinct(study_title, .keep_all = TRUE) |> 
  saveRDS("data/EVIATLAS.RDS")

