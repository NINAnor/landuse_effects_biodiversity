library(tidyverse)
library(googlesheets4)
library(dplyr, warn.conflicts = FALSE)
library(tidygeocoder)
library(rayyanR)
#gs4_auth()
## read in GoogleSheets----
#form <- read_sheet("https://docs.google.com/spreadsheets/d/1wwOfwz_QqwXpNxKx4Jupn336x2GttOfoXKibd-_rMuc")
#saveRDS(form, "data/form.RDS")
form<-readRDS("data/form.RDS")
country_centroid<-read.csv("https://raw.githubusercontent.com/gavinr/world-countries-centroids/master/dist/countries.csv")


# ## clean names----
form <- form |>
   janitor::clean_names()

### Get matched details from Rayyan----
# 
# # read the Rayyan results
# 
rayyan_biblio <- synthesisr::read_ref("data/Extraction/articles.ris")
rayyan_biblio_cleaned <- rayyanR::parse_rayyan(rayyan_df = rayyan_biblio)

form$accession_zr<-unlist(form$study_id)
 
join_form <- form |>
   left_join(rayyan_biblio_cleaned, join_by("accession_zr"))
 
#join_form |> view()

#names(join_form)

unnest_country<-join_form |> 
  mutate(country=strsplit(country, ",")) |> 
  unnest(country) 


join<-unnest_country |> 
  full_join(country_centroid, by=c("country"="COUNTRY"))   
join$doi
join$url


doi_list <- strsplit(join$doi, split = " ")
doi_list <- sapply(doi_list, "[[", 1)

doi_list<-paste0("https://doi.org/", doi_list)


join$url<-doi_list


join |> 
  select(study_id, study_title, source, doi, url, longitude, latitude, country) |> 
  mutate(longitude=case_when(
    country=="Norway"~10.421906,
    TRUE~longitude),
    latitude=case_when(
      country=="Norway"~63.446827,
      TRUE~latitude)) |> 
  drop_na(study_title) |>
  write.csv("data/EviAtlas.csv")


