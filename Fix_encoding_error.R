#fix encoding error

form <- read_sheet("https://docs.google.com/spreadsheets/d/1wwOfwz_QqwXpNxKx4Jupn336x2GttOfoXKibd-_rMuc")

## clean names----
form <- form |>
  janitor::clean_names()
## explore----
#names(form)

saveRDS(form, "QR/data/cache_data/form.RDS")

# ### select Norwegian studies----
Nor_form <- form |>
  filter(grepl("Norway", country))

### selet studies that are included in the review----
Nor_form_inc <- Nor_form |>
  filter(does_the_study_assess_land_use_or_land_use_change_effects_on_the_biodiversity_ecosystem_services_functions_or_carbon_sequestration_and_storage_in_terrestrial_aquatic_or_coastal_areas_are_include_i_e_studies_focusing_on_marine_systems_are_excluded == "Yes" | does_the_study_assess_conflicts_tools_or_governance_relating_to_land_use_and_cover_change_its_effects_on_biodiversity_ecosystem_services_and_or_carbon_sequestration_and_storage_by_ecosystems_in_fenno_scandinavia_the_relation_should_be_explicit_in_study_for_the_record_to_be_included == "Yes")

### Get matched details from Rayyan----

# read the Rayyan results

rayyan_biblio <- synthesisr::read_ref("data/Extraction/articles.ris")
rayyan_biblio_cleaned <- rayyanR::parse_rayyan(rayyan_df = rayyan_biblio)

Nor_form_inc$accession_zr<-unlist(Nor_form_inc$study_id)

join_Nor_form <- Nor_form_inc |>
  left_join(rayyan_biblio_cleaned, join_by("accession_zr"))

# join_Nor_form |> view()

join_Nor_form |>
  filter(is.na(join_Nor_form$doi)) |>
  select(study_id, study_title)

#### find missing doi's by title
which(is.na(join_Nor_form$doi))
#11, 47,52,60,61,63,74,75,125,158, 217

join_Nor_form[11, ]$doi <- "10.3233/hsm-1986-6105"
join_Nor_form[47, ]$doi <- "10.1017/s0024282996000424"
join_Nor_form[52, ]$doi <- "10.1127/nova.hedwigia/66/1998/283"
join_Nor_form[60, ]$doi <- NA
join_Nor_form[61, ]$doi <- "10.14430/arctic829"
join_Nor_form[63, ]$doi<-"10.1007/s002670010195"
join_Nor_form[c(74,75,125,158, 217), ]$doi<-NA

# get openAlexR data


doi_list <- strsplit(join_Nor_form$doi, split = " ")
doi_list <- sapply(doi_list, "[[", 1)


bib_ls <- list(
  doi = c(doi_list),
  entity = "works"
)

nor_bib <- do.call(oa_fetch, bib_ls) %>%
  oa2bibliometrix()
saveRDS(nor_bib, "QR/data/cache_data/nor_bib.RDS")

nor_bib <- readRDS(here::here("QR/data/cache_data/nor_bib.RDS"))

join_Nor_form<-readRDS(here::here("QR/data/cache_data/join_Nor_form.RDS"))


#join_Nor_form[grep("Sundt", join_Nor_form$author),]

join_Nor_form[140,]$author<- "Junker kohler, B. and Sundt, H."

#which(join_Nor_form$study_id=="rayyan-674230320")
join_Nor_form[17,]$address<-"Norwegian Institute for Nature Research, Trondheim,Norway"

#join_Nor_form[grep("Norwegian Institute for Water Research", join_Nor_form$address),]$title

#join_Nor_form[grep("Department of Finance, Accounting and Risk", join_Nor_form$address),]

#which(join_Nor_form$study_id=="rayyan-674232781")

#which(join_Nor_form$title=="Light and temperature controls of aquatic plant photosynthesis downstream of a hydropower plant and the effect of plant removal")


join_Nor_form[135,]$title<-"Best practiceIs natural revegetation sufficient to achieve mitigation goals in road construction?"

join_Nor_form[130,]$title<-"Habitat Protection Approaches Facilitate Conservation of Overlooked Fungal Diversity A Case Study From the Norwegian Coastal Heathland System"
join_Nor_form[69,]$title<-"A stand level scenario model for the Norwegian forestry a case study on forest management under climate change"

join_Nor_form[62,]$title<-"Choice of metrics matters Future scenarios on milk and beef production in Norway using an LCA approach"
        
join_Nor_form[111,]$address<-"[\"Norwegian Institute for Water Research (NIVA)\", \"Faculty of Environmental Sciences and Natural Resource Management, Norwegian University of Life Sciences, Norway\", \"Norwegian Research Centre, Bergen, 5008, Norway\", \"Department of Biological Sciences, University of Bergen, ThormC8hlensgate 53 A & B, Bergen, 5006, Norway\", \"Department Aquatic Ecosystem Analysis (ASAM), Helmholtz Centre for Environmental Research, Germany\""

join_Nor_form[228,]$address<-"[\"Department of Finance, Accounting and Risk, Glasgow School for Business and Society, Glasgow Caledonian University\", \"The James Hutton Institute, UK, United Kingdom\", \"Norwegian University of Science and Technology (NTNU), Norway\", \"Departamento de EconomC-a y Ciencias Sociales, Universitat PolitC(cnica de ValC(ncia (UPV), Spain\", \"Department of Agriculture, Food and Environment, University of Pisa (Pisa), Italy\"]" 


saveRDS(join_Nor_form, "QR/data/cache_data/join_Nor_form.RDS")
join_Nor_form<-readRDS(here::here("QR/data/cache_data/join_Nor_form.RDS"))

#split social sci responses 

join_Nor_form_soc<-join_Nor_form |>
  filter(does_the_study_assess_conflicts_tools_or_governance_relating_to_land_use_and_cover_change_its_effects_on_biodiversity_ecosystem_services_and_or_carbon_sequestration_and_storage_by_ecosystems_in_fenno_scandinavia_the_relation_should_be_explicit_in_study_for_the_record_to_be_included=="Yes")

join_Nor_form_nat<-join_Nor_form |>
  filter(does_the_study_assess_conflicts_tools_or_governance_relating_to_land_use_and_cover_change_its_effects_on_biodiversity_ecosystem_services_and_or_carbon_sequestration_and_storage_by_ecosystems_in_fenno_scandinavia_the_relation_should_be_explicit_in_study_for_the_record_to_be_included=="No")





saveRDS(join_Nor_form_soc, "QR/data/cache_data/join_Nor_form_soc.RDS")
saveRDS(join_Nor_form_nat, "QR/data/cache_data/join_Nor_form_nat.RDS")

join_Nor_form_soc<-readRDS("QR/data/cache_data/join_Nor_form_soc.RDS")
join_Nor_form_nat<-readRDS("QR/data/cache_data/join_Nor_form_nat.RDS")

