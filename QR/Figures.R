
### Overordnet karakterisering av studiedesign, disiplin og tilnærming

 join_Nor_form |> 
  select(open_data, year) |>
  drop_na() |>
  mutate(od2 = if_else(open_data == "No", 0, 1)) |>
  group_by(year) |>
  summarize(od_score = mean(od2)) |>
  ungroup() |> 
  ggplot (aes(x = year, y = od_score)) +
  geom_point(col="darkorange") + 
  ylab("Proportion of papers with open data") +
  ylim(0,1) 
 
 join_Nor_form |> 
   select(open_code, year) |>
   drop_na() |>
   mutate(od2 = if_else(open_code == "No", 0, 1)) |>
   group_by(year) |>
   summarize(od_score = mean(od2)) |>
   ungroup() |> 
   ggplot (aes(x = year, y = od_score)) +
   geom_point(col="darkorange") + 
   ylab("Proportion of papers with open code") +
   ylim(0,1) 

## Empiriske data vs andre tilnærminger

join_Nor_form |> 
  select(study_id ,study_title, research_type) |> 
  group_by(research_type) |>
  tally() |> 
  drop_na() |> 
  top_n(5) |>
  ggplot(aes(reorder(research_type, n),n,fill=research_type))+
  geom_histogram(stat="identity")+
  coord_flip()+
  theme(legend.position = "Null")+
  labs(x="", y="antall publikasjoner") +
  scale_fill_viridis(discrete = TRUE, option = "D")

## eksperimenter vs observajoner

join_Nor_form |> 
  filter(grepl("observational",tolower(research_type))) |> 
  select(study_id ,study_title,study_design_observational_studies ) |> 
  group_by(study_design_observational_studies) |> 
  tally() |> 
  drop_na() |> 
  ggplot(aes(reorder(study_design_observational_studies,n),n,fill=study_design_observational_studies))+
  geom_histogram(stat="identity")+
  coord_flip()+
  theme(legend.position = "Null")+
  labs(x="", y="antall publikasjoner") +
  scale_fill_viridis(discrete = TRUE, option = "D")

## Disipliner

join_Nor_form |> 
  mutate(study_area_discipline = fct_recode(study_area_discipline, Economy = "economic")) |>
  group_by(study_area_discipline) |> 
  tally() |> 
  drop_na() |> 
  ggplot(aes(reorder(study_area_discipline, n),n,fill=study_area_discipline))+
  geom_histogram(stat="identity")+
  coord_flip()+
  theme(legend.position = "Null")+
  labs(x="", y="antall publikasjoner") +
  scale_fill_viridis(discrete = TRUE, option = "D")


## Romlig skala
join_Nor_form |> 
  group_by(spatial_scale) |>
  tally() |> 
  drop_na() |> 
  ggplot(aes(reorder(spatial_scale, n),n,fill=spatial_scale))+
  geom_histogram(stat="identity")+
  coord_flip()+
  theme(legend.position = "Null")+
  labs(x="", y="antall publikasjoner") +
  scale_fill_viridis(discrete = TRUE, option = "D")


## Open Science
od<-join_Nor_form |> 
  select(open_data) |> 
  group_by(open_data) |> 
  tally()
oc<-join_Nor_form |> 
  select(open_code) |> 
  group_by(open_code) |> 
  tally()
pr<-join_Nor_form |> 
  select(preregistration_or_published_protocol) |> 
  group_by(preregistration_or_published_protocol) |> 
  tally()

names(od)<-c("Response", "n")
names(oc)<-names(pr)<-names(od)
od$question<-"open data"
oc$question<-"open code"
pr$question<-"Preregistration"


all_open<-rbind(od,oc,pr)

all_open |> 
  ggplot(aes(Response, n, fill = question))+
  geom_histogram(stat="identity", position = "dodge")+
  labs(x="", y="Antall publikasjoner") +
  scale_fill_viridis(discrete = TRUE, option = "D")

################################################################################
################################################################################
## Arealbruk, arealbruksendringer og økosystemer

## Region

join_Nor_form |> 
  group_by(county_name_of_norwegian_county) |>
  mutate(county_name_of_norwegian_county_main = strsplit(county_name_of_norwegian_county, ","))  |> 
  unnest(county_name_of_norwegian_county_main)  |> 
  group_by(county_name_of_norwegian_county_main) |> 
  mutate(county_name_of_norwegian_county_main=trimws(county_name_of_norwegian_county_main)) |> 
  tally() |>
  mutate(county_name_of_norwegian_county_main = fct_recode(county_name_of_norwegian_county_main, 
                                                Innlandet = "Hedmark", Innlandet = "Oppland")) |> 
  group_by(county_name_of_norwegian_county_main) |>
  summarize(n = sum(n)) |> 
  drop_na() |>
  filter(county_name_of_norwegian_county_main != "Whole country" & county_name_of_norwegian_county_main != "west Norway") |>
  ggplot(aes(reorder(county_name_of_norwegian_county_main, n),n,fill=county_name_of_norwegian_county_main))+
  geom_histogram(stat="identity")+
  coord_flip()+
  theme(legend.position = "Null")+
  labs(x="", y="antall publikasjoner") +
  scale_fill_viridis(discrete = TRUE, option = "D")  



### Økosystemer

join_Nor_form |> 
  group_by(ecosystem_type_main) |>
  mutate(ecosystems_main = strsplit(ecosystem_type_main, ","))  |> 
  unnest(ecosystems_main)  |> 
  group_by(ecosystems_main) |> 
  mutate(ecosystems_main=trimws(ecosystems_main)) |> 
  tally() |> 
  drop_na() |>
  top_n(10) |> 
  ggplot(aes(reorder(ecosystems_main, n),n,fill=ecosystems_main))+
  geom_histogram(stat="identity")+
  coord_flip()+
  theme(legend.position = "Null")+
  labs(x="", y="antall publikasjoner") +
  scale_fill_viridis(discrete = TRUE, option = "D")


#### Arealbruksendringer

join_Nor_form |> 
  group_by(x23) |>
  mutate(land_use = strsplit(x23, ","))  |> 
  unnest(land_use)  |> 
  group_by(land_use) |> 
  mutate(land_use=trimws(land_use)) |> 
  tally() |> 
  filter(land_use != "fishing collection") |>
  mutate(land_use = fct_recode(land_use, "Biological resource use: hunting, fishing, collection" = "Biological resource use: hunting")) |>
  drop_na() |> 
  top_n(20) |> 
  ggplot(aes(reorder(land_use, n),n,fill=land_use))+
  geom_histogram(stat="identity")+
  coord_flip()+
  theme(legend.position = "Null")+
  labs(x="", y="antall publikasjoner") +
  scale_fill_viridis(discrete = TRUE, option = "D")


join_Nor_form |> 
  group_by(x23) |>
  mutate(land_use = strsplit(x23, ","))  |> 
  unnest(land_use)  |> 
  group_by(land_use) |> 
  tally() |> 
  drop_na() |> 
  mutate(land_use=case_when(
    grepl("Agricul", land_use)~"Agriculture",
    grepl("Biological resource use", land_use)~"Biological resource use",
    grepl("Energy", land_use)~"Energy",
    grepl("Resident", land_use)~"Residential",
    grepl("Transport", land_use)~"Transportation",
    grepl("fish", land_use)~"Biological resource use",
    TRUE~land_use
  )) |> 
  mutate(land_use=trimws(land_use)) |> 
  group_by(land_use) |> 
  summarise(n=sum(n))|> 
  ggplot(aes(reorder(land_use, n),n,fill=land_use))+
  geom_histogram(stat="identity")+
  coord_flip()+
  theme(legend.position = "Null")+
  labs(x="", y="antall publikasjoner") +
  scale_fill_viridis(discrete = TRUE, option = "D")




################################################################################
################################################################################
## Karakterisering av biologisk mangfold og økosystemtjenester

## Biologisk organisering

'%!in%' <- function(x,y)!('%in%'(x,y))

join_Nor_form |> 
  group_by(taxonomic_level) |> 
  tally() |> 
  drop_na() |>
  mutate(taxonomic_level=case_when(
    taxonomic_level %!in% c("Class", "Family","Genus", "Kingdom", "Order", "Species")~"Other",
    TRUE~taxonomic_level
  )) |> 
  group_by(taxonomic_level) |> 
  summarise(n=sum(n)) |> 
  ggplot(aes(reorder(taxonomic_level, n),n,fill=taxonomic_level))+
  geom_histogram(stat="identity")+
  coord_flip()+
  labs(x="", y="antall publikasjoner")+
  theme(legend.position = "Null") +
  scale_fill_viridis(discrete = TRUE, option = "D")


### Arter; 

join_Nor_form |> 
  filter(taxonomic_level == "Species") |>
  group_by(taxonomic_name) |>
  mutate(taxonomic_name2 = strsplit(taxonomic_name, ","))  |> 
  unnest(taxonomic_name2)  |> 
  group_by(taxonomic_name2) |> 
  mutate(taxonomic_name2=trimws(taxonomic_name2)) |> 
  tally() |> 
  drop_na() |> 
  top_n(10) |>
  ggplot(aes(reorder(taxonomic_name2, n),n,fill=taxonomic_name2))+
  geom_histogram(stat="identity")+
  coord_flip()+
  theme(legend.position = "Null")+
  labs(x="", y="antall publikasjoner") +
  scale_fill_viridis(discrete = TRUE, option = "D")

## EBV


EBVs<-c("genetic_composition", "species_populations", "species_traits","community_composition", "ecosystem_functioning", "ecosystem_structure", "ecosystem_services")

plotEBVs<-function(var){
  join_Nor_form |> 
    select({{var}}) |>
    mutate(var = strsplit({{var}}, ","))  |> 
    unnest(var)  |> 
    mutate(var = trimws(var))  |> 
    group_by(var) |> 
    tally() |> 
    drop_na() |> 
    ggplot(aes(reorder(var, n),n,fill=var))+
    geom_histogram(stat="identity")+
    coord_flip()+
    theme(legend.position = "Null")+
    labs(x="", y="antall publikasjoner") +
    scale_fill_viridis(discrete = TRUE, option = "D")
}

plotEBVs(genetic_composition)
plotEBVs(species_populations)
plotEBVs(species_traits)
plotEBVs(community_composition)
plotEBVs(ecosystem_functioning)
plotEBVs(ecosystem_structure)
plotEBVs(ecosystem_services)

################################################################################
################################################################################
## Samfunnsvitenskap

### Analytiske tilnærminger

join_Nor_form |> 
  filter(does_the_study_assess_conflicts_tools_or_governance_relating_to_land_use_and_cover_change_its_effects_on_biodiversity_ecosystem_services_and_or_carbon_sequestration_and_storage_by_ecosystems_in_fenno_scandinavia_the_relation_should_be_explicit_in_study_for_the_record_to_be_included== "Yes") |> 
  select(data_gathering_methods) |>
  mutate(data_gathering_methods = strsplit(data_gathering_methods, ","))  |> 
  unnest(data_gathering_methods)  |> 
  group_by(data_gathering_methods) |> 
  tally() |> 
  drop_na() |> 
  ggplot(aes(reorder(data_gathering_methods, n),n,fill=data_gathering_methods))+
  geom_histogram(stat="identity")+
  coord_flip()+
  theme(legend.position = "Null")+
  labs(x="", y="antall publikasjoner") +
  scale_fill_viridis(discrete = TRUE, option = "D")

### Aktører 

join_Nor_form |> 
  filter(does_the_study_assess_conflicts_tools_or_governance_relating_to_land_use_and_cover_change_its_effects_on_biodiversity_ecosystem_services_and_or_carbon_sequestration_and_storage_by_ecosystems_in_fenno_scandinavia_the_relation_should_be_explicit_in_study_for_the_record_to_be_included== "Yes") |> 
  select(actors) |>
  mutate(actors = strsplit(actors, ","))  |> 
  unnest(actors)  |> 
  mutate(actors=trimws(actors)) |> 
  group_by(actors) |> 
  tally() |> 
  drop_na() |>
  ggplot(aes(reorder(actors, n),n,fill=actors))+
  geom_histogram(stat="identity")+
  coord_flip()+
  theme(legend.position = "Null")+
  labs(x="", y="antall publikasjoner") +
  scale_fill_viridis(discrete = TRUE, option = "D")

### Sektor

join_Nor_form |> 
  filter(does_the_study_assess_conflicts_tools_or_governance_relating_to_land_use_and_cover_change_its_effects_on_biodiversity_ecosystem_services_and_or_carbon_sequestration_and_storage_by_ecosystems_in_fenno_scandinavia_the_relation_should_be_explicit_in_study_for_the_record_to_be_included== "Yes") |> 
  select(sector_societal_concern) |>
  mutate(sector_societal_concern = strsplit(sector_societal_concern, ","))  |> 
  unnest(sector_societal_concern)  |> 
  mutate(sector_societal_concern=trimws(sector_societal_concern)) |> 
  group_by(sector_societal_concern) |> 
  tally() |> 
  drop_na() |>
  ggplot(aes(reorder(sector_societal_concern, n),n,fill=sector_societal_concern))+
  geom_histogram(stat="identity")+
  coord_flip()+
  theme(legend.position = "Null")+
  labs(x="", y="antall publikasjoner") +
  scale_fill_viridis(discrete = TRUE, option = "D")

### Tools

join_Nor_form |> 
  filter(does_the_study_assess_conflicts_tools_or_governance_relating_to_land_use_and_cover_change_its_effects_on_biodiversity_ecosystem_services_and_or_carbon_sequestration_and_storage_by_ecosystems_in_fenno_scandinavia_the_relation_should_be_explicit_in_study_for_the_record_to_be_included== "Yes") |> 
  select(governce_tools) |>
  # mutate(governce_tools = strsplit(governce_tools, ","))  |> 
  # unnest(governce_tools)  |> 
  # mutate(governce_tools=trimws(governce_tools)) |> 
  group_by(governce_tools) |> 
  tally() |> 
  drop_na() |>
  ggplot(aes(reorder(governce_tools, n),n,fill=governce_tools)) +
  geom_histogram(stat="identity")+
  coord_flip()+
  theme(legend.position = "Null")+
  labs(x="", y="antall publikasjoner") +
  scale_fill_viridis(discrete = TRUE, option = "D") 

### Ecosystem - social science papers; 

join_Nor_form |> 
  filter(does_the_study_assess_conflicts_tools_or_governance_relating_to_land_use_and_cover_change_its_effects_on_biodiversity_ecosystem_services_and_or_carbon_sequestration_and_storage_by_ecosystems_in_fenno_scandinavia_the_relation_should_be_explicit_in_study_for_the_record_to_be_included== "Yes") |> 
  group_by(ecosystem_type_main) |>
  mutate(ecosystems_main = strsplit(ecosystem_type_main, ","))  |> 
  unnest(ecosystems_main)  |> 
  group_by(ecosystems_main) |> 
  mutate(ecosystems_main=trimws(ecosystems_main)) |> 
  tally() |> 
  drop_na() |>
  top_n(10) |> 
  ggplot(aes(reorder(ecosystems_main, n),n,fill=ecosystems_main))+
  geom_histogram(stat="identity")+
  coord_flip()+
  theme(legend.position = "Null")+
  labs(x="", y="antall publikasjoner") +
  scale_fill_viridis(discrete = TRUE, option = "D")


### Aralbruk - social science

join_Nor_form |> 
  filter(does_the_study_assess_conflicts_tools_or_governance_relating_to_land_use_and_cover_change_its_effects_on_biodiversity_ecosystem_services_and_or_carbon_sequestration_and_storage_by_ecosystems_in_fenno_scandinavia_the_relation_should_be_explicit_in_study_for_the_record_to_be_included== "Yes") |> 
  group_by(x23) |>
  mutate(land_use = strsplit(x23, ","))  |> 
  unnest(land_use)  |> 
  group_by(land_use) |> 
  mutate(land_use=trimws(land_use)) |> 
  tally() |> 
  filter(land_use != "fishing collection") |>
  mutate(land_use = fct_recode(land_use, "Biological resource use: hunting, fishing, collection" = "Biological resource use: hunting")) |>
  drop_na() |> 
  top_n(20) |> 
  ggplot(aes(reorder(land_use, n),n,fill=land_use))+
  geom_histogram(stat="identity")+
  coord_flip()+
  theme(legend.position = "Null")+
  labs(x="", y="antall publikasjoner") +
  scale_fill_viridis(discrete = TRUE, option = "D")


join_Nor_form |> 
  filter(does_the_study_assess_conflicts_tools_or_governance_relating_to_land_use_and_cover_change_its_effects_on_biodiversity_ecosystem_services_and_or_carbon_sequestration_and_storage_by_ecosystems_in_fenno_scandinavia_the_relation_should_be_explicit_in_study_for_the_record_to_be_included== "Yes") |> 
  group_by(x23) |>
  mutate(land_use = strsplit(x23, ","))  |> 
  unnest(land_use)  |> 
  group_by(land_use) |> 
  tally() |> 
  drop_na() |> 
  mutate(land_use=case_when(
    grepl("Agricul", land_use)~"Agriculture",
    grepl("Biological resource use", land_use)~"Biological resource use",
    grepl("Energy", land_use)~"Energy",
    grepl("Resident", land_use)~"Residential",
    grepl("Transport", land_use)~"Transportation",
    grepl("fish", land_use)~"Biological resource use",
    TRUE~land_use
  )) |> 
  mutate(land_use=trimws(land_use)) |> 
  group_by(land_use) |> 
  summarise(n=sum(n))|> 
  ggplot(aes(reorder(land_use, n),n,fill=land_use))+
  geom_histogram(stat="identity")+
  coord_flip()+
  theme(legend.position = "Null")+
  labs(x="", y="antall publikasjoner") +
  scale_fill_viridis(discrete = TRUE, option = "D")



################################################################################
################################################################################

### Sys rev ecosystem

sysRev |> 
  group_by(ecosystem_type_main) |>
  mutate(ecosystems_main = strsplit(ecosystem_type_main, ","))  |> 
  unnest(ecosystems_main)  |> 
  group_by(ecosystems_main) |> 
  mutate(ecosystems_main=trimws(ecosystems_main)) |> 
  tally() |> 
  drop_na() |>
  top_n(10) |> 
  ggplot(aes(reorder(ecosystems_main, n),n,fill=ecosystems_main))+
  geom_histogram(stat="identity")+
  coord_flip()+
  theme(legend.position = "Null")+
  labs(x="", y="antall publikasjoner") +
  scale_fill_viridis(discrete = TRUE, option = "D")

### Sys rev area use

sysRev |> 
  group_by(x23) |>
  mutate(land_use = strsplit(x23, ","))  |> 
  unnest(land_use)  |> 
  group_by(land_use) |> 
  tally() |> 
  drop_na() |> 
  mutate(land_use=case_when(
    grepl("Agricul", land_use)~"Agriculture",
    grepl("Biological resource use", land_use)~"Biological resource use",
    grepl("Energy", land_use)~"Energy",
    grepl("Resident", land_use)~"Residential",
    grepl("Transport", land_use)~"Transportation",
    grepl("fish", land_use)~"Biological resource use",
    TRUE~land_use
  )) |> 
  mutate(land_use=trimws(land_use)) |> 
  group_by(land_use) |> 
  summarise(n=sum(n))|> 
  ggplot(aes(reorder(land_use, n),n,fill=land_use))+
  geom_histogram(stat="identity")+
  coord_flip()+
  theme(legend.position = "Null")+
  labs(x="", y="antall publikasjoner") +
  scale_fill_viridis(discrete = TRUE, option = "D")

### small scale

sysRev |> 
  group_by(x23) |>
  mutate(land_use = strsplit(x23, ","))  |> 
  unnest(land_use)  |> 
  group_by(land_use) |> 
  mutate(land_use=trimws(land_use)) |> 
  tally() |> 
  filter(land_use != "fishing collection") |>
  mutate(land_use = fct_recode(land_use, "Biological resource use: hunting, fishing, collection" = "Biological resource use: hunting")) |>
  drop_na() |> 
  top_n(15) |> 
  ggplot(aes(reorder(land_use, n),n,fill=land_use))+
  geom_histogram(stat="identity")+
  coord_flip()+
  theme(legend.position = "Null")+
  labs(x="", y="antall publikasjoner") +
  scale_fill_viridis(discrete = TRUE, option = "D")


#################################################################################
#################################################################################

## Only social science

test <- join_Nor_form_soc |> filter(does_the_study_assess_land_use_or_land_use_change_effects_on_the_biodiversity_ecosystem_services_functions_or_carbon_sequestration_and_storage_in_terrestrial_aquatic_or_coastal_areas_are_include_i_e_studies_focusing_on_marine_systems_are_excluded == "No")

write_delim(test, "QR/data/social_science.csv", delim=";")
write_delim(join_Nor_form_soc, "QR/data/social_science_all.csv", delim=";")






