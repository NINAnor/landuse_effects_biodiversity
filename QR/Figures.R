
### Overordnet karakterisering av studiedesign, disiplin og tilnærming

## Empiriske data vs andre tilnærminger

join_Nor_form |> 
  select(study_id ,study_title, research_type) |> 
  group_by(research_type) |>
  tally() |> 
  drop_na() |> 
  ggplot(aes(reorder(research_type, n),n,fill=research_type))+
  geom_histogram(stat="identity")+
  coord_flip()+
  theme(legend.position = "Null")+
  labs(x="", y="antall publikasjoner")

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
  labs(x="", y="antall publikasjoner")

## Disipliner

join_Nor_form |> 
  group_by(study_area_discipline) |> 
  tally() |> 
  drop_na() |> 
  ggplot(aes(reorder(study_area_discipline, n),n,fill=study_area_discipline))+
  geom_histogram(stat="identity")+
  coord_flip()+
  theme(legend.position = "Null")+
  labs(x="", y="antall publikasjoner")


## Romlig skala
join_Nor_form |> 
  group_by(spatial_scale) |>
  #mutate(ecosystems_main = strsplit(ecosystem_type_main, ","))  |> 
  #unnest(ecosystems_main)  |> 
  #group_by(ecosystems_main) |> 
  tally() |> 
  drop_na() |> 
  ggplot(aes(reorder(spatial_scale, n),n,fill=spatial_scale))+
  geom_histogram(stat="identity")+
  coord_flip()+
  theme(legend.position = "Null")+
  labs(x="", y="antall publikasjoner")


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
  labs(x="", y="Antall publikasjoner")

################################################################################
################################################################################
## Arealbruk, arealbruksendringer og økosystemer

## Hvor

my_list<-strsplit(join_Nor_form$county_name_of_norwegian_county, ",")

# Function to replace specific problematic characters
replace_problematic_chars <- function(x) {
  # Replacing specific problematic unicode sequence if visible in the string
  x <- gsub("\xc3\x98", "O", x, fixed = TRUE)  # Replacing 'Ø' which sometimes appears as '\xc3\x98' in UTF-8
  
  return(x)
}

# Apply this function to each element in the list
my_list <- lapply(my_list, function(x) {
  if (is.character(x)) {
    sapply(x, replace_problematic_chars, USE.NAMES = FALSE)
  } else {
    x
  }
})

# Flatten the list
flattened <- unlist(my_list)

# Remove NA and clean up
flattened <- na.omit(flattened)
flattened <- trimws(flattened)

# Create a dataframe
df <- data.frame(Name = flattened, stringsAsFactors = FALSE)

# Print the dataframe
#print(df)

df |> 
  group_by(Name) |> 
  tally() |> 
  ggplot(aes(reorder(Name, n),n)) + 
  geom_histogram(stat="identity", fill="blue")+
  coord_flip()+
  labs(x="", y="Antall publikasjoner")


#### Arealbruksendringer

p2<-join_Nor_form |> 
  group_by(x23) |>
  mutate(land_use = strsplit(x23, ","))  |> 
  unnest(land_use)  |> 
  group_by(land_use) |> 
  tally() |> 
  drop_na() |> 
  top_n(10) |> 
  ggplot(aes(reorder(land_use, n),n,fill=land_use))+
  geom_histogram(stat="identity")+
  coord_flip()+
  theme(legend.position = "Null")+
  labs(x="", y="antall publikasjoner")


p1<-join_Nor_form |> 
  group_by(x23) |>
  mutate(land_use = strsplit(x23, ","))  |> 
  unnest(land_use)  |> 
  group_by(land_use) |> 
  tally() |> 
  drop_na() |> 
  mutate(land_use=case_when(
    grepl("Agricul", land_use)~"Agriculture",
    grepl("Biological resource use", land_use)~"Biological",
    grepl("Energy", land_use)~"Energy",
    grepl("Resident", land_use)~"Residential",
    grepl("Transport", land_use)~"Transportation",
    grepl("fish", land_use)~"Biological",
    TRUE~land_use
  )) |> 
  mutate(land_use=trimws(land_use)) |> 
  group_by(land_use) |> 
  summarise(n=sum(n))|> 
  ggplot(aes(reorder(land_use, n),n,fill=land_use))+
  geom_histogram(stat="identity")+
  coord_flip()+
  theme(legend.position = "Null")+
  labs(x="", y="antall publikasjoner")

p1
p2

### Økosystemer

join_Nor_form |> 
  group_by(ecosystem_type_main) |>
  mutate(ecosystems_main = strsplit(ecosystem_type_main, ","))  |> 
  unnest(ecosystems_main)  |> 
  group_by(ecosystems_main) |> 
  tally() |> 
  drop_na() |> 
  ggplot(aes(reorder(ecosystems_main, n),n,fill=ecosystems_main))+
  geom_histogram(stat="identity")+
  coord_flip()+
  theme(legend.position = "Null")+
  labs(x="", y="antall publikasjoner")


################################################################################
################################################################################









