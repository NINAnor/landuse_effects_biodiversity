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
saveRDS(join_Nor_form,"QR/data/cache_data/join_nor_form.RDS")

join_Nor_form<-readRDS(here::here("QR/data/cache_data/join_Nor_form.RDS"))
nor_bib <- readRDS(here::here("QR/data/cache_data/nor_bib.RDS"))


join_Nor_form[grep("Hydro", join_Nor_form$abstract),]

join_Nor_form[140,]$author<- "Junker kohler, B. and Sundt, H."

which(join_Nor_form$study_id=="rayyan-674234078")
join_Nor_form[17,]$address<-"Norwegian Institute for Nature Research, Trondheim,Norway"

#join_Nor_form[grep("Riparian forest buffers have multiple benefits", join_Nor_form$abstract),]$study_id

#join_Nor_form[grep("Department of Finance, Accounting and Risk", join_Nor_form$address),]

#which(join_Nor_form$study_id=="rayyan-674233054")

#which(join_Nor_form$title=="Light and temperature controls of aquatic plant photosynthesis downstream of a hydropower plant and the effect of plant removal")

join_Nor_form[238,]$title<-"Recent forest on abandoned agricultural land in the boreonemoral zone Biodiversity of plants and fungi in relation to historical and present tree cover"
join_Nor_form[228,]$title<-"New old risks on the small farm: Iconic species rewilding in Europe"
join_Nor_form[215,]$title<-"How effective are European agrienvironment schemes in conserving and promoting biodiversity?"
join_Nor_form[140,]$title<-"Assessing visual preferences of the local public for environmental mitigation measures of hydropower impactsdoes point of view location make a difference?"

join_Nor_form[135,]$title<-"Best practiceIs natural revegetation sufficient to achieve mitigation goals in road construction?"

join_Nor_form[130,]$title<-"Habitat Protection Approaches Facilitate Conservation of Overlooked Fungal Diversity A Case Study From the Norwegian Coastal Heathland System"
join_Nor_form[69,]$title<-"A stand level scenario model for the Norwegian forestry a case study on forest management under climate change"

join_Nor_form[62,]$title<-"Choice of metrics matters Future scenarios on milk and beef production in Norway using an LCA approach"
        
join_Nor_form[111,]$address<-"[\"Norwegian Institute for Water Research (NIVA)\", \"Faculty of Environmental Sciences and Natural Resource Management, Norwegian University of Life Sciences, Norway\", \"Norwegian Research Centre, Bergen, 5008, Norway\", \"Department of Biological Sciences, University of Bergen, ThormC8hlensgate 53 A & B, Bergen, 5006, Norway\", \"Department Aquatic Ecosystem Analysis (ASAM), Helmholtz Centre for Environmental Research, Germany\""

join_Nor_form[228,]$address<-"[\"Department of Finance, Accounting and Risk, Glasgow School for Business and Society, Glasgow Caledonian University\", \"The James Hutton Institute, UK, United Kingdom\", \"Norwegian University of Science and Technology (NTNU), Norway\", \"Departamento de EconomC-a y Ciencias Sociales, Universitat PolitC(cnica de ValC(ncia (UPV), Spain\", \"Department of Agriculture, Food and Environment, University of Pisa (Pisa), Italy\"]" 


join_Nor_form[1,]$abstract<-"Worldwide semi-natural habitats of high biological value are in decline. Consequently, numerous Agri-Environment Schemes (AESs) intended to halt biodiversity loss within these habitats have been implemented. One approach has been the application of adaptive management, where scientific knowledge is applied alongside the traditional ecological knowledge (TEK) of stakeholders in order to establish an integrated approach that is adjusted as outcomes are assessed. In this paper we examine the effectiveness of the adaptive management approach of Norways Action Plan for Hay Meadows (APHM). Twenty-nine hay meadows from fourteen farms in the county of More og Romsdal were ecologically surveyed over a 2 year period. Interviews were also conducted with owners and land managers to explore TEK and management issues. The interdisciplinary study found that the disembedding of hay meadow management from its initial commercial purpose (in particular the loss of much of the livestock from the region) has contributed to a significant loss of TEK  which is now largely limited to knowledge of how the fields were managed recently. While, the APHM is limiting biodiversity decline by promoting traditional practices there were indications that the standardisation of management actions might negatively affect species composition in the long term. More critically, continued farm abandonment within the region means that without alternatives to management by farmers many of these meadows are likely to disappear in the next couple of decades. We conclude that adaptive management provides an effective short-term means of preserving hay meadows, but long term conservation will require a means of addressing the continued decline of local farming communities."

join_Nor_form[3,]$abstract<-"Riparian forest buffers have multiple benefits for biodiversity and ecosystem services in both freshwater and terrestrial habitats but are rarely implemented in water ecosystem management, partly reflecting the lack of information on the effectiveness of this measure. In this context, social learning is valuable to inform stakeholders of the efficacy of riparian vegetation in mitigating stream degradation. We aim to develop a Bayesian belief network (BBN) model for application as a learning tool to simulate and assess the reach- and segment-scale effects of riparian vegetation properties and land use on instream invertebrates. We surveyed reach-scale riparian conditions, extracted segment-scale riparian and subcatchment land use information from geographic information system data, and collected macroinvertebrate samples from four catchments in Europe (Belgium, Norway, Romania, and Sweden). We modelled the ecological condition based on the Average Score Per Taxon (ASPT) index, a macroinvertebrate-based index widely used in European bioassessment, as a function of different riparian variables using the BBN modelling approach. The results of the model simulations provided insights into the usefulness of riparian vegetation attributes in enhancing the ecological condition, with reach-scale riparian vegetation quality associated with the strongest improvements in ecological status. Specifically, reach-scale buffer vegetation of score 3 (i.e. moderate quality) generally results in the highest probability of a good ASPT score (99 to 100%). In contrast, a site with a narrow width of riparian trees and a small area of trees with reach-scale buffer vegetation of score 1 (i.e. low quality) predicts a high probability of a bad ASPT score (74%). The strengths of the BBN model are the ease of interpretation, fast simulation, ability to explicitly indicate uncertainty in model outcomes, and interactivity. These merits point to the potential use of the BBN model in workshop activities to stimulate key learning processes that help inform the management of riparian zones."

join_Nor_form[13,]$abstract<-"Growth, density and production of juvenile Atlantic salmon and brown trout were studied in three different sections of the Kvassheimsåna River in south-western Norway from 1979 to 1983. Section 1. in the upper part of the river, is located above a waterfall impassable for migratory salmonids and is surrounded by grazing land. Sections 2 and 3, in the middle and lower parts of the river, are influenced by agricultural activity. Total nitrogen concentration varied between 250 and 1000 μg l 1 in section 1 and 1500 and 2500 μg l1 in sections 2 and 3. Total phosphorus (Tot-P) concentrations also increased with decreasing altitude: 1946 μg l1 in section I and 31101 μg l 1 in sections 2 and 3.

The number of 0 + salmon in sections 2 and 3 varied between 30.1 and 167.8 specimens 100 m 2, with means 90.2 and 95.2 specimens 100 m 2:, respectively; the density of 1 + salmon, with mean values of 16.3 and 51.0 specimens 100m2 was significantly correlated with the original fry density. The growth rate of 0+ salmon was not inversely related to cohort density, but was significantly so for 1 + salmon. Mean annual salmon production in section 2 was 1595 g 100 m2 year 1, and in section 3 was 841 g 100m2 year 1. A logarithmic function gave the best curve fit between salmon production and mean annual biomass. Thus, production levelled off for the highest values recorded in section 2, and perhaps approached the carrying capacity of the stream. A multiple regression analysis showed that yearly variation in 1 + salmon density was the single factor accounting for most of the total variability in production (60%). Variation in water temperature and nutrient content were not significantly related to variation in fish production.

Densities of brown trout were low in all sections (<20 specimens 100m 2). Fry density was highest in section 3 and parr density in section 1. All age groups of sympatric brown trout grew significantly faster in sections 2 and 3 compared with allopatric brown trout in section 1."

join_Nor_form[16,]$abstract<-"Dense beds of aquatic plants are often perceived as nuisance and therefore mechanically removed, often at substantial cost. Such removal, however, may affect a range of ecosystem functions and consequently also the ecosystem services that benefit society.
We studied five cases: River Otra (Norway), River Spree (Germany), Lake Kemnade (Germany), Lake Grand-Lieu (France) and Hartbeespoort Dam (South Africa). In all, nuisance aquatic plant growth is managed, but dominant species, geographic setting and major societal uses are different. We quantified 12 final ecosystem services as flows per area and year in biophysical and monetary terms. Quantified services were food and fodder production, commercial fisheries, hunting and gathering wild products, hydropower production, drinking and irrigation water production, flood prevention, carbon sequestration, active and passive recreation and biodiversity conservation (nonuse).
These services were related to aquatic plant cover via a range of ecosystem functions, and the effects were estimated of three plant removal regimes on the relative importance of the quantified ecosystem services and on the total sum of the monetary estimates (total economic value, TEV). The three removal regimes were maximum removal, current practice and do nothing.
In all five cases, TEV was dominated by different forms of recreation. TEV was highest for Lake Kemnade, where visitor densities were highest. TEV was most sensitive to the different management regimes in Lake Kemnade, because a threshold in aesthetic appreciation was passed in the ‘do-nothing’ regime, and in Hartbeespoort Dam, because of the effect on boating and angling. In the other cases, the different removal regimes had little effect on the estimated TEV.
Synthesis and applications. Since recreation dominated the estimated societal benefits in the studied ecosystems, also where provision of hydropower, drinking water or irrigation water were relevant, effects on recreation should be a core consideration in the management of nuisance aquatic plants. Furthermore, aquatic plant management strategies will benefit from taking into account the differences in perceived nuisance among different categories of recreative users before engaging in costly removal."


join_Nor_form[17,]$abstract<-"The EU Water Framework Directive (WFD) aims to achieve good status of aquatic habitats. Classification of ecological status and identifying the stressors impacting aquatic habitats is essential for achieving this aim. Here, we evaluate different methods and indices for assessing ecological status in a hydropower-regulated river in central Norway using kick-sampling and macroinvertebrate identification through morphology and DNA as well as using environmental DNA. In Norway, the ASPT index (Average Score Per Taxon) is commonly used to evaluate ecological condition in general, although the index only provides evidence for organic pollution. We observed lower than expected diversity in the regulated river, but this was not reflected in the ASPT index, which showed Good to High status for all samples and methods. An alternative index, the IBIBI (Intercalibrated Benthic Invertebrate Biodiversity Index), returned Bad to Moderate status using the same data. The DNA-based identification methods returned in general higher species richness and somewhat higher index values than morphological species identification did. Our study exemplifies the importance of including relevant biological quality indices in WFD compliant assessments, and we advocate inclusion of a pressure-independent index like IBIBI in Norwegian river management and DNA-based identification methods for future river management in general."
join_Nor_form[25,]$abstract<-"The ongoing loss of red-listed coastal heathlands is a threat to biodiversity and cultural heritage legacies throughout the Atlantic coastal regions of Europe. It is possible to restore degraded and afforested heathlands, but restoration interventions are often labour-intensive and costly, and the outcome of specific restoration actions are not well documented. We assess the efficiency of restoring coastal heathlands through natural succession (i.e. ‘passive restoration') after removal of Sitka spruce Picea sitchensis (Bong.) Carr. plantations. The study was replicated on two neighbouring islands in a nature reserve in Western Norway. Low-intensity free-range sheep grazing was implemented as part of the reserve management plan. Furthermore, we tested the effect of leaving the clear-felled woody material as chips on site, this being a cost-efficient strategy on islands. Succession was monitored 1, 2, 4/5 and 8 years after clear-felling, and revegetation of vascular plants and bryophytes was compared to target heathland vegetation. Surprisingly, we found different successional trajectories on the two islands. Species composition on one island approached target heathland vegetation during succession, but not on the other. Wood chips reduced species richness and slowed the restoration process, but these negative effects were only short-term (<8 years). Differences in seed bank composition and soil conditions due to land use may explain the deviating successional trajectories on the two islands. We also found that management actions beyond clear-felling and introducing sheep grazing are necessary due to the rapid seed regeneration of the Sitka spruce."

join_Nor_form[28,]$abstract<-"After centuries of intense persecution, several large carnivore species in Europe and North America have experienced a rebound. Today's spatial configuration of large carnivore populations has likely arisen from the interplay between their ecological traits and current environmental conditions, but also from their history of persecution and protection. Yet, due to the challenge of studying population-level phenomena, we are rarely able to disentangle and quantify the influence of past and present factors driving the distribution and density of these controversial species. Using spatial capture-recapture models and a data set of 742 genetically identified wolverines Gulo gulo collected over ½ million km2 across their entire range in Norway and Sweden, we identify landscape-level factors explaining the current population density of wolverines in the Scandinavian Peninsula. Distance from the relict range along the Swedish–Norwegian border, where the wolverine population survived a long history of persecution, remains a key determinant of wolverine density today. However, regional differences in management and environmental conditions also played an important role in shaping spatial patterns in present-day wolverine density. Specifically, we found evidence of slower recolonization in areas that had set lower wolverine population goals in terms of the desired number of annual reproductions. Management of transboundary large carnivore populations at biologically relevant scales may be inhibited by administrative fragmentation. Yet, as our study shows, population-level monitoring is an achievable prerequisite for a comprehensive understanding of the distribution and density of large carnivores across an increasingly anthropogenic landscape."

join_Nor_form[32,]$abstract<-"As demand for renewable energy is rising, wind power development is rapidly growing worldwide. In its wake, conflicts arise over land use changes converting pristine nature into industrial power plants and its associated adverse biodiversity effects, crowned by one of the most obvious and deadly consequences: bird collisions. Most post-construction studies report low levels of avian mortality, but the majority of these studies are conducted primarily on larger birds. However, the diversity and abundance of small passerine birds are rarely reflected in the carcass surveys, although they in numeric proportion to their abundances should be the most numerous. The assumption that surveys find all carcasses seems thus rarely fulfilled and passerine mortality is likely to be grossly underestimated. We therefore designed an experiment with dummy birds to estimate mortality of small-bodied passerines and other small-bodied birds during post-construction surveys, tested in a medium-sized wind farm in western Norway. The wind farm was surveyed weekly during the migration periods by carcass survey teams using trained dogs to find killed birds. The dogs in the carcass surveys were more successful in locating the large than the small dummy birds (60–200 g), where they found 74% of the large dummy birds. Detecting the smaller category (5–24 g) was more demanding and the dogs only found 17% of the small dummy birds. Correcting the post-construction carcass survey outcome with the results from the experiment leads to an almost fourfold increase in estimated mortality rates, largely due to the low detection rate of the smallest category. The detection rates will naturally vary between wind farms, depending on the specific habitat characteristics, the efficiency of the carcass surveys and the search intervals. Thus, implementing a simple experiment with dummy birds to future post-construction surveys will produce more accurate estimates of the wind turbine mortality rates, and thus improve our understanding of the biodiversity effects of conforming to a more sustainable future."
join_Nor_form[140,]$title

join_Nor_form[140,]$abstract<-"Hydropower is a highly appreciated climate-friendly source of energy production. However, it has non-negligible negative impacts on the environment and landscape aesthetics where the energy is produced, affecting the recreational interests of the public using the respective local river spaces. The preferences of the local public are increasingly assessed and involved in the planning of mitigation measures for impacted rivers. Aesthetic assessment methods using a common user perspective, i.e., an “on-the-ground” perspective, could potentially be improved by using an aerial perspective facilitated by modern drone technology. Studies on the compatibility of these two perspectives of assessment in terms of public preference elicitation are lacking so far. In river Nea, Norway, we conducted a quantitative analysis of the visual preferences of the local public for different environmental mitigation measures related to weirs, minimum flow, and recreational infrastructure using both perspectives. The results indicate that there exist significant differences in the preferences for scenarios based on the two different visual perspectives, and that a compatibility between them cannot be assumed and therefore requires further investigation. Finally, based on our study setup and previous experience, we outline and propose a standardized procedure for the visualization of mitigation measures as an input to environmental design projects where public perception is incorporated"




# join_Nor_form$abstract<-as.character(join_Nor_form$abstract)
# 
# Encoding(join_Nor_form$abstract)
# library(stringi)
# join_Nor_form$abstract<-stri_encode(join_Nor_form$abstract, "", "UTF-8")


#join_Nor_form[grep("\"", join_Nor_form$abstract),]$abstract


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

