###---------------------------------------------------------------------------###
###                TRANSPATH DATABASE INDICATOR HARMONIZATION                 ###
###                Author: Christian Neumann                                  ###
###                Date: 31st May 2024                                        ###
###---------------------------------------------------------------------------###

###
#1.) Create environment####
###

#load libraries
library(readxl) #read xlsx data
library(tidyverse) #reads e.g. ggplot, dplyr, tidyr, stringr, purrr, a.o. 
library(RColorBrewer) #create color scales 
library(wordcloud) #wordcloud plots (if needed)
library(tm) #needed for text mining analysis

#load needed functions (developed to work with database)
source("Functions/Coding_functions_v03.R")

#define output/input paths
datapath <- paste(getwd(), "/Input/", sep = "")

#load data 
WP3_Subset <- readRDS(paste(datapath, "10_06_25_Final_Data_Perc_Ch.Rds", sep = ""))

#load reference lookup table, to define reference scenarios 
ref_scen_lookup <- read_xlsx(paste(datapath, "Ref_scenarios_lookupv02.xlsx", sep = ""))
ref_scen_lookup <- ref_scen_lookup[, c(2:3, 6:7)]

WP3_Subset <- WP3_Subset %>% 
  left_join(., ref_scen_lookup) 

###
#2.) Prepare data/calculate z-scores####
###

#first, subset data: 
data <- WP3_Subset %>%
  select(Study_nr., Index, Scenario_type, Reference_index, Reference_scenario, Model, Biodiversity_model, Intervention_agg_lvl1, starts_with("SDG13"), starts_with("SDG15"), -SDG15_Deforestation, -`SDG13_Carbon_price_(US$/t CO2)`, -`SDG15_Nitrogen_fixation_(Mt N/yr)`, -ends_with("Impact"), -ends_with("horizon"), -ends_with("raw_data"), -ends_with("steps"), -ends_with("decades")) 

#invert biodiversity indicators where an increase is not an benefit for consistency with other indicators!
data$`SDG15_Species_affected_by_50%_range_loss_(%)` <- data$`SDG15_Species_affected_by_50%_range_loss_(%)`*-1
data$`SDG15_Loss_suitable_habitat_(%)` <- data$`SDG15_Loss_suitable_habitat_(%)`*-1
data$`SDG15_Biodiversity_hotspot_loss_(%)` <- data$`SDG15_Biodiversity_hotspot_loss_(%)`*-1
data$`SDG15_Reduction_vascular_plant_species_(%)` <- data$`SDG15_Reduction_vascular_plant_species_(%)`*-1
data$SDG15_Extinction_per_million_species_years <- data$SDG15_Extinction_per_million_species_years*-1
data$`SDG15_Potentially_disappeared_fraction_of_species_(PDF)` <- data$`SDG15_Potentially_disappeared_fraction_of_species_(PDF)`*-1

#create interventions vector
interventions <- Frequency(data$Intervention_agg_lvl1)
interventions <- as.vector(interventions$Overview)

#add up all interventions as separate column
for (i in interventions) {
  data[[i]] <- NA  
}

#fill columns if intervention was applied = 1 in column, if it was not applied in scenario = 0
for (name in interventions) {
  data[[name]] <- ifelse(grepl(name, data$Intervention_agg_lvl1), 1, 0)
}

#data$SDG15_Deforestation_Percentage_Change <- data$SDG15_Deforestation_Percentage_Change*-1
#bring data in longer data structure, and extract indicators information
data <- pivot_longer(data, cols = c(starts_with("SDG13"), starts_with("SDG15")), names_to = "Indicator", values_to = "Indicator_Value")

#z-score transform each indicator 
data <- data %>%
  group_by(Indicator) %>%
  mutate(Indicator_Value = (Indicator_Value - mean(Indicator_Value, na.rm = TRUE)) / 
           sd(Indicator_Value, na.rm = TRUE)) %>%
  ungroup()

###
#3.) Calculate absolute change from reference scenarios:####
###

#filter for reference scenarios, exclude index == 38 because we only use 1 of two reference scenarios defined for IPCC IMP marker scenarios defined in climate change mitigation report 2023
data_ref <- subset(data, data$Scenario_type == "reference" & !is.na(data$Indicator_Value) & data$Index !=38)
data <- subset(data, data$Scenario_type == "policy-screening" | data$Scenario_type == "target-seeking")
data <- data %>% filter(., Reference_index != "None" & !is.na(Reference_index))

#create key columns in reference and normal df, for matching results 
data_ref$key <- paste(data_ref$Index, data_ref$Indicator, sep = "_")
data$key <- paste(data$Reference_index, data$Indicator, sep = "_")

#create reference values vector, containing information on key
ref_values <- setNames(data_ref$Indicator_Value, data_ref$key)

#use ifelse condition to substract reference value from scenario value using key column and names of reference vector (we use this approach here, because we have double entries for each key combination (because of the interventions), join or match operations would retrieve errors/wrong combinations because of double entries)
data$Indicator_Value <- ifelse(data$key %in% names(ref_values),
                               data$Indicator_Value - ref_values[data$key],
                               NA)

###
#4.) Final data output####
###

#omit NAs 
data <- na.omit(data)

biodiv_cols <- unique(data$Indicator[startsWith(data$Indicator, "SDG15")])
cc_cols <- unique(data$Indicator[startsWith(data$Indicator, "SDG13")])

data_final <- data %>%
  select(-key) %>%
  pivot_wider(names_from = Indicator, values_from = Indicator_Value) %>%
  select(Study_nr., Index, Scenario_type, Reference_index, Reference_scenario, Model, Biodiversity_model, Intervention_agg_lvl1, all_of(interventions)[colSums(select(., all_of(interventions))) > 5], all_of(cc_cols), all_of(biodiv_cols))

saveRDS(data_final, file = paste(datapath, "10_06_25_Reference_harmonized_data.rds"))

