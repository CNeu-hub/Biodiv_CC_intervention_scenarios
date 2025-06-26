###---------------------------------------------------------------------------###
###                Clustering based on jaccard similarity for random for. data###
###                Author: Christian Neumann                                  ###
###                Date: 31st May 2024                                        ###
###---------------------------------------------------------------------------###

###
#1.) Create environment####
###

#load libraries
library(tidyverse)
library(ggpubr)
library(patchwork)
library(igraph)

#load needed functions (developed to work with database)
source("Database_release_v01/Functions/Jaccard_similarity_v01.R")

#define input/output path 
datapath <- paste0(getwd(), "/Database_release_v01/Input/")
figpath <- paste(getwd(), sep = "/", "Database_release_v01/Output/Manuscript/Figures/")

###
#2.) Create input data####
###

data <- readRDS(paste(datapath, "10_06_25_Reference_harmonized_data.rds"))

#extract biodiv and cc cols for selecting data later on 
biodiv_cols <- colnames(data)[startsWith(colnames(data), "SDG15")]
biodiv_cols <- str_replace_all(biodiv_cols, " |-", "_")
cc_cols <- colnames(data)[startsWith(colnames(data), "SDG13")]
cc_cols <- str_replace_all(cc_cols, " |-", "_")
colnames(data) <- str_replace_all(colnames(data), " |-", "_")

#define interventions vector, and calculate number of jointly implemented interventions within each row (i.e., each row = each scenario)
interventions <- colnames(data[9:40])
data$Interventions_number <- rowSums(data[, interventions])

###
#3.) Cluster/prepare biodiversity dataset####
###

#select biodiversity data & exclude all interventions that are occuring in 5 or more scenarios, remove NAs
data_biodiv <- data %>%
  select(Study_nr., Index, Scenario_type, Reference_index, Reference_scenario, Model, Biodiversity_model, Intervention_agg_lvl1, all_of(interventions)[colSums(select(., all_of(interventions))) > 4], starts_with("SDG15"), Interventions_number) %>%
  filter(rowSums(!is.na(select(., all_of(biodiv_cols)))) > 0)

#now determine clusters for this dataset using the calculate_jaccard function 
jaccard <- calculate_jaccard(data_biodiv[interventions], similarity_threshold = 0.7, hclustering = FALSE, cluster_height = 0.3)
subset <- jaccard$Jaccard_subset

#create graph with Jaccard similarity as edge weights
g <- graph_from_data_frame(subset, directed = FALSE)

#create an pdf export of jaccard similarity of interventions and clusters 
pdf(paste(figpath, "Jaccard_clustering_biodiv_RF.pdf"), width = 18, height = 12)
jaccard$Plot
plot.igraph(g, main = "Louvain clusters of interventions with jaccard similarity >0.7 (n = 235 scenarios)")
dev.off()

#Louvain components clustering, finds maximal connected pairs of interventions (all pairs that are connected with each other)
clusters <- igraph::components(g)

#assign clusters to interventions
subset$class <- clusters$membership[match(subset$Intervention_a, names(clusters$membership))]

#extract cluster names (will be used later on)
cluster1 <- names(clusters$membership[clusters$membership==1])
cluster2 <- names(clusters$membership[clusters$membership==2])

#convert to long format for metrics data (that each quantified metric per scenario is treated as an observation, we handle this later with the weighting in the random forest models)
data_biodiv <- data_biodiv %>%
  pivot_longer(., starts_with("SDG15"), values_to = "Biodiversity_impact", names_to = "Biodiversity_indicator") %>%
  na.omit(.) %>%
  select(Index, Study_nr., Biodiversity_model, Model, all_of(interventions), Biodiversity_impact, Biodiversity_indicator, Interventions_number)

#create new cluster binary columns & exclude all interventions usually included in the clusters
data_biodiv_final <- data_biodiv %>%
  rowwise() %>%
  mutate(Cluster_1 = ifelse(any(c_across(all_of(cluster1)) == 1), 1, 0)) %>%
  mutate(Cluster_2 = ifelse(any(c_across(all_of(cluster2)) == 1), 1, 0)) %>%
  ungroup() %>%
  select(-all_of(cluster1), -all_of(cluster2)) %>%
  select(Index, Study_nr., Biodiversity_model, Model,, any_of(interventions)[colSums(select(., any_of(interventions))) > 4], Cluster_1, Cluster_2, Biodiversity_impact, Biodiversity_indicator, Interventions_number)

###
#4.) Cluster/prepare climate dataset####
###

#select climate data & exclude all interventions that are occuring in 5 or more scenarios, remove NAs
data_cc <- data %>%
  select(Study_nr., Index, Scenario_type, Reference_index, Reference_scenario, Model, Biodiversity_model, Intervention_agg_lvl1, all_of(interventions)[colSums(select(., all_of(interventions))) > 4], starts_with("SDG13"), Interventions_number) %>%
  filter(rowSums(!is.na(select(., all_of(cc_cols)))) > 0)

#now determine clusters for this dataset using the calculate_jaccard function 
jaccard <- calculate_jaccard(data_cc[interventions], similarity_threshold = 0.7, hclustering = FALSE, cluster_height = 0.3)
subset <- jaccard$Jaccard_subset

#create graph with Jaccard similarity as edge weights
g <- graph_from_data_frame(subset, directed = FALSE)

#create an pdf export of jaccard similarity of interventions and clusters 
pdf(paste(figpath, "Jaccard_clustering_cc_RF.pdf"), width = 18, height = 12)
jaccard$Plot
plot.igraph(g, main = "Louvain clusters of interventions with jaccard similarity >0.7 (n = 235 scenarios)")
dev.off()

#components clustering, finds maximal connected pairs of interventions (all pairs that are connected with each other)
clusters <- igraph::components(g)

#assign clusters to interventions
subset$class <- clusters$membership[match(subset$Intervention_a, names(clusters$membership))]

cluster1 <- names(clusters$membership[clusters$membership==1])
cluster3 <- names(clusters$membership[clusters$membership==2])

#convert to long format for metrics data (that each quantified metric per scenario is treated as an observation, we handle this later with the weighting in the random forest models)
data_cc <- data_cc %>%
  pivot_longer(., starts_with("SDG13"), values_to = "Climate_impact", names_to = "Climate_indicator") %>%
  na.omit(.) %>%
  select(Index, Study_nr., Model, all_of(interventions), Climate_impact, Climate_indicator, Interventions_number)

#create new cluster binary columns & exclude all interventions usually included in the clusters
data_cc_final <- data_cc %>%
  rowwise() %>%
  mutate(Cluster_1 = ifelse(any(c_across(all_of(cluster1)) == 1), 1, 0)) %>%
  mutate(Cluster_3 = ifelse(any(c_across(all_of(cluster3)) == 1), 1, 0)) %>%
  ungroup() %>%
  select(-all_of(cluster1), -all_of(cluster3)) %>%
  select(Index, Study_nr., Model,, any_of(interventions)[colSums(select(., any_of(interventions))) > 4], Cluster_1, Cluster_3, Climate_impact, Climate_indicator, Interventions_number)

#export output data for later input to random forest modelling
saveRDS(list(data_biodiv_final, data_cc_final), file = paste(datapath, "RF_input_data_10_06_25_weighted.rds"))


