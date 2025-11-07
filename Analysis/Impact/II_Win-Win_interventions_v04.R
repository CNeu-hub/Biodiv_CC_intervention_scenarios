###---------------------------------------------------------------------------###
###                Create win-win/lose-lose plot layout for interventions that###
###                were applied in scenarios that quantified cc and biodiv   ###
###                Author: Christian Neumann                                  ###
###                Date: 16th May 2024                                        ###
###---------------------------------------------------------------------------###

###
#1.) Create environment####
###

#load libraries
library(tidyverse)
library(RColorBrewer)
library(igraph)
library(patchwork)
library(gridExtra) #used for plot arrangement
library(gridtext) #used for adding another text layer below plot 
library(grid)

#load needed functions (developed to work with database)
source("Functions/Jaccard_similarity_v01.R")

#define output/input paths
figpath <- paste(getwd(), "/Output/Figures/", sep = "")
tablepath <- paste(getwd(), "/Output/Tables/", sep = "")
datapath <- paste(getwd(), "/Input/", sep = "")

data <- readRDS(paste(datapath, "10_06_25_Reference_harmonized_data.rds"))

###
#2.) Filter/Prepare data####
###

#define biodiversity and climate change indicator columns
biodiv_cols <- colnames(data)[startsWith(colnames(data), "SDG15")]
cc_cols <- colnames(data)[startsWith(colnames(data), "SDG13")]

#define interventions vector and calculate number of interventions applied in a scenario
interventions <- colnames(data[9:40])
data$Interventions_number <- rowSums(data[, interventions])

#filter rows where at least one biodiversity and one climate change indicator is not NA 
data <- data %>%
  filter(rowSums(!is.na(select(., all_of(biodiv_cols)))) > 0 & rowSums(!is.na(select(., all_of(cc_cols)))) > 0) %>%      
  select(Study_nr., Index, Scenario_type, Reference_index, Reference_scenario, Model, Biodiversity_model, Intervention_agg_lvl1, all_of(interventions)[colSums(select(., all_of(interventions))) > 4], starts_with("SDG15"), starts_with("SDG13"), Interventions_number) 

###
#3.) Clustering based on Jaccard similarity####
###

#now determine clusters for this dataset 
jaccard <- calculate_jaccard(data[9:31], similarity_threshold = 0.7, hclustering = FALSE, cluster_height = 0.3)
subset <- jaccard$Jaccard_subset

#create graph with Jaccard similarity as edge weights
g <- graph_from_data_frame(subset, directed = FALSE)

pdf(paste(figpath, "Supplementary_figure_S5.pdf"), width = 14, height = 10)
jaccard$Plot
plot.igraph(g, main = "Louvain clusters of interventions with jaccard similarity >0.7 (n = 190 scenarios)")
dev.off()

#components clustering, finds maximal connected pairs of interventions (all pairs that are connected with each other)
clusters <- igraph::components(g)

#assign clusters to interventions
subset$class <- clusters$membership[match(subset$Intervention_a, names(clusters$membership))]

group1 <- names(clusters$membership[clusters$membership == 1])
group2 <- names(clusters$membership[clusters$membership == 2])

###
#4.) Prepare and harmonize data for plotting####
###

data <- data %>%
  pivot_longer(., any_of(interventions), names_to = "Intervention", values_to = "Intervention_yes_no") %>%
  filter(., Intervention_yes_no == 1) %>%
  pivot_longer(., starts_with("SDG15"), values_to = "Biodiversity_impact", names_to = "Biodiversity_indicator") %>%
  pivot_longer(., starts_with("SDG13"), values_to = "Climate_impact", names_to = "Climate_indicator")# %>%

data <- na.omit(data)

data <- data %>%
  mutate(Intervention = case_when(Intervention %in% group1 ~ "Cluster 1",
                                  Intervention %in% group2 ~ "Cluster 2",
                                  TRUE ~ Intervention))

#harmonize data (calculate for each scenario a harmonized biodiversity/climate impact (average of z-score transformed values))
data <- data %>%
  group_by(Index) %>% 
  summarise(Study_nr = unique(Study_nr.),
            Intervention = unique(Intervention),
            Biodiversity_impact = mean(Biodiversity_impact),
            Climate_impact = mean(Climate_impact),
            Interventions_number = unique(Interventions_number)) %>%
  ungroup()

#color coding
data$color_category <- ifelse(data$Biodiversity_impact < 0, 
                              ifelse(data$Climate_impact > 0, "Lose-Lose", "Lose-Win"),
                              ifelse(data$Climate_impact < 0, "Win-Win", "Win-Lose"))

#calculate sd_deviation of climate and biodiveristy impact (used for calculating color categories)
sd_deviation <- sd(data$Climate_impact + data$Biodiversity_impact)

#every dot falling into 0.5*sd_deviation will be green (i.e. considered as a synergistic impact with balanced impacts on both, climate and biodiversity)
data$color_category2 <- ifelse(abs(data$Climate_impact + data$Biodiversity_impact) <= 0.5 * sd_deviation, "Balanced", 
                               ifelse(data$Climate_impact > - data$Biodiversity_impact, "Biodiversity positive", "Climate positive"))
data$color_category2 <- ifelse(abs(data$Climate_impact > 0) & abs(data$Biodiversity_impact < 0), "Lose-lose", data$color_category2)

#additional trade-offs group
data$color_category2 <- ifelse(abs(data$Climate_impact < 0) & abs(data$Biodiversity_impact < 0), "Trade-off", data$color_category2)
data$color_category2 <- ifelse(abs(data$Climate_impact > 0) & abs(data$Biodiversity_impact > 0), "Trade-off", data$color_category2)

data$size_category <- cut(data$Interventions_number, breaks = c(0, 3, 6, 9, 12, 14), labels = c("0-3", "3-6", "6-9", "9-12", ">12"))

#now calculate percentage to which interventions fall into categories biodiversity positive, climate postivie, synergistic
data <- data %>%
  group_by(Intervention) %>%
  mutate(Observations = n()) %>%
  group_by(Intervention, color_category2) %>%
  mutate(Proportions_color_cat2 = round((n()/Observations)*100, digits = 1)) %>%
  ungroup() %>%
  mutate(Intervention_labs = paste(Intervention, " (n = ", Observations, ")", sep = ""))

#write xlsx subset of scenarios as example table for supplementary material
write.csv(data[1:10, ], file = paste(tablepath, sep = "", "biodiv_cc_dataset_example.csv"), row.names = FALSE)

###
#5.) Final plot####
###

#optional: just exclude interventions with n < 15
data <- data %>% 
  filter(!(Intervention_labs %in% c("Cluster 2 (n = 11)", "Sustainable agricultural intensification (n = 5)"))) %>%
  mutate(Intervention_labs = case_when(Intervention_labs == "Cluster 1 (n = 96)" ~ "Energy and building decarbonization cluster (n = 96)", 
                                       TRUE ~ Intervention_labs))
a <- ggplot() +  
  geom_vline(xintercept = 0, color = "darkgrey", linetype = 4) +
  geom_hline(yintercept = 0, color = "darkgrey", linetype = 4) +
  geom_point(data = data, alpha = 0.5, aes (x = Biodiversity_impact, y = Climate_impact, color = color_category2, size = factor(size_category))) + 
  facet_wrap(~Intervention_labs, ncol = 2) + 
  #geom_text(data = data %>% distinct(Observations),
  #          aes(label = paste("Observations:", Observations), y = -3.5, x  = 1.5), size = 14/.pt, hjust = 0) + 
  geom_text(data = data %>% distinct(Intervention_labs, color_category2, Proportions_color_cat2) %>% filter(color_category2 == "Balanced"),
            aes(label = paste(Proportions_color_cat2, "%", sep = ""), 
                color = color_category2), x = 2.2, y = -3.5, size = 14/.pt, hjust = 0) +
  geom_text(data = data %>% distinct(Intervention_labs, color_category2, Proportions_color_cat2) %>% filter(color_category2 == "Biodiversity positive"),
            aes(label = paste(Proportions_color_cat2, "%", sep = ""), 
                color = color_category2), x = 2.2, y = -0.5, size = 14/.pt, hjust = 0) +
  geom_text(data = data %>% distinct(Intervention_labs, color_category2, Proportions_color_cat2) %>% filter(color_category2 == "Climate positive"),
            aes(label = paste(Proportions_color_cat2, "%", sep = ""), 
                color = color_category2), x = -1.5, y = -3.5, size = 14/.pt, hjust = 0) +
  geom_text(data = data %>% distinct(Intervention_labs, color_category2, Proportions_color_cat2) %>% filter(color_category2 == "Lose-lose"),
            aes(label = paste(Proportions_color_cat2, "%", sep = ""), 
                color = color_category2), x = -1.5, y = -0.5, size = 14/.pt, hjust = 0) +
  geom_text(data = data %>% distinct(Intervention_labs, color_category2, Proportions_color_cat2) %>% filter(color_category2 == "Trade-off"),
            aes(label = paste(Proportions_color_cat2, "%", sep = ""), 
                color = color_category2), x = -1.5, y = -2, size = 14/.pt, hjust = 0) +
  scale_color_manual(values = c("Balanced" = "#008080", "Biodiversity positive" = "#DAA520", "Climate positive" = "#998EC3", "Lose-lose" = "firebrick", "Trade-off" = "pink")) +
  geom_abline (slope=-1, linetype = "dashed", color="black") +
  labs(color = "Impact direction", size = "Number of jointly implemented interventions", x = "Biodiversity impact [z-score, absolute change from reference]", y = "Climate impact [z-score, absolute change from reference]") +
  theme_classic() +
  theme(#aspect.ratio = 0.75,
    strip.background = element_blank(),
    legend.box = "vertical",
    legend.box.just = "left" , 
    legend.justification = "left",
    legend.position = "bottom", legend.title = element_text(size = 14, face = "bold"), legend.text = element_text(size = 14),
    axis.text.y = element_text(size=14,hjust=1),
    axis.text.x = element_text(size=14),
    axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0),size=14,face="bold"),
    axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),size=14,face="bold"),
    plot.title = element_text(face = "bold", hjust = 0.5,size=14),
    strip.text = element_text(size = 14, hjust = 0)) +
  guides(Average = guide_legend(title = "Average"))

a

#HTML/Markdown-formatted cluster text with bold headers
#cluster_text <- paste0(
#  "<b>Cluster 1:</b> Energy lifestyle changes, Energy technology and efficiency, Renewable energy, Building technology and efficiency,<br>", "Energy constraints, Fossil fuel phase out<br>",
#  "<b>Cluster 2:</b> Energy trade policies, Slower infrastructure expansion, Sustainable forest management,<br>", "Universal access to basic needs and services, Energy subsidies and incentives, Industry technology and efficiency, Transport lifestyle changes"
#)

cluster_text <- paste0(
  "<b>Energy and building decarbonization cluster:</b> Energy lifestyle changes, Energy technology and efficiency,<br>", "Renewable energy, Building technology and efficiency, Energy constraints, Fossil fuel phase out<br>")

#create a rich text grob from cluster text, for plotting with gridExtra package
table_grob <- richtext_grob(
  cluster_text,
  gp = gpar(fontsize = 14), x = 0.066, y = 0.5, hjust = 0
)

pdf(paste(figpath, "Figure_4.pdf"), width = 10.5, height = 12)

grid.arrange(a, table_grob, heights = c(3, 0.3))

dev.off()
