###---------------------------------------------------------------------------###
###                Boxplot of climate/biodiversity impacts for number of      ###
###                interventions in a scenario categories (SI)                ###
###                Author: Christian Neumann                                  ###
###                Date: 31st May 2024                                        ###
###---------------------------------------------------------------------------###

###
#1.) Create environment####
###

#load libraries
library(tidyverse)

#define input/output path 
datapath <- paste0(getwd(), "/Database_release_v01/Input/")
figpath <- paste(getwd(), sep = "/", "Database_release_v01/Output/Manuscript/Figures/")

#load data
data <- readRDS(paste(datapath, "10_06_25_Reference_harmonized_data.rds"))

###
#2.) Prepare data (count interventions etc.)####
###

# Define biodiversity and climate change indicator columns
biodiv_cols <- colnames(data)[startsWith(colnames(data), "SDG15")]
cc_cols <- colnames(data)[startsWith(colnames(data), "SDG13")]
interventions <- colnames(data[9:40])
data$Interventions_number <- rowSums(data[, interventions])

data <- data %>% 
  pivot_longer(., starts_with("SDG15"), values_to = "Biodiversity impact", names_to = "Biodiversity_indicator") %>%
  pivot_longer(., starts_with("SDG13"), values_to = "Climate impact", names_to = "Climate_indicator") %>%
  group_by(Index) %>% 
  summarise(Study_Nr = unique(Study_nr.),
            Scenario_type = unique(Scenario_type),
            Index = unique(Index),
            `Biodiversity impact` = mean(`Biodiversity impact`, na.rm = T),
            `Climate impact` = mean(`Climate impact`, na.rm = T)*-1, #*-1 to negate impact, so that a positive impact means positive for biodiversity and climate
            Interventions_number = (unique(Interventions_number)))

#Create groups
data$Interventions_category <- cut(data$Interventions_number, breaks = c(0, 5, 10, 20), labels = c("1-5", "6-10", "> 10"))

data_plot <- data %>%
  pivot_longer(., c("Biodiversity impact", "Climate impact"), values_to = "Impact", names_to = "Impact_class") %>%
  na.omit(.)

#count observations to plot observations side-by-side to boxplots
data_counts_inter <- data_plot %>%
  count(Interventions_category, Impact_class)

###
#3.) Final boxplot####
###

pdf(paste(figpath, "boxplot_no_interventions.pdf"), height = 4, width = 8)
ggplot(data = data_plot, aes(x = Impact, y = Interventions_category, fill = Impact_class)) +
  #facet_wrap(~Impact_class)+
  geom_boxplot() +
  geom_vline(xintercept = 0, color = "darkgrey", linetype = 4) +
  # Add observation counts above boxplots
  geom_text(data = data_counts_inter, aes(x = (max(data_plot$Impact, na.rm = TRUE)) - 0.2, y = as.numeric(as.factor(Interventions_category)) + ifelse(Impact_class == "Biodiversity impact", -0.2, 0.2), label = paste("n =", n)), hjust = 0.5, size = 5) +
  #scale_fill_manual(values = c("policy-screening" = "#DDDDDD", "target-seeking" = "darkgrey")) +
  scale_fill_manual(values = c("Biodiversity impact" = "#DAA520", "Climate impact" = "#998EC3")) +
  labs(fill = "Impact", x = "Impact [Z-score, absolute change from reference]", y = "Number of interventions") +
  theme_classic() +
  theme(#aspect.ratio = 0.75,
    legend.box = "vertical",
    legend.justification = "left",
    legend.position = "bottom", legend.title = element_text(size = 14, face = "bold"), legend.text = element_text(size = 14),
    axis.text.y = element_text(size=14,hjust=1),
    axis.text.x = element_text(size=14),
    axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0),size=14,face="bold"),
    axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),size=14,face="bold"),
    plot.title = element_text(face = "bold", hjust = 0.5,size=14),
    strip.text = element_text(size = 14)) 
dev.off()
