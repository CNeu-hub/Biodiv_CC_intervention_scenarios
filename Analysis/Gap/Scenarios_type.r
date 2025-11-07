###---------------------------------------------------------------------------###
###                Calculates frequency of scenarios/studies                  ### 
###                per scenario type (intervention/non-intervention)          ###
###                Author: Christian Neumann                                  ###
###                Date: July 2025.                                           ###
###---------------------------------------------------------------------------###

###
#1.) Create environment####
###

library(tidyverse)
library(ggpubr)
library(patchwork)
library(colorspace)

#define output/input paths
figpath <- paste(getwd(), "/Output/Figures/", sep = "")
tablepath <- paste(getwd(), "/Output/Tables/", sep = "")
datapath <- paste(getwd(), "/Input/", sep = "")

#load data 
WP3_Subset <- readRDS(file = paste(datapath, "10_06_25_Final_Data_Perc_Ch.Rds", sep = ""))

###
#3.) create a plot showing the no. of scenarios & studies together with their scenario type####
###

#format modest-intervention scenarios in non-intervention scenarios
Studies_Count <- WP3_Subset %>%
  mutate(Scenario_class = ifelse(Scenario_class == "Modest-intervention", "Non-intervention", Scenario_class))

#group and calculate the sum for each group, proportions are calculated by dividing x by total number of studies (60) or scenarios (601) because 60 different studies in results, so proportions should be calculated from those for representing studies that investigated specific scenario classes/types (as most studies investigated not only one but more scenario types)
Studies_Count2 <- Studies_Count %>%
  group_by(Scenario_class, Scenario_type) %>%
  summarise(Scenarios = n(),
            Studies = sum(n_distinct(Study_nr.))) %>% ungroup() %>%
  mutate(Scenarios_perc = round((Scenarios/sum(Scenarios))*100, digits = 1),
         Studies_perc = round((Studies/60)*100, digits = 1)) 

Studies_Count2 <- Studies_Count2 %>%
  pivot_longer(c("Scenarios", "Studies"), names_to = "Type", values_to = "Count") %>%
  mutate(Percentages = ifelse(Type == "Scenarios", Scenarios_perc, Studies_perc)) %>%
  select(-Scenarios_perc, -Studies_perc)

Tabular_output <- Studies_Count2

write.csv(Tabular_output, paste(tablepath, "Scenario_type.csv", sep = ""))

Studies_Count2 <- Studies_Count2 %>%
  mutate(Count = ifelse(Type == 'Scenarios', -Count, Count)) 

col_pal2 <- c("#DDDDDD", "#555555")

#now transform data for barplot that gives better overview of studies:
Intervention <- subset(Studies_Count2, grepl("Intervention", Studies_Count2$Scenario_class))
Intervention$Sum_Focus <- "Intervention"

Non <- subset(Studies_Count2, grepl("Non-intervention", Studies_Count2$Scenario_class))
Non$Sum_Focus <- "Non-intervention"

Count_Summary2<- rbind(Intervention, Non)

individ_scale <- list(
  scale_x_continuous(labels = abs, breaks = c(seq(-500, 0, 100))),
  scale_x_continuous(labels = abs, breaks = c(seq(0, 50, 10))))

#create a diverging HCL palette
col_pal <- grey.colors(7)
col_pal
swatchplot(col_pal)

pdf(paste(figpath, "Supplementary_figure_S2.pdf", sep = "/"), height = 5, width = 12)

b <- ggplotcol_pala <- ggplot(data = Count_Summary2, aes(x = Count, y = reorder(Scenario_class, -abs(Count)), fill = reorder(Scenario_type, abs(Count)))) +
  geom_bar(stat = "identity", width = 0.8,  color = "white") +
  scale_fill_manual(values = col_pal) +
  facet_wrap(~Type, scales = "free_x") +
  ggh4x::facetted_pos_scales(x = individ_scale) +
  #scale_x_continuous(labels = abs, breaks = c(seq(-500, 0, 100),  seq(0, 250, 25))) + # Show absolute values on the x-axis
  labs(x = "No. of scenarios/studies", y = "", fill = "") +
  #ggrepel::geom_text_repel(aes(label = paste0(Percentages, "%")),
  #                         position = position_stack(vjust = 0.5), angle = 0, force = 2, force_pull = 2, size = 14/.pt) + 
  geom_text(aes(label = paste0(Percentages, "%")),
            position = position_stack(vjust = 0.5), angle = 90, size = 14/.pt, check_overlap = TRUE) +
  geom_vline(xintercept = 0) +
  theme_classic() +
  theme(legend.position = "bottom", legend.justification = "left", legend.title = element_text(size = 14, face = "bold"), legend.text = element_text(size = 14)) +
  theme(axis.text.y = element_text(size=14,hjust=0))+
  theme(axis.text.x = element_text(size=14,hjust=0))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0),size=14,face="bold")) +
  theme(axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),size=14,face="bold")) +
  guides(fill = guide_legend(nrow = 2)) +
  theme(strip.text = element_text(size = 14)) +
  theme(plot.title = element_text(face = "bold", hjust =0.5,size=14))

b

dev.off()

