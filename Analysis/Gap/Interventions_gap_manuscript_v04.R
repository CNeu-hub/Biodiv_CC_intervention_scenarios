###---------------------------------------------------------------------------###
###                Plots heatmap of recoded interventions applied in scenarios###
###                and barplots of sectors, types frequencies                 ###
###                Author: Christian Neumann                                  ###
###                Date: 1st June 2025                                        ###
###---------------------------------------------------------------------------###

###
#1.) Create environment####
###

#load libraries
library(tidyverse) #load the tidyverse to restructure data (dplyr), tidy up data (tidyr), text analysis (stringr)
library(RColorBrewer) #color palettes
library(ggh4x) #individual facet grids customization for ggplot2
library(ggpubr)
library(patchwork)

#load functions 
source("Functions/Coding_functions_v03.R")

#define output/input paths
figpath <- paste(getwd(), "/Output/Figures/", sep = "")
tablepath <- paste(getwd(), "/Output/Tables/", sep = "")
datapath <- paste(getwd(), "/Input/", sep = "")

#load data 
WP3_Subset <- readRDS(file = paste(datapath, "10_06_25_Final_Data_Perc_Ch.Rds", sep = ""))

#load lookup table again
lookup <- read_excel(paste(datapath, "Interventions_lookup_v05.xlsx", sep = ""))
lookup <- lookup[3:6] #remove ID and Interventions, we are only interested in Intervention_agg_lvl1 here
colnames(lookup) <- c("Intervention_agg_lvl1", "Intervention_type", "Intervention_category", "Intervention_sector")
lookup <- distinct(lookup) #retain only unique rows for matching frequencies later

#select policy-screening/target-seeking intervention scenarios 
WP3_Subset <- subset(WP3_Subset, WP3_Subset$`Interventions_Yes/No` == "Yes" & WP3_Subset$Scenario_class == "Intervention" & WP3_Subset$Scenario_type == "policy-screening" | WP3_Subset$Scenario_type == "target-seeking")

###
#2.) Intervention heatmap####
###

###Showing no. of scenarios per intervention, sector, category, and type in a heatmap
#Calculate frequency of interventions across scenarios for agg_lvl1
Intervention_data <- Frequency(WP3_Subset$Intervention_agg_lvl1)
colnames(Intervention_data) <- c("Intervention_agg_lvl1", "Frequency")

#add up categorization using lookup table
Intervention_data <- Intervention_data %>%
  left_join(lookup, by = c("Intervention_agg_lvl1" = "Intervention_agg_lvl1")) 

#using ggh4x package here, allows for in depth customization of facet grids 
strip_variation <- ggh4x::strip_themed(
  #NULL,
  # Horizontal strips
  background_x = elem_list_rect(fill = c("#DDDDDD", "#AEAEAE"), colour = c(rep("white",2))),
  text_x = elem_list_text(size = 14, face = c("bold")),
  by_layer_x = FALSE,
  # Vertical strips
  background_y = elem_list_rect(fill = c(c("#DDDDDD", "#AEAEAE", "#DDDDDD", "#AEAEAE", 
                                           "#DDDDDD", "#AEAEAE", "#DDDDDD", "#AEAEAE", "#DDDDDD")), colour = c(rep("white",9))),
  text_y = elem_list_text(size = 14, face = c("bold")),
  by_layer_y = FALSE)

#create interventions factor for ordered plotting: 
Intervention_data$Intervention_agg_lvl1 <- factor(Intervention_data$Intervention_agg_lvl1, levels = sort(unique(Intervention_data$Intervention_agg_lvl1), decreasing = TRUE))

#ggplot heatmap with geom_tile & facet_grid
a <- ggplot(Intervention_data, aes(x = Intervention_type, y = Intervention_agg_lvl1, fill = Frequency)) +
  geom_tile(color = "white") +  
  geom_text(aes(label = paste0(Frequency)), size = 14/.pt, color = ifelse(Intervention_data$Frequency < 130, "black", "white")) + # fontface = ifelse(!is.na(test3$P.Value) & test3$P.Value < 0.05, "bold", "plain")) +
  scale_fill_gradient2() +
  scale_x_discrete(position = "bottom") +
  ggh4x::facet_grid2(rows = vars(Intervention_sector),cols = vars(Intervention_category), scales = "free", space = "free", switch = "both", strip = strip_variation) +
  labs(fill = "No. of scenarios", y = "Intervention", x = "Intervention type") +
  geom_hline(yintercept = c(0.4), color = "#AEAEAE", linetype = 4, size = 0.8) +
  theme_void()+
  theme(legend.position.inside = c(-0.8, -0.15),
        legend.position = "inside",
        legend.justification = c(0),
        legend.title.position = "left",
        legend.text = element_text(size=14), 
        legend.title = element_text(size=14,face="bold"),
        legend.margin=margin(grid::unit(0, "cm")), legend.key.height=grid::unit(0.3, "cm"), legend.key.width=grid::unit(1, "cm"),
        panel.spacing=unit(0,units = "cm"),
        strip.placement = "inside",                      # Place facet labels outside x axis labels.
        strip.background = element_rect(fill = "white"),
        axis.ticks = element_blank(), # Remove axis ticks
        axis.title = element_blank(), # Remove axis titles
        axis.line = element_blank(),
        axis.text.y = element_text(size=14, hjust = 1),
        axis.text.x = element_text(size=14, angle = 90, vjust = 1, hjust = 1),
        strip.text = element_text(size = 14)) 

a

###
#2.) Sectors frequency per study and scenario barplot####
###

data <- Frequency(WP3_Subset$Intervention_sector)
Sectors_data <- as.vector(data$Overview)

#add up all interventions as separate column
for (i in Sectors_data) {
  WP3_Subset[[i]] <- NA  
}

#fill columns if intervention was applied = 1 in column, if it was not applied in scenario = 0
for (name in Sectors_data) {
  WP3_Subset[[name]] <- ifelse(grepl(name, WP3_Subset$Intervention_sector), 1, 0)
}

#WP3_Subset <- WP3_Subset[c(1:17, 38, 163:204)]

WP3_subset_long <- pivot_longer(WP3_Subset, c("AFOLU", "Building", "Energy", "Food", "Industry", "Mixed", "Nature", "Transport"), names_to = "Sectors", values_to = "Value")

# Group by 'Group' and calculate the sum for each group
Studies_Count <- WP3_subset_long %>%
  group_by(Sectors) %>%
  filter(., Value == 1) %>%
  reframe(
    Scenarios = sum(Value),
  )

col_pal <- c("#DDDDDD", "#555555")

b <- ggplot(data = Studies_Count, aes(x = Scenarios, y = reorder(Sectors, abs(Scenarios)))) +
  geom_bar(stat = "identity", width = 0.8, color = "white", fill = "#DDDDDD")  +
  # scale_fill_manual(values = c("darkolivegreen", "steelblue4")) +
  scale_fill_manual(values = col_pal) +
  scale_x_continuous(labels = abs, limits = c(0, 500), breaks = seq(0, 500, 50)) + # Show absolute values on the x-axis
  labs(x = "Count", y = "Sector", fill = "") +
  #geom_vline(xintercept = 0) +
  theme_classic() +
  theme(legend.position = "top", legend.title = element_text(size = 14, face = "bold"), legend.text = element_text(size = 14)) +
  theme(axis.text.y = element_text(size=14,hjust=1))+
  theme(axis.text.x = element_text(size=14)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0),size=14,face="bold")) +
  theme(axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),size=14,face="bold")) +
  theme(plot.title = element_text(face = "bold", hjust =0.5,size=14))

b

###
#3.) Type frequency per study and scenario barplot####
###

data <- Frequency(WP3_Subset$Intervention_type)
Type_data <- as.vector(data$Overview)

#add up all interventions as separate column
for (i in Type_data) {
  WP3_Subset[[i]] <- NA  
}

#fill columns if intervention was applied = 1 in column, if it was not applied in scenario = 0
for (name in Type_data) {
  WP3_Subset[[name]] <- ifelse(grepl(name, WP3_Subset$Intervention_type), 1, 0)
}

#WP3_Subset <- WP3_Subset[c(1:17, 38, 163:204)]

WP3_subset_long2 <- pivot_longer(WP3_Subset, c("Economic instruments", "Land use management", "Lifestyle", "Regulatory standards", "Technologies", "Waste management", "Social instruments"), names_to = "Intervention_types", values_to = "Value")

# Group by 'Group' and calculate the sum for each group
Studies_Count2 <- WP3_subset_long2 %>%
  group_by(Intervention_types) %>%
  filter(., Value == 1) %>%
  reframe(
    Scenarios = sum(Value)
  )

col_pal <- c("#DDDDDD", "#555555")

c <- ggplot(data = Studies_Count2, aes(x = Scenarios, y = reorder(Intervention_types, abs(Scenarios)), fill = "#DDDDDD")) +
  geom_bar(stat = "identity", width = 0.8, color = "white") +
  # scale_fill_manual(values = c("darkolivegreen", "steelblue4")) +
  scale_fill_manual(values = col_pal) +
  scale_x_continuous(labels = abs, breaks = c(seq(0, 500, 50))) + # Show absolute values on the x-axis
  labs(x = "Count", y = "Intervention type", fill = "") +
  #geom_vline(xintercept = 0) +
  theme_classic() +
  theme(legend.position = "none", legend.title = element_text(size = 14, face = "bold"), legend.text = element_text(size = 14)) +
  theme(axis.text.y = element_text(size=14,hjust=1))+
  theme(axis.text.x = element_text(size=14)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0),size=14,face="bold")) +
  theme(axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),size=14,face="bold")) +
  theme(plot.title = element_text(face = "bold", hjust =0.5,size=14))

c

###
#4.) Intervention category frequency per study and scenario barplot####
###

data <- Frequency(WP3_Subset$Intervention_category)
Type_data <- as.vector(data$Overview)

#add up all interventions as separate column
for (i in Type_data) {
  WP3_Subset[[i]] <- NA  
}

#fill columns if intervention was applied = 1 in column, if it was not applied in scenario = 0
for (name in Type_data) {
  WP3_Subset[[name]] <- ifelse(grepl(name, WP3_Subset$Intervention_category), 1, 0)
}

#WP3_Subset <- WP3_Subset[c(1:17, 38, 163:204)]

WP3_subset_long3 <- pivot_longer(WP3_Subset, c("Physical interventions", "Policy instruments"), names_to = "Drivers", values_to = "Value")

# Group by 'Group' and calculate the sum for each group
Studies_Count3 <- WP3_subset_long3 %>%
  group_by(Drivers) %>%
  filter(., Value == 1) %>%
  reframe(
    Scenarios = sum(Value)
  )

col_pal <- c("#DDDDDD", "#555555")

d <- ggplot(data = Studies_Count3, aes(x = Scenarios, y = reorder(Drivers, abs(Scenarios)), fill = "#DDDDDD")) +
  geom_bar(stat = "identity", width = 0.8, color = "white") +
  # scale_fill_manual(values = c("darkolivegreen", "steelblue4")) +
  scale_fill_manual(values = col_pal) +
  scale_x_continuous(labels = abs, breaks = c(seq(0, 500, 50))) + # Show absolute values on the x-axis
  labs(x = "Count", y = "Physical/Non-physical", fill = "") +
  #geom_vline(xintercept = 0) +
  theme_classic() +
  #theme(aspect.ratio = 0.75) +
  theme(legend.position = "none", legend.title = element_text(size = 14, face = "bold"), legend.text = element_text(size = 14)) +
  theme(axis.text.y = element_text(size=14,hjust=1))+
  theme(axis.text.x = element_text(size=14)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0),size=14,face="bold")) +
  theme(axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),size=14,face="bold")) +
  theme(plot.title = element_text(face = "bold", hjust =0.5,size=14))

d

###
#5.) Arranging final plots####
###

#create combined plot with heatmap + barplots 
a <- a + theme(aspect.ratio = NULL, plot.margin = margin(5, 5, 5, 5))
b <- b + theme(aspect.ratio = NULL, plot.margin = margin(5, 5, 5, 5))
c <- c + theme(aspect.ratio = NULL, plot.margin = margin(5, 5, 5, 5))
d <- d + theme(aspect.ratio = NULL, plot.margin = margin(5, 5, 5, 5))

b <- b + scale_x_continuous(limits = c(0, 400), breaks = seq(0, 500, 100)) + theme(axis.title.y = element_blank(), axis.text.y = element_text(hjust = 1)) #+ 
#scale_y_discrete(position = "right") + scale_x_reverse(limits = c(400, 0))
b

c <- c + scale_x_continuous(limits = c(0, 400), breaks = seq(0, 500, 100)) +  theme(axis.title.y = element_blank(), axis.text.y = element_text(hjust = 1)) #+ 
#scale_y_discrete(position = "right") + scale_x_reverse(limits = c(400, 0))

d <- d + scale_x_continuous(limits = c(0, 400), breaks = seq(0, 500, 100)) +  theme(axis.title.y = element_blank(),axis.text.y = element_text(hjust = 1)) #+ 
#scale_y_discrete(position = "right") + scale_x_reverse(limits = c(400, 0))

e <- d/b/c +
  plot_layout(heights = c(0.25,1,1), axis_titles = "collect_x")
e

pdf(paste(figpath, "Interventions_v06_12_06_25.pdf", sep = ""), height = 10, width = 14)
combined_plot <- (a|free(e, side = "b", type = "space")) + plot_layout(widths = c(0.7, 0.3)) + plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") 
combined_plot
dev.off()

#just heatmap output 
pdf(paste(figpath, "Interventions_heatmap.pdf", sep = ""), width = 10, height = 10)
a
dev.off()

#just barplots output 
f <- b|c|d + 
  plot_layout() +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") 
f

pdf(paste(figpath, "Interventions_categories.pdf", sep = ""), width = 14, height = 4)
f
dev.off()

#create table output of frequencies
write.csv(Studies_Count, file = paste(tablepath, "Intervention_sector_frequency.csv", sep = ""))
write.csv(Studies_Count2, file = paste(tablepath, "Intervention_type_frequency.csv", sep = ""))
write.csv(Studies_Count3, file = paste(tablepath, "Physical_policy_frequency.csv", sep = ""))

