###---------------------------------------------------------------------------###
###                Calculates frequency of scenarios/studies                  ### 
###                per scenario type and impact focus                         ###
###                Author: Christian Neumann                                  ###
###                Date: 1st June 2025                                        ###
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
#2.) Venn diagram: Create a plot showing the no. of scenarios & studies together with their main focus####
###

#group and calculate the sum for each group, proportions are calculated by dividing x by the appropriate marginal sums (We use this approach because some studies investigated more than one impact/type/etc.)
#Studies_Count <- WP3_Subset %>%
#  group_by(Impact_focus) %>%
#  summarise(Scenarios = n(),
#            Studies = sum(n_distinct(`Study_nr.`))) %>%
#  mutate(Scenarios_perc = round((Scenarios/sum(Scenarios))*100, digits = 1),
#         Studies_perc = round((Studies/sum(Studies))*100, digits = 1)) #sum of studies, because some assessment reports assessed in one scenario only climate and in another only biodiversity for example, this leads to doublecounting of studies which is why we use 64 instead of the original 60 studies (e.g.: OECD 2050 study)

#new version (separate calculation for studies and scenarios, because some studies include scenarios that investigate single impacts but the whole study assesses impacts for more than one impact in different scenarios)
Scenarios_Count <- WP3_Subset %>%
  group_by(Impact_focus) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = round((Count/sum(Count))*100, digits = 1),
         Type = "Scenarios")

#Now the same for studies but set impact focus study-specific before summarizing, not scenario specific (OECD study had different scenarios investigating different impacts, but for consistency we code this here as SDG biodiversity and climate impacts for each scenario (study-specific))
Studies_Count <- WP3_Subset %>%
  mutate(Impact_focus = ifelse(WP3_Subset$Study_nr. == 29 | WP3_Subset$Study_nr. == 12 | WP3_Subset$Study_nr. == 45, "Biodiversity, Climate change, SDGs", WP3_Subset$Impact_focus)) %>%
  group_by(Impact_focus) %>%
  summarise(Count = sum(n_distinct(`Study_nr.`))) %>%
  mutate(Percentage = round((Count/sum(Count))*100, digits = 1),
         Type = "Studies")

#now rbind final output as input 
Studies_Count <- rbind(Studies_Count, Scenarios_Count)

#Studies_Count <- Studies_Count %>%
#  pivot_longer(c("Scenarios", "Studies"), names_to = "Type", values_to = "Count") %>%
#  mutate(Percentages = ifelse(Type == "Scenarios", Scenarios_perc, Studies_perc)) %>%
#  select(-Scenarios_perc, -Studies_perc)

Tabular_output <- Studies_Count

write.csv(Tabular_output, paste(tablepath, "Main_focus.csv", sep = ""))

#VENN diagram output
scenarios_count <- subset(Studies_Count, Studies_Count$Type == "Scenarios")

#used total frequency of all scenarios per impact category for main circles, because otherwise visulization will not work properly
euler_data_log <- eulerr::euler(c("Climate change" = 249+31+183+22, #total frequency of all scenarios that investigated climate change
                              "Biodiversity" = 109+22+183+3,  #total frequency of all scenarios that investigated biodiversity 
                              "Other SDGs" = 4+3+183+31,  #total frequency of all scenarios that investigated SDGs
                              "Climate change&Biodiversity" = 22,
                              "Climate change&Other SDGs" = 31,
                              "Climate change&Biodiversity&Other SDGs" = 183,
                              "Biodiversity&Other SDGs" = 3),
                            #input = "union",
                            quantities = TRUE, 
                            shape = "circle")

#manual label definition because we log-transformed scenarios counts for representation as euler diagram 
labels_text <- sprintf("%d\n(%.1f%%)", 
                       c(249, 109, 4, 22, 31, 3, 183), 
                       (c(249, 109, 4, 22, 31, 3, 183)/sum(c(249, 109, 4, 22, 31, 3, 183))) * 100)

# Generate plot with numbers + percentages and custom colors
Scenarios_plot <- plot(euler_data_log,
     fills = c("Climate change" = "steelblue",   
               "Biodiversity" = "#005700",     
               "Other SDGs" = "#8E063B"),            
     alpha = 0.3,
     quantities = list(labels = labels_text, fontsize = 16),
     labels = list(fontsize = 16),
     main = list(label = "Scenarios", font = 2 , fontsize = 14)) 
     #legend = list(labels = c("Climate change", "Biodiversity", "SDGs"), fontsize = 14))

Scenarios_plot

studies_count <- subset(Studies_Count, Studies_Count$Type == "Studies")

#used total frequency of all studies per impact category for main circles, because otherwise visulization will not work properly
euler_data_log <- eulerr::euler(c("Climate change" = 29+10+2+7, #total frequency of all studies that investigated climate change
                              "Biodiversity" = 11+10+1+2, #total frequency of all studies that investigated biodiversity 
                              "Other SDGs" = 1+7+10, #total frequency of all studies that investigated SDGs
                              "Climate change&Biodiversity" = 2,
                              "Climate change&Other SDGs" = 7,
                              "Climate change&Biodiversity&Other SDGs" = 10,
                              "Biodiversity&Other SDGs" = 1),
                            #input = "union",
                            quantities = TRUE, 
                            shape = "circle")
# shape = "ellipse")

labels_text <- sprintf("%d\n(%.1f%%)", 
                       c(29, 11, 0, 2, 7, 1, 10), 
                       (c(29, 11, 0, 2, 7, 1, 10)/sum(c(29, 11, 0, 2, 7, 1, 10))) * 100) #sum of studies = 64, because some assessment reports assessed in one scenario only climate and in another only biodiversity for example, this leads to doublecounting of studies which is why we use 64 instead of the original 60 studies (e.g.: OECD 2050 study)

# Generate plot with numbers + percentages and custom colors
Studies_plot <- plot(euler_data_log,
                       fills = c("Climate change" = "steelblue",  
                                 "Biodiversity" = "#005700",     
                                 "Other SDGs" = "#8E063B"), 
                     labels = list(fontsize = 16),
                     alpha = 0.3,
                     quantities = list(labels = labels_text, fontsize = 16),
                     main = list(label = "Studies", font = 2 , fontsize = 14)) 
                       #legend = list(side = "bottom", labels = c("Climate change", "Biodiversity", "SDGs"), fontsize = 14))

Studies_plot

#arrange with patchwork (possible for eulerrr plots with wrap_elements!)
final_venn <- wrap_elements(Scenarios_plot) + wrap_elements(Studies_plot) +
  plot_layout(guides = "collect", widths = c(0.2, 0.2)) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(
    legend.position = "bottom",
    plot.tag.position = c(0.05, 0.8),
    plot.tag = element_text(size = 14, hjust = 0, vjust = 1),
    plot.title = element_text(size = 16, face = "bold"))
final_venn

###
#3.) Calculate frequency of collected metrics (implicit + explicit impacts)####
###

#create indicator vector of colnames to loop over
Indicators <- c(colnames(WP3_Subset %>% select(contains("SDG"), -ends_with("Impact"), -ends_with("horizon"), -ends_with("raw_data"), -ends_with("steps"), -ends_with("decades"))))

# Create an empty data frame
sums <- data.frame(indicator = character(length(Indicators)),
                   frequency = numeric(length(Indicators)),
                   studies = numeric(length(Indicators)),
                   SDG = character(length(Indicators)))

#create list to calculate unique studies for each SDG (Important for calculating % of studies that calculated indicators within SDG)
unique_studies_per_SDG <- list()

for (i in seq_along(Indicators)) {
  
  sums[i, "indicator"] <- Indicators[i]
  sums[i, "frequency"] <- sum(!is.na(WP3_Subset[[Indicators[i]]]))
  sums[i, "studies"] <- length(unique(WP3_Subset$Study_nr.[!is.na(WP3_Subset[[Indicators[i]]])]))
  
  #extract first part of the string
  category_parts <- unlist(strsplit(Indicators[i], "_"))
  sums[i, "SDG"] <- category_parts[1]
  
  #calculate unique studies for current indicator
  unique_studies <- unique(WP3_Subset$Study_nr.[!is.na(WP3_Subset[[Indicators[i]]])])
  
  #sum up count of unique studies for each SDG
  SDG <- category_parts[1]
  if (is.null(unique_studies_per_SDG[[SDG]])) {
    unique_studies_per_SDG[[SDG]] <- unique_studies
  } else {
    unique_studies_per_SDG[[SDG]] <- union(unique_studies_per_SDG[[SDG]], unique_studies)
  }
}

#calculate the total number of unique studies
total_unique_studies <- length(unique(WP3_Subset$Study_nr.))

#calculate the number of unique studies per SDG
for (i in seq_along(unique_studies_per_SDG)) {
  unique_studies_per_SDG[[i]] <- length(unique_studies_per_SDG[[i]])
}

sums$indicator <- gsub("_"," ",sums$indicator)
sums$indicator <- gsub("SDG1|SDG2|SDG3|SDG4|SDG5|SDG6|SDG7|SDG8|SDG9|SDG10|SDG11|SDG12|SDG13|SDG14|SDG15|SDG16|SDG17","",sums$indicator)

sums$SDG <- factor(sums$SDG, levels = c("SDG1","SDG2","SDG3","SDG4","SDG5","SDG6","SDG7","SDG8","SDG9","SDG10","SDG11","SDG12","SDG13","SDG14","SDG15","SDG16","SDG17"))

#calculate overall used studies 
studies_count <- sum(n_distinct(WP3_Subset$Study_nr.))

#merge number of unique studies per SDG and sums 
for(sdg in unique(sums$SDG)) {
  sums$unique_studies_SDG_count[sums$SDG == sdg] <- unique_studies_per_SDG[[sdg]]
}

#get the name and the y position of each label
sums$id <- seq(1,nrow(sums))
sums$id2 <- as.numeric(str_remove(sums$SDG, "SDG"))

#calculate percentage coverage of indicators & SDGs per study 

sums <- sums %>% 
  group_by(indicator) %>%
  mutate(Percentage_Indicators = (studies / 60) * 100) %>% #64 = sum of studies, because some assessment reports assessed in one scenario only climate and in another only biodiversity for example, this leads to doublecounting of studies which is why we use 64 instead of the original 60 studies (e.g.: OECD 2050 study)
  ungroup() %>%
  group_by(SDG) %>%
  mutate(Percentage_SDGs = (unique(unique_studies_SDG_count) / 60) *100) %>%
  mutate(xmin = min(as.numeric(id)) - 0.5,
         xmax = max(as.numeric(id)) + 0.5,
         ymin = -0.5,
         ymax = -Percentage_SDGs) %>%
  ungroup()

sums$Percentage_Indicators <- round(sums$Percentage_Indicators, digits = 1)
sums$Percentage_SDGs <- round(sums$Percentage_SDGs, digits = 1)

###
#4.) Calculate position indices and coordinates of bars for plotting, create indicator labels####
###

label_data <- sums
label_data$indicator_shortcut <- c("Extreme poverty", "Population < 1 $",
                                   "Hunger risk", "Undernourishment", "Hunger incidence", "Malnourished children", "FPI 2005", "FPI 2010", "FPI 2015", "Change food prices", "Fertilizer use", "Nitrogen use",
                                   "NO2 emissions", "NOx emissions", "SO2 emissions", "SOx emissions", "BC emissions", "PM2.5 emissions",
                                   "MYS", "Adults no education", "Female education", "Primary education", "Gender rat. prim. edu.", "Gender rat. sec. edu.",
                                   "Access improved water", "Access basic sanitation", "Lack improved water", "Water stress river bas.", "Water stress", "Agr. irrigation",
                                   "Share renewables", "UE build. tr. p. cap.", 
                                   "Income convergence", "Unemployment", "Unemployment rate", "Ind. hydrogen electr. share", 
                                   "International inequity", "National equity", "International equity", "Rel. poverty", "Rat. GDP/cap", "10% richest/10% poorest rat.", "GINI coefficient",
                                   "Urban PM2.5",
                                   "Food waste (kcal/cap/day)", "Food waste (Mt/yr)",
                                   "GHG emissions", "CO2 emissions", "AFOLU CO2 emissions", "AFOLU GHG emissions", "Forcing", "CO2 concentration", "Temperature change", "Carbon price",
                                   "River discharge N", "River discharge P", "MTI", "MSA aquatic", "Ocean acidification", 
                                   "Habitat range size", "Species 50% range loss", "GMA", "INSIGHTS Index", "Habitat loss", "Biodiv. hotspot loss", "Red. vasc. plant sp.", "Extinction MSY", "AOH/ESH", "Species range protection lvl.", "Ecoregions protection lvl.", "Deforestation", "NCI-pb", "LPI", "RLI", "MSA terrestrial", "BII", "FRRS", "FGRS CB17BDM", "FGRS US16BDM", "PDF", "Nitrogen fixation", "Mean sp. richness",
                                   "Peace probability", "Equality",
                                   "Int. climate finance") 

#calculate angle for each bar (indicator), used for plotting (i.e. arranging bars)
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar 
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

label_sdgs <- sums 
number_of_bar <- nrow(label_sdgs)
angle <- 90 - 360 * (label_sdgs$id-0.5) /number_of_bar
label_sdgs$hjust <- ifelse(angle < -90, 1, 0)
label_sdgs$angle <- ifelse(angle < -90, angle+180, angle)
label_sdgs$x <- (label_sdgs$xmax+label_sdgs$xmin)/2
label_sdgs$y <- (label_sdgs$ymax+label_sdgs$ymin)/2
label_sdgs <- label_sdgs %>%
  group_by(SDG) %>%
  mutate(angle = mean(angle))

#create labeling and color vectors 
#colors are based on United Nations (2019) HEX codes: https://www.un.org/sustainabledevelopment/wp-content/uploads/2019/01/SDG_Guidelines_AUG_2019_Final.pdf 
colors = c("#e5243b","#DDA63A", "#4C9F38", "#C5192D", "#FF3A21", "#26BDE2", "#FCC30B", "#A21942",
           "#FD6925", "#DD1367", "#FD9D24", "#BF8B2E", "#3F7E44", "#0A97D9", "#56C02B", "#00689D","#19486A" )

labels = c("SDG 1 No Poverty","SDG 2 Zero Hunger","SDG 3 Good Health and Well-Being","SDG 4 Quality Education","SDG 5 Gender Equality","SDG 6 Clean Water and Sanitation","SDG 7 Affordable and Clean Energy","SDG 8 Decent Work and Economic Growth",
           "SDG 9 Industry, Innovation and Infrastructure","SDG 10 Reduced Inequalities","SDG 11 Sustainable Cities and Communities","SDG 12 Responsible Consumption and Production","SDG 13 Climate Action","SDG 14 Life below Water","SDG 15 Life on Land","SDG 16 Peace, Justice and Strong Institutions","SDG 17 Partnerships for the Goals")

labels2 <- c("SDG1", "SDG2", "SDG3", "SDG4", "SDG5", "SDG6", "SDG7", "SDG8", "SDG9", "SDG10", "SDG11", "SDG12", "SDG13", "SDG14", "SDG15", "SDG16", "SDG17")

label_sdgs$label <- ifelse(label_sdgs$hjust == 1, paste0("<b>", label_sdgs$SDG, "</b> ", label_sdgs$Percentage_SDGs, "%"), paste0(label_sdgs$Percentage_SDGs, "% ", "<b>", label_sdgs$SDG, "</b>"))
label_sdgs_unique <- label_sdgs %>% distinct(SDG, .keep_all = TRUE)

###
#5.) Plot circular histogram and create exports####
###

#plot circular histogram
pdf(paste(figpath, "Supplementary_figure_S3.pdf"), height = 15, width = 13, paper = "special")

histogram <- ggplot(sums, aes(x=as.factor(id), y=Percentage_Indicators, fill = SDG)) +     
  geom_bar(stat="identity") +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),alpha = 0.1) +
  #geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = +10), alpha = 0.2) +
  scale_fill_manual(values = colors, labels = labels2)+
  geom_text(data=label_data, aes(x=id, y=Percentage_Indicators, label= ifelse(hjust == 1, paste0(indicator_shortcut, " ", Percentage_Indicators, "%"), paste0(Percentage_Indicators, "% ", indicator_shortcut)), hjust=hjust), fontface = "bold", color="black",alpha=0.6, size = 16/.pt, angle= label_data$angle, inherit.aes = FALSE ) +
  ggtext::geom_richtext(data = label_sdgs_unique, aes(x = x, y = ymin-25, label = label), fill = NA, label.color = NA, color = "black", size = 16 / .pt, angle = label_sdgs_unique$angle)+
  labs(fill = "") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(0, 0, 0, 0),
    legend.position = c("none")) +
    #plot.margin = unit(rep(-0.1, 10), "cm")) +
  coord_radial(start = 0, clip = "off") 
histogram

dev.off()

#MANUSCRIPT VERSION #2: 
colors = c("#8E063B","#8E063B", "#8E063B", "#8E063B", "#8E063B", "#8E063B", "#8E063B", "#8E063B",
           "#8E063B", "#8E063B", "#8E063B", "#8E063B", "steelblue", "#8E063B", "#005700", "#8E063B","#8E063B", "white", "white")

labels = c("SDG 1 No Poverty","SDG 2 Zero Hunger","SDG 3 Good Health and Well-Being","SDG 4 Quality Education","SDG 5 Gender Equality","SDG 6 Clean Water and Sanitation","SDG 7 Affordable and Clean Energy","SDG 8 Decent Work and Economic Growth",
           "SDG 9 Industry, Innovation and Infrastructure","SDG 10 Reduced Inequalities","SDG 11 Sustainable Cities and Communities","SDG 12 Responsible Consumption and Production","SDG 13 Climate Action","SDG 14 Life below Water","SDG 15 Life on Land","SDG 16 Peace, Justice and Strong Institutions","SDG 17 Partnerships for the Goals")

labels2 <- c("SDG1", "SDG2", "SDG3", "SDG4", "SDG5", "SDG6", "SDG7", "SDG8", "SDG9", "SDG10", "SDG11", "SDG12", "SDG13", "SDG14", "SDG15", "SDG16", "SDG17")

sums$xmax <- sums$xmax-0.01
sums$xmin <- sums$xmin+0.1

label_sdgs <- sums 
placeholder <- sums[1, ] %>%
  mutate(indicator = "placeholder", frequency = 0, studies = 0, SDG = "", unique_studies_SDG_count = 0, Percentage_Indicators = "", ymin = -0.5, ymax = 30, Percentage_SDGs = "", id = 86, id2 = 18, xmin = 85.5, xmax = 120)
label_sdgs <- rbind(label_sdgs, placeholder)

number_of_bar <- nrow(label_sdgs)
angle <- 90 - 360 * (label_sdgs$id-0.5) / number_of_bar
label_sdgs$hjust <- ifelse(angle < -90, 1, 0)
label_sdgs$angle <- ifelse(angle < -90, angle+180, angle)
label_sdgs$x <- (label_sdgs$xmax+label_sdgs$xmin)/2
label_sdgs$y <- (label_sdgs$ymax+label_sdgs$ymin)/2

label_sdgs <- label_sdgs %>%
  group_by(SDG) %>%
  mutate(angle = mean(angle))

label_sdgs$label <- ifelse(label_sdgs$hjust == 1, paste0("<b>", label_sdgs$SDG, "</b> ", label_sdgs$Percentage_SDGs, "%"), paste0(label_sdgs$Percentage_SDGs, "% ", "<b>", label_sdgs$SDG, "</b>"))
label_sdgs_unique <- label_sdgs %>% distinct(SDG, .keep_all = TRUE) %>% mutate(label = case_when(SDG == "" ~ "", TRUE ~ label))

label_sdgs_unique$angle <- label_sdgs_unique$id2 + label_sdgs_unique$angle
label_sdgs_unique$angle <- c(86.5, 62.75, 40, 20, 7, -7, -20, -28, -35, -50, 118, 112, 94, 70, 24, -20, -24, -70)


#5.) Plot circular histogram and create exports####
###

#plot circular histogram
pdf(paste(figpath, "Figure_2c.pdf"), height = 16, width = 16, paper = "special")

histogram <- ggplot(sums, aes(x=as.factor(id), y=Percentage_Indicators)) +     
  geom_hline(yintercept = 25, color = "grey60", linetype = "dashed") +
  geom_hline(yintercept = 50, color = "grey20", linetype = "dashed") +
  geom_bar(stat="identity", aes(fill = SDG), alpha = 1) +
  geom_rect(data = subset(sums, !duplicated(SDG)), aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = SDG), alpha = 0.3) +
  #geom_rect(aes(xmin = 85.5, xmax = 100, ymin = 0, ymax = 0, fill = "white"), alpha = 0.3) +
  #geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = +10), alpha = 0.2) +
  scale_fill_manual(values = colors, labels = labels2) +
  #geom_text(data=label_data, aes(x=id, y=Percentage_Indicators, label= ifelse(hjust == 1, paste0(indicator_shortcut, " ", Percentage_Indicators, "%"), paste0(Percentage_Indicators, "% ", indicator_shortcut)), hjust=hjust), fontface = "bold", color="black",alpha=0.6, size = 16/.pt, angle= label_data$angle, inherit.aes = FALSE ) +
  ggtext::geom_richtext(data = label_sdgs_unique, aes(x = x, y = ymin-25, label = label), fill = NA, label.color = NA, color = "black", alpha = 1, size = 16 / .pt, angle = label_sdgs_unique$angle)+
  labs(fill = "") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(0, 0, 0, 0),
    legend.position = c("none")) +
  #plot.margin = unit(rep(-0.1, 10), "cm")) +
  coord_radial(start = 0, clip = "off")  
histogram

dev.off()

###
#6.) Bar plot (Climate and biodiversity metrics)####
###

#subset to only biodiversity and climate metrics for visualization in main manuscript
metrics <- sums %>%
  filter(SDG == "SDG13" | SDG == "SDG15" & !(indicator %in% c(" Deforestation", " Nitrogen fixation (Mt N/yr)"))) %>%
  select(indicator, studies, Percentage_Indicators, SDG) %>%
  mutate(impact = case_when(SDG == "SDG13" ~ "Climate change", SDG == "SDG15" ~ "Biodiversity"))

histogram <- ggplot(metrics, aes(x = reorder(indicator, Percentage_Indicators, decreasing = TRUE), y = Percentage_Indicators, fill = impact)) +     
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("#0057004D", "#4682B44D"))+
  scale_y_continuous(breaks = seq(0, 100, 10)) + 
  #scale_x_discrete(labels = c(" Total CO2 emissions (Mt CO2/yr)" = expression("Total "*CO[2]*" emissions (Mt "*CO[2]*"/yr)"),
  #                            " Forcing (W/m-2)" = expression("Forcing"~(W/m^2)),
  #                            " Total GHG emissions (Mt CO2eq/yr)" = expression("Total GHG emissions (Mt "*CO[2]*"eq"*"/yr)"),
  #                            " AFOLU CO2 emissions (Mt CO2/yr)" = expression("AFOLU "*CO[2]*"emissions (Mt "*CO[2]*"/yr)"),
  #                            " Carbon price (US$/t CO2)" = expression("Carbon price (US$/t "*CO[2]*")"),
  #                            " CO2 concentration (ppm)" = expression(CO[2]*" concentration (ppm)"),
  #                            " AFOLU emissions (Mt CO2eq/yr)" = expression("AFOLU emissions (Mt "*CO[2]*"eq/yr)"))) +
  scale_x_discrete(labels = c(" Total CO2 emissions (Mt CO2/yr)" = expression("Total "*CO[2]*" emissions"),
                              " Forcing (W/m-2)" = expression("Forcing"),
                              " Temperature change (Since pre-industrial age)" = "Temperature change",
                              " Total GHG emissions (Mt CO2eq/yr)" = expression("Total GHG emissions"),
                              " AFOLU CO2 emissions (Mt CO2/yr)" = expression("AFOLU "*CO[2]*"emissions"),
                              " Carbon price (US$/t CO2)" = expression("Carbon price"),
                              " CO2 concentration (ppm)" = expression(CO[2]*" concentration"),
                              " AFOLU emissions (Mt CO2eq/yr)" = expression("AFOLU GHG emissions"),
                              " Mean species abundance terrestrial (MSA)" = "Mean species abundance",
                              " Biodiversity intactness index (BII)" = "Biodiversity intactness index", 
                              " Area of habitat (ESH/AOH)" = "Area of habitat",
                              " Living planet index (LPI)" = "Living planet index",
                              " Red list index (RLI)" = "Red list index",
                              " Biodiversity hotspot loss (%)" = "Biodiversity hotspot loss",
                              " Ecoregions protection level (%)" = "Ecoregions protection level",
                              " Fraction globally remaining species (cSAR CB17BDM)" = "Fraction glob. rem. sp. (CB17BDM)",
                              " Fraction globally remaining species (cSAR US16BDM)" = "Fraction glob. rem. sp. (US16BDM)",
                              " Fraction regionally remaining species (FRRS)" = "Fraction reg. rem. species",
                              " Geometric mean abundance (GMA)" = "Geometric mean abundance",
                              " Habitat range size (%)" = "Habitat range size",
                              " Loss suitable habitat (%)" = "Loss suitable habitat",
                              " Mean species richness (Species p. grid cell)" = "Mean species richness",
                              " Potentially disappeared fraction of species (PDF)" = "Pot. disappeared fraction of species", 
                              " Pressure-based natural capital index (NCI-pb)" = "Pressure-based natural capital index",
                              " Species affected by 50% range loss (%)" = "Species affected by 50% range loss",
                              " Reduction vascular plant species (%)" = "Reduction vascular plant species",
                              " Species range protection level (%)" = "Species range protection level")) +
  labs(fill = "", y = "Studies [%]") +
  geom_text(data = metrics, aes(x = indicator, y = Percentage_Indicators + 2.5), label = metrics$studies, fontface = "bold", color="black",alpha=0.6, size = 16/.pt, inherit.aes = FALSE ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 16, angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        panel.grid = element_blank(),
        legend.position = c("none"), 
        legend.justification = "left",
        legend.text = element_text(size = 16),
        legend.key.size = unit(1.5, "line"),
        plot.margin = margin(0, 0, 0, 120)) 
  #plot.margin = unit(rep(-0.1, 10), "cm")) +
histogram

######SDG
#7.) Final plot (Venn diagram + circular barplot)####
###
#pdf(paste(figpath, "Impact_gap.pdf"), width = 14, height = 20)
pdf(paste(figpath, "Figure_2.pdf"), width = 14, height = 14)

final <- (wrap_elements(Scenarios_plot) + wrap_elements(Studies_plot))/histogram +
  plot_layout(heights = c(0.5, 0.4)) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(
    #plot.tag.position = c(0.05, 0.8),
    plot.tag = element_text(size = 18, hjust = 1, vjust = 1),
    plot.margin = margin(10, 0, 0, 60))
final

dev.off()

