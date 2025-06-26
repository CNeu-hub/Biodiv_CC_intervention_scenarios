###---------------------------------------------------------------------------###
###                Random forest modelling of climate/biodiversity impacts    ###
###                Author: Christian Neumann                                  ###
###                Date: 31st May 2024                                        ###
###---------------------------------------------------------------------------###

###
#1.) Create environment####
###

#load libraries
library(tidyverse)
library(ranger)
library(treeshap)
library(shapviz)
library(reshape2)
library(ggpubr)
library(patchwork)
library(gridtext)
library(gridExtra)
library(grid)

#define output/input paths
figpath <- paste(getwd(), "/Output/Figures/", sep = "")
tablepath <- paste(getwd(), "/Output/Tables/", sep = "")
datapath <- paste(getwd(), "/Input/", sep = "")

#load data
input_data <- readRDS(paste(datapath, "RF_input_data_10_06_25_weighted.rds", sep = ""))

#extract separate prepared biodiv and cc datasets
data_biodiv <- input_data[[1]]
data_cc <- input_data[[2]]

###
#2.) Biodiversity random forest model####
###

#create weighting for each scenario (that each scenario is weighted equally even though it has 7 observations because results are available for 7 indicators)
data_biodiv$Weights <- 1/table(data_biodiv$Index)[as.character(data_biodiv$Index)]

#create number of interventions categories
data_biodiv$Interventions_number <- cut(data_biodiv$Interventions_number, breaks = c(0, 5, 10, 20), labels = c(0, 0.5, 1))

#create formula for predictors and confounding factors
form <- as.formula(paste0("Biodiversity_impact~",  paste(colnames(data_biodiv[5:16]), collapse="+"), " + Cluster_1 + Cluster_2 + Interventions_number + Model + Biodiversity_model + Study_nr. + Biodiversity_indicator"))
print(form)

#convert "random effects" (i.e. confounding factors) as factor, because if character, ranger.unify function will encounter a fatal error to R
data_biodiv$Model <- as.factor(data_biodiv$Model)
data_biodiv$Biodiversity_model <- as.factor(data_biodiv$Biodiversity_model)
data_biodiv$Biodiversity_indicator <- as.factor(data_biodiv$Biodiversity_indicator)
data_biodiv$Study_nr. <- as.factor(data_biodiv$Study_nr.)

#run random forest with ranger package (fast implementation of random forest algorithm by Leo Breiman)
#max.depth = Controls the maximum depth of the tree. A deeper tree will be able to learn more complex relationships between the features and the target variable, but it is also more likely to overfit the training data
#num.trees = Number of trees to build (default = 500)
#mtry = Number of variables to try at each split (default to m = sqrt(p))
#min.node.size = Minimum number of observations in each node (default = 5)
#set.seed to ensure reproducibility
set.seed(123)
rf <- ranger::ranger(form, data = data_biodiv, max.depth = 10, num.trees = 1000, mtry = 7,
                     case.weights = data_biodiv$Weights) #balance contribution of observations because of pseudoreplication per scenario)

sink(paste(tablepath, "Biodiv_RF_output.txt", sep = ""))
print(rf)
sink()

#unify model, afterwards compute shapely values using unify model, afterwards compute shapely values using treeshap package
unified_model <- ranger.unify(rf, data_biodiv)
shaps <- treeshap(unified_model, data_biodiv, interactions = FALSE)#, interactions = TRUE)

#plot feature importance using shapviz package
sh <- shapviz(shaps)

shap_biodiv_bee <- sv_importance(sh, kind = "beeswarm", viridis_args = list(option = "inferno", begin = 0.25, end = 0.85, limits = c(0, 1))) +
  theme_classic() + 
  # scale_color_viridis_c(option = "inferno", begin = 0.25, end = 0.85, limits = c(0, 1)) +
  scale_y_discrete(labels = function(x) gsub("_", " " , x)) +
  labs(color = "", x = "Variable impact on prediction [SHAP value]") +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  theme(#aspect.ratio = 0.75,
    #axis.text.y = element_text(size=14,hjust=1),
    axis.text.y = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0),size=14,face="bold"),
    legend.position = "bottom", legend.title = element_text(size = 14, face = "bold"), legend.text = element_text(size = 14),
    axis.text.x = element_text(size=14),
    axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),size=14,face="bold"),
    plot.title = element_text(face = "bold", hjust = 0.5,size=14)) 

shap_biodiv_bee

interventions <- c(colnames(data_biodiv[5:16]), "Cluster_1", "Cluster_2")

shap_biodiv_bee$data$color <- ifelse(shap_biodiv_bee$data$feature %in% interventions, 
                                     ifelse(shap_biodiv_bee$data$color == 1, "Intervention applied", "Intervention not applied"),
                                     "Confounding factor")

shap_biodiv_bee <- shap_biodiv_bee +  
  scale_color_manual(values = c("Intervention applied" = "#FBBE22FF", 
                                "Intervention not applied" = "#56106EFF",
                                "Confounding factor" = "grey40"))

shap_biodiv_bee

shap_biodiv_bar <- sv_importance(sh, kind = "bar", show_numbers = TRUE, number_size = 14/.pt, fill = "#DDDDDD") +
  #scale_x_reverse() +
  scale_y_discrete(labels = function(x) gsub("_", " " , x)) +
  labs(x = "Variable importance [Mean absolute SHAP value]") +
  theme_classic() +
  theme(#aspect.ratio = 0.75,
    legend.position = "right", legend.title = element_text(size = 14, face = "bold"), legend.text = element_text(size = 14),
    axis.text.y = element_text(size=14,hjust=1),
    axis.text.x = element_text(size = 14),
    #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0),size=14,face="bold"),
    #axis.title.x = element_blank(),
    #axis.line.x = element_blank(),
    #axis.ticks.x = element_blank(),
    axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),size=14,face="bold"),
    plot.title = element_text(face = "bold", hjust = 0.5,size=14))

shap_biodiv_bar

shap_biodiv_bar|shap_biodiv_bee 

pdf(paste(figpath, "Shap_Biodiversity_weighted.pdf", sep = ""), height = 6, width = 16)

a <- (shap_biodiv_bar|shap_biodiv_bee) +
  plot_layout(guides = "collect") +
  plot_annotation(title = "Biodiversity model") &
  theme(
    legend.position = "bottom",
    legend.justification = "left",
    legend.box = "vertical",
    plot.tag = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold")
  )
a
dev.off()

###
#3.) Climate random forest model####
###

#create weighting for each scenario (that each scenario is weighted equally even though it has 7 observations because results are available for 7 indicators)
data_cc$Weights <- 1/table(data_cc$Index)[as.character(data_cc$Index)]

#create number of interventions categories
data_cc$Interventions_number <- cut(data_cc$Interventions_number, breaks = c(0, 5, 10, 20), labels = c(0, 0.5, 1))

#invert climate impact, that impact goes in the same direction as for biodiversity model (positive = positive impact, negative = negative impact i.e. increasing emissions e.g.)
data_cc$Climate_impact <- data_cc$Climate_impact*-1

#create formula for input factors 
form <- as.formula(paste0("Climate_impact~",  paste(colnames(data_cc[4:27]), collapse="+"), "+ Cluster_1 + Cluster_3 +  Interventions_number + Model + Study_nr. + Climate_indicator"))
print(form)

#convert "random effects" as factor, because if character, ranger.unify will encounter a fatal error to R
data_cc$Model <- as.factor(data_cc$Model)
data_cc$Study_nr. <- as.factor(data_cc$Study_nr.)
data_cc$Climate_indicator <- as.factor(data_cc$Climate_indicator)

#run random forest
#set.seed to ensure reproducibility
set.seed(456)
rf <- ranger::ranger(form, data = data_cc, max.depth = 10, num.trees = 1000, mtry = 9,
                     case.weights = data_cc$Weights) #balance contribution of observations because of pseudoreplication per scenario)

sink(paste(tablepath, "CC_RF_output.txt", sep = ""))
print(rf)
sink()

# compute shapely values
unified_model <- ranger.unify(rf, data_cc)
shaps <- treeshap(unified_model, data_cc, interactions = FALSE)

#plot feature importance using shapviz package
sh <- shapviz(shaps)

shap_cc_bee <- sv_importance(sh, kind = "beeswarm", viridis_args = list(option = "inferno", begin = 0.25, end = 0.85, limits = c(0, 1))) +
  theme_classic() + 
  # scale_color_viridis_c(option = "inferno", begin = 0.25, end = 0.85, limits = c(0, 1)) +
  scale_y_discrete(labels = function(x) gsub("_", " " , x)) +
  labs(color = "", x = "Variable impact on prediction [SHAP value]") +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  theme(#aspect.ratio = 0.75,
    #axis.text.y = element_text(size=14,hjust=1),
    axis.text.y = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0),size=14,face="bold"),
    legend.position = "right", 
    legend.title = element_text(size = 14, face = "bold"), legend.text = element_text(size = 14),
    axis.text.x = element_text(size=14),
    axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),size=14,face="bold"),
    plot.title = element_text(face = "bold", hjust = 0.5,size=14)) 

shap_cc_bee

interventions <- c(colnames(data_cc[4:27]), "Cluster_1", "Cluster_3")

shap_cc_bee$data$color <- ifelse(shap_cc_bee$data$feature %in% interventions, 
                                 ifelse(shap_cc_bee$data$color == 1, "Intervention applied", "Intervention not applied"),
                                 "Confounding factor")

shap_cc_bee <- shap_cc_bee +  
  scale_color_manual(values = c("Intervention applied" = "#FBBE22FF", 
                                "Intervention not applied" = "#56106EFF",
                                "Confounding factor" = "grey40"))

shap_cc_bee

shap_cc_bar <- sv_importance(sh, kind = "bar", show_numbers = TRUE, number_size = 14/.pt, fill = "#DDDDDD") +
  #scale_x_reverse() +
  scale_y_discrete(labels = function(x) gsub("_", " " , x)) +
  labs(x = "Variable importance [Mean absolute SHAP value]") +
  theme_classic() +
  theme(#aspect.ratio = 0.75,
    legend.position = "right", legend.title = element_text(size = 14, face = "bold"), legend.text = element_text(size = 14),
    axis.text.y = element_text(size=14,hjust=1),
    axis.text.x = element_text(size = 14),
    #axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0),size=14,face="bold"),
    #axis.title.x = element_blank(),
    #axis.line.x = element_blank(),
    #axis.ticks.x = element_blank(),
    axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),size=14,face="bold"),
    plot.title = element_text(face = "bold", hjust = 0.5,size=14))

shap_cc_bar

shap_cc_bar|shap_cc_bee

pdf(paste(figpath, "Shap_ClimateChange_weighted.pdf", sep = ""), height = 6, width = 16)

b <- (shap_cc_bar|shap_cc_bee) +
  plot_annotation(title = "Climate change model") &
  theme(
    legend.position = "none",
    legend.justification = "left",
    legend.box = "vertical",
    plot.tag = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold"))
b

dev.off()

###
#4.) Final plot####
###

#HTML/Markdown-formatted cluster text with bold headers
cluster_text <- paste0(
  "<b>Cluster 1:</b> Energy lifestyle changes, Energy technology and efficiency, Renewable energy,<br>", "Building technology and efficiency, Energy constraints, Fossil fuel phase out<br>",
  "<b>Cluster 3:</b> Traditional bioenergy phase out, Energy taxes"
)

#create a rich text grob from cluster text, for plotting with gridExtra package
table_grob <- richtext_grob(
  cluster_text,
  gp = gpar(fontsize = 14), x = 0.3, y = 0.5, hjust = 0
)

pdf(paste(figpath, "SHAP_cc_biodiv_weighted.pdf", sep = ""), height = 12, width = 13)
wrap_elements(b)/wrap_elements(a)/table_grob +
  plot_layout(heights = c(1, 1, 0.11)) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") 

dev.off()

sh$X$Climate_indicator <- case_when(sh$X$Climate_indicator == "SDG13_AFOLU_emissions_(Mt_CO2eq/yr)" ~ "AFOLU emissions",
                                    sh$X$Climate_indicator == "SDG13_Total_GHG_emissions_(Mt_CO2eq/yr)" ~ "Total GHG emissions",
                                    sh$X$Climate_indicator == "SDG13_Total_CO2_emissions_(Mt_CO2/yr)" ~ "Total CO2 emissions",
                                    sh$X$Climate_indicator == "SDG13_Forcing_(W/m_2)" ~ "Forcing",
                                    sh$X$Climate_indicator == "SDG13_CO2_concentration_(ppm)" ~ "CO2 concentration",
                                    sh$X$Climate_indicator == "SDG13_Temperature_change_(Since_pre_industrial_age)" ~ "Temperature change",
                                    sh$X$Climate_indicator == "SDG13_AFOLU_CO2_emissions_(Mt_CO2/yr)" ~ "AFOLU CO2 emissions")

cols <- brewer.pal(7, "Dark2")
RColorBrewer::display.brewer.pal(7, "Dark2")
#cols = c("red", "red", rep("grey", 5))

#final SHAP dependence plot (for SI)
pdf(paste(figpath, "Climate_indicator_StudyNumber_SHAP_dependence.pdf", sep = ""), width = 12, height = 12)
dependence <- sv_dependence(sh, v = c("Study_nr."), color_var = c("Climate_indicator"), interactions = F, viridis_args = list(option = "turbo")) +
  labs(color = "") +
  scale_color_brewer(palette = "Dark2") +
  theme_classic() +
  guides(color = guide_legend(override.aes = list(size = 3), ncol = 3)) +
  plot_layout(nrow = 2, ncol = 1, guides = "collect") &
  theme(legend.position = "bottom",  axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.justification = "left",
        axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 14, face = "bold"), 
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)) 
dependence
dev.off()

pdf(paste(figpath, "Climate_indicator_Model_SHAP_dependence.pdf", sep = ""), width = 18, height = 16)
dependence2 <- sv_dependence(sh, v = c("Model"), color_var = c("Climate_indicator"), interactions = F, viridis_args = list(option = "turbo")) +
  labs(color = "Climate indicator") +
  scale_color_brewer(palette = "Dark2") +
  theme_classic() +
  guides(color = guide_legend(override.aes = list(size = 3), ncol = 7)) +
  plot_layout(nrow = 2, ncol = 1, guides = "collect") &
  theme(legend.position = "bottom",  
        axis.text.x = element_text(angle = 90, hjust = 1, size = 14),
        legend.justification = "left",
        axis.text.y = element_text(size = 14), 
        axis.title = element_text(size = 14, face = "bold"), 
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 14)) 
dependence2
dev.off()
