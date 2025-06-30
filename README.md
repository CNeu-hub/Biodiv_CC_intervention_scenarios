# Code repository for a meta-analysis of global climate change mitigation and biodiversity restoration intervention scenarios

### Author:

Christian Neumann 30-06-2025

### Short description:

This is the project repository to perform the analysis of: 

*Neumann, C., Alkemade, R., Van Vuuren, D., Burian, A., Aschi, F., Seppelt, R. (2025). Trade-offs and synergies between climate change mitigation and biodiversity restoration: A meta-analysis of global intervention scenarios*

The provided code performs a descriptive content and impact analysis using the [intervention scenarios database](https://zenodo.org/records/15753209) of the [Transpath project](https://www.transpath.eu/). The database collects a suite of global model-based policy-screening/target-seeking and policy-oriented exploratory scenarios aiming to mitigate climate change and/or restoring/conserving biodiversity. It includes qualitative and quantitative assumptions underlying the scenarios, accompanied by information about the applied interventions, and impacts of the scenarios.

A detailed description of the database can be found in: 

*Neumann, C., Alkemade, R., Van Vuuren, D., Seppelt, R. (2025). Global Assessment of Biodiversity-Climate Pathways [Submitted to European Union].*

The preprocessing of the database for the upload on Zenodo and described in the deliverable is outlined in the according [GitHub repository](https://github.com/CNeu-hub/Transpath_scenarios_database).

### Usage:

All scripts can be executed from scratch after cloning the repository. Folder description is provided below.

See the session info below for the R packages needed to execute the scripts.

### Structure:

The repository includes five folders:

**1. Analysis:**

Contains all the code needed to perform the analysis. Includes scripts to calculate relative frequencies of measured impacts, applied interventions (Gap folder) (Figures 1/2). Furthermore, in the Impact folder, it contains the code to plot the win-win/lose-lose-style plots of interventions impact on biodiversity and climate (Figure 3).
It also contains the code to prepare the data for the random forest modelling, the random forest modelling script (Figure 4, supplemental figure S7), and a script to plot harmonized inmpacts on biodiversity/climate in a descriptive way using boxplots to create the plot in the supplementary material (Supplemental figure S3). 

**2. Functions:**

Contains some functions used to preprocess as well as work with the database and the function used to calculate the Jaccard similarities of interventions. 

**3. Input:**

Contains the data input needed to perform the analysis and a list of reference scenarios as well as an interventions lookup list to work with the data. 

**4. Output:**

Contains the output of the analysis (Figures/Tables).

**5. Preprocessing:**

Contains the preprocessing scripts to calculate z-score transformed absolute changes from the reference scenarios for each scenario (using the reference lookup table) 

### R session info:

```
utils::sessionInfo()

R version 4.5.1 (2025-06-13)
Platform: aarch64-apple-darwin20
Running under: macOS Sequoia 15.5

Matrix products: default
BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
LAPACK: /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.1

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

time zone: Europe/Berlin
tzcode source: internal

attached base packages:
[1] grid      stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] gridExtra_2.3      gridtext_0.1.5     shapviz_0.9.7      treeshap_0.3.1     ranger_0.17.0      reshape2_1.4.4    
 [7] igraph_2.1.4       ggtext_0.1.2       eulerr_7.0.2       colorspace_2.1-1   patchwork_1.3.0    ggpubr_0.6.0      
[13] ggh4x_0.3.0        tm_0.7-16          NLP_0.3-2          wordcloud_2.6      RColorBrewer_1.1-3 lubridate_1.9.4   
[19] forcats_1.0.0      stringr_1.5.1      dplyr_1.1.4        purrr_1.0.4        readr_2.1.5        tidyr_1.3.1       
[25] tibble_3.2.1       ggplot2_3.5.2      tidyverse_2.0.0    readxl_1.4.5      

loaded via a namespace (and not attached):
 [1] gtable_0.3.6      xfun_0.52         htmlwidgets_1.6.4 rstatix_0.7.2     lattice_0.22-7    tzdb_0.5.0       
 [7] vctrs_0.6.5       tools_4.5.1       generics_0.1.3    parallel_4.5.1    pkgconfig_2.0.3   Matrix_1.7-3     
[13] data.table_1.17.0 checkmate_2.3.2   polylabelr_0.3.0  lifecycle_1.0.4   compiler_4.5.1    farver_2.1.2     
[19] carData_3.0-5     litedown_0.7      htmltools_0.5.8.1 htmlTable_2.4.3   Formula_1.2-5     pillar_1.10.2    
[25] car_3.1-3         crayon_1.5.3      abind_1.4-8       commonmark_1.9.5  tidyselect_1.2.1  digest_0.6.37    
[31] stringi_1.8.7     slam_0.1-55       labeling_0.4.3    polyclip_1.10-7   fastmap_1.2.0     cli_3.6.5        
[37] magrittr_2.0.3    broom_1.0.8       withr_3.0.2       scales_1.4.0      backports_1.5.0   xgboost_1.7.10.1 
[43] timechange_0.3.0  rmarkdown_2.29    ggsignif_0.6.4    cellranger_1.1.0  hms_1.1.3         evaluate_1.0.3   
[49] knitr_1.50        viridisLite_0.4.2 markdown_2.0      rlang_1.1.6       Rcpp_1.0.14       glue_1.8.0       
[55] xml2_1.3.8        jsonlite_2.0.0    rstudioapi_0.17.1 plyr_1.8.9        R6_2.6.1

```
