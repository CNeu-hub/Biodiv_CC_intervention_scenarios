###
###Function to calculate jaccard similarities for interventions among scenarios, plot them as a heatmap and create hierarchical clusters based on jaccard distance using maximum (complete) linkage method 
###

#input_data = data input, can be a data.frame consisting of binary data for groups (interventions), each column represents vector data if a group is applied (1) or not (0)
#similarity_threshold = Numerical, if you want to retain only interventions that have a jaccard_similarity > a specific threshold
#heatmap = Logical, should a heatmap be ploted with an overview of all interventions and their jaccard similarities? 
#hclustering = Logical, should a hierarchical cluster algorithm be performed - or not? 
#cluster_height = Numerical, which height should be used to cut the hierarchical cluster tree for visualizing the corresponding clusters

calculate_jaccard <- function(input_data, similarity_threshold = 0.7, hclustering = TRUE, cluster_height = 0.5) {
  
  library(tidyverse) 
  library(reshape2)
  
  #note: Equation for jaccard similarity used for binary data: 
  #J(A, B) = a_11 / (a_11 + b_01 + c_10)
  
  #create function to calculate jaccard similarity 
  jaccard_similarity <- function(x, y) {
    
    intersect <- sum(x & y)  #|A ∩ B| --> both groups applied (1, 1)
    union <- sum(x | y)      #|A ∪ B| --> at least one group was applied (0, 1 | 1, 0)
    return(intersect / union)
    
    }
  
  #create a similarity matrix for groups (pairwise comparisons)
  interventions <- colnames(input_data)
  no_inter <- ncol(input_data)
  jaccard_matrix <- matrix(0, nrow = no_inter, ncol = no_inter,
                           dimnames = list(interventions, interventions))
  
  #calculate jaccard similarity for matrix comparisons 
  for (i in 1:no_inter) {
    for (j in i:no_inter) {
      
      jaccard_matrix[i, j] <- jaccard_similarity(input_data[, i], input_data[, j])
      jaccard_matrix[j, i] <- jaccard_matrix[i, j]
      
    }
  }

  #heatmap
  #convert to long format
  jaccard_long <- melt(jaccard_matrix)
  colnames(jaccard_long) <- c("Intervention_a", "Intervention_b", "Jaccard")
  
  no_scenarios <- nrow(input_data)

  #plot heatmap
  jac_plot <- ggplot(jaccard_long, aes(x = factor(Intervention_a), y = factor(Intervention_b), fill = Jaccard)) +
    geom_tile() + 
    theme_minimal() +
    labs(fill = paste("Jaccard similarity of interventions", " (n = ", no_scenarios, "scenarios)"), y = "Intervention a", x = "Intervention b") +
    geom_text(aes(label = paste0(round(Jaccard, digits = 2))), size = 14/.pt, color = ifelse(jaccard_long$Jaccard < 0.25, "black", "white")) + # fontface = ifelse(!is.na(test3$P.Value) & test3$P.Value < 0.05, "bold", "plain")) +
    scale_fill_gradient2(midpoint = 0) +
    scale_y_discrete(labels = function(x) gsub("_", " " , x)) +
    scale_x_discrete(labels = function(x) gsub("_", " " , x)) +
    theme_classic()+
    theme(legend.position = "top",
          legend.title.position = "left",
          #plot.margin = margin(20),
          plot.title = element_blank(),
          legend.justification = "left",
          # legend.text = element_text(size=14),
          legend.title = element_text(size=14,face="bold", hjust = 0, vjust = 0.5),
          axis.ticks = element_blank(), 
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(), 
          axis.line = element_blank(),
          axis.text.y = element_text(size=14, hjust = 1),
          axis.text.x = element_text(size=14, angle = 90, vjust = 1, hjust = 1)) 
  
print(jac_plot)

  jaccard_subset <- subset(jaccard_long, jaccard_long$Jaccard > similarity_threshold & jaccard_long$Jaccard != 1)
  print(paste(n_distinct(jaccard_subset$Intervention_a), "unique interventions with jaccard similarity >", similarity_threshold))
  print(unique(jaccard_subset$Intervention_b))
  
  if(hclustering == TRUE) {
    
    #create cluster
    hc <- hclust(as.dist(1 - jaccard_matrix), method = "complete")#method = "ward.D2")
    plot(hc)
    abline(h = cluster_height) #draw line at level 0.5
    
    #cut the dendrogram into clusters
    clusters <- data.frame(cutree(hc, h = cluster_height))  #use h (height), to cut the tree based on height = 0.5 (interventions are exactly as similar to one another as they are dissimilar, so if we look at the clusters of samples below 0.5 – i.e., (B,F), (A,E,C,G) and (D) – within each cluster the samples have more than 50% similarity, more than 50% co-presences of species.
    print(clusters)
    
    final_clusters <- clusters %>%
      mutate(Intervention = rownames(.)) %>%
      rename(Cluster = cutree.hc..h...cluster_height.) %>%
      group_by(Cluster) %>%
      summarise(Intervention = Intervention) %>%
      mutate(No_Interventions = n_distinct(Intervention)) %>%
      ungroup()
    
    cols <- colorRampPalette(brewer.pal(9, "Set3"))(nrow(final_clusters))
    
    print(ggplot(data = final_clusters, aes(x = No_Interventions/No_Interventions, y = as.factor(Cluster), fill = Intervention)) +
      geom_bar(stat = "identity") + 
      labs(x = "Number of interventions in cluster", y = "Cluster", fill = "Intervention", title = paste("Intervention clusters based on Jaccard distance with height <", cluster_height)) +
      scale_fill_manual(values = cols))
    
  } else {
    
    print("No hierarchical clustering performed")
    
  }
  
  jaccard <- list(Jaccard_matrix = jaccard_matrix, Plot = jac_plot, Jaccard_subset = jaccard_subset, Final_clusters = ifelse(hclustering == TRUE, final_clusters, NA))
  
  return(jaccard)
  
}
