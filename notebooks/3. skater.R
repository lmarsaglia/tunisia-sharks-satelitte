library(spdep)
library(cluster)

data_sf = readRDS(here("data", "data_sf.RData"))

# Clustering SKATER 
df_sf <- st_as_sf(data_sf, crs = wgs)%>% 
 filter(!is.na(id))

df_sf$n_detections[is.na(df_sf$n_detections)] <- 0

data_sp = df_sf[,c("id", "x")]

data_sp <- data_sp%>%  
  filter(!is.na(id)) %>%
  as_Spatial() 

data_ict <- df_sf[,c("n_detections")] %>% 
  st_drop_geometry()
# neighbour list
data.nb <- poly2nb(data_sp)
summary(data.nb)

plot(data_sp, border=gray(.5))
{plot(data.nb, coordinates(data_sp), col="blue", add=TRUE)}

# edge costs (pairwise dissimilarity between the values on the variables
# and the values for the neighbouring observation)
# = generalised weight for a spatial weight matrix

lcosts <- nbcosts(data.nb, data_ict)

data.w <- nb2listw(data.nb, lcosts, style="B")
summary(data.w)

# minimum spanning tree
data.mst <- mstree(data.w)
head(data.mst)

{plot(data_sp, border=gray(.5))}
{plot.mst(data.mst, coordinates(data_sp), 
          col="blue", cex.lab=0.2, cex.circles=0.00005, add=TRUE)}

# Initialize a list to store mean silhouette widths
silhouette_results <- list()

# silhouette loop
for (k in 6:10) {
  # Perform clustering
  skater_result <- skater(data.mst[, 1:2], data_ict, method = "euclidean", k)
  
  # Extract groups
  groups <- skater_result$groups
  
  # Add groups to data_ict
  data_ict$groups <- groups
  
  # Standardize the data
  #df_var <- decostand(data_ict, method = 'range', margin = 2)
  
  # Calculate distance matrix
  dist_matrix <- dist(df_sf$n_detections)
  
  # Calculate silhouette
  sil <- silhouette(data_ict$groups, dist_matrix)
  
  # Calculate mean silhouette width
  mean_silhouette_width <- mean(sil[, 3])
  
  silhouette_results[[as.character(k)]] <- mean_silhouette_width
}

# Convert results to a data frame
silhouette_df <- data.frame(clusters = 6:10, mean_silhouette_width = unlist(silhouette_results))

# Print the results
plot(silhouette_df)

# choose first lowest value in silhouette plot as number of cluster and plot skater function
skater <- skater(data.mst[,1:2], data_ict, method = "euclidean", p=4)
str(skater)

groups <- skater$groups



{plot(data_sp, border=gray(.5))}
{plot(skater, coordinates(data_sp), cex.lab=.2,
      groups.colors=c("blue", "brown", "pink","yellow","green","orange","cyan","grey","black"), cex.circles=0.00005, add=TRUE)}

#plot skater groups
write_rds(df_sf,here('data','df_w_cluster.RData'))

df_sf$skate_groups<- groups


df_sf %>% 
ggplot()+
  geom_sf(aes(fill = as.factor(skate_groups)))+
  scale_fill_manual(values = c("blue", "brown", "pink","yellow","green","orange","cyan","grey","black","purple","tan" ), name = "My name", 
                    guide = guide_legend(reverse = TRUE))+
  coord_sf(datum = NA)+
  theme(legend.position="right",
        plot.title = element_text(hjust = 0.5,color = "Gray40", size = 16, face = "bold"),
        plot.subtitle = element_text(color = "blue"),
        plot.caption = element_text(color = "Gray60"))+ 
  guides(fill = guide_legend(title = "Unit: 1000", title.position = "bottom", title.theme =element_text(size = 10, face = "bold",colour = "gray70",angle = 0)))


ggsave( filename = here::here('figures', "skater_by_0.1_grid.png"),width = 30, height = 20, units = "cm")

