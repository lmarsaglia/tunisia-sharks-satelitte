
port_otb=readRDS(here('data','port_df_otb.Rdata'))
df_w_cluster=readRDS(here('data','df_w_cluster.RData'))

#transform datasets in spatial to calculate distance
wgs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

port_otb_sf <- st_as_sf(port_otb, 
                     coords = c("lon", "lat"), 
                     crs = wgs)

st_crs(port_otb_sf) = wgs

cluster_sf = df_w_cluster[,c('id','centroid','skate_groups')] %>% 
  group_by(centroid,skate_groups,id) %>%
  summarise()

distance_matrix <- st_distance(cluster_sf, port_otb_sf)

distance_df <- as.data.frame(distance_matrix)

rownames(distance_df) <- cluster_sf$id
colnames(distance_df) <- port_otb_sf$port_name

distance_long <- distance_df %>%
  tibble::rownames_to_column(var = "id") %>%
  tidyr::pivot_longer(-id, names_to = "port_name", values_to = "distance")

print(distance_long)
distance_long$distance = as.numeric(distance_long$distance)

##
# Define the port
port <- distance_long$port_name

# Define fishing grounds
grounds <- distance_long$id

distances = distance_long$distance
# Gravity model function
compute_probabilities <- function(distances, b) {
  numerators <- 1 / distances^b
  probabilities <- numerators / rowSums(numerators)
  return(probabilities)
}

df_long <- df %>%
  pivot_longer(cols = -id, names_to = "port", values_to = "probability")

# Display the result

# Compute probabilities for each fishing ground from each port
distances = as.matrix(distance_df)
probabilities <- compute_probabilities(distances, b)

# Define an empty data frame to store the results
df <- as.data.frame(probabilities) 
# Display the result
df_long_probability <- df %>%
  tibble::rownames_to_column(var = "id") %>%
  pivot_longer(cols = -id, names_to = "port", values_to = "probability")

df_long_probability = left_join(df_long_probability, df_sf) %>%
  dplyr::select(id,port,probability,skate_groups,x) %>% st_as_sf()

df_long_probability %>% 
  filter(probability>0.01,skate_groups!=1,skate_groups!=7,skate_groups!=8) %>%
  ggplot()+
  geom_sf(aes(color = factor(skate_groups)), size = 0.5) +
  scale_color_manual(
    values = c("blue", "brown", 
               "pink","yellow","green",
               "orange","cyan","grey","black",
               "purple","tan"), # Define the colors for skater_group
    guide = guide_legend(title = "Skater Group"))+
  
  ggnewscale::new_scale_fill() +
  geom_sf(aes(fill = probability),color=NA)+
  scale_fill_gradient(  
    guide = guide_legend(reverse = TRUE),limits = c(0, 0.5), oob = scales::squish)+
  coord_sf(datum = NA) +
  theme(legend.position="right",
        plot.title = element_text(hjust = 0.5,color = "Gray40", size = 16, face = "bold"),
        plot.subtitle = element_text(color = "blue"),
        plot.caption = element_text(color = "Gray60"))+ 
  guides(fill = guide_legend(title = "probability", 
                             title.position = "bottom", 
                             title.theme =element_text(size = 10, face = "bold",
                                                       colour = "gray70",angle = 0))) +
  facet_wrap(~port)

ggsave( filename = here::here('figures', "probability_cluster.png"),width = 30, height = 20, units = "cm")


#####assign port to fishing ground

aissigned = df_long_probability %>% 
  filter(probability>0.5) %>%
  group_by(skate_groups) %>%
  summarise(summarized_ports = paste(unique(port), 
            collapse = ", ")) %>%
  filter(skate_groups!=1,skate_groups!=7,skate_groups!=8)



