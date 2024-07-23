landings = elasmo_landings_all %>% 
  dplyr::select(Port,month,year,QR,ESPECE)

landing_cluster <- landings %>%
  mutate(
    f_ground = case_when(
      Port %in% c("La Goulette", "Sousse", "Kélibia", "Bizerte", "Tabarka") ~ 4,
      Port%in%c("Chebba", "Mahdia", "Monastir", "Sousse","Sayada","Teboulba") ~ 6,
      Port%in%c("Chebba","Mahdia", "Monastir", "Sousse","Sayada","Teboulba") ~ 5,
      Port %in% c("Chebba","Mahdia", "Monastir", "Sousse","Sayada","Teboulba") ~ 3,
      Port %in% c("Zarzis", "Sfax") ~ 2,
      TRUE ~ NA_integer_  # Default case
    )
  )


#DETCTIONS DF PREP
df_clusters=read_rds(here::here("data","df_w_cluster.RData"))

good_points$detection=1

detections=good_points %>%
  dplyr:: select(id,month,year,detection) %>%
  filter(!is.na(id)) %>%
  group_by(id,month,year) %>%
        summarise(n_detections=sum(detection)) %>%
        sf::st_drop_geometry() 

detections=detections %>% 
  left_join(df_sf,by=c("id"='id')) %>%
  dplyr::select(id, month,year,n_detections.x,skate_groups)

detections_group = detections %>%
  group_by(month,year,skate_groups) %>%
  summarise(n_detections=sum(n_detections.x))


#Merge w LANDING 
landing_cluster_grouped = landing_cluster %>%
  group_by(year,month,f_ground,ESPECE) %>%
  summarise(landing=sum(QR))

detections_landing = detections_group %>%
  filter(skate_groups!=1,
          skate_groups!=7,
          skate_groups!=8,
         ) %>%
 left_join(landing_cluster_grouped,
            by = c("year"="year", 
                   "month"= "month",
                   "skate_groups"="f_ground")) %>% 
  filter(year!='2020') %>% 
  na.omit() 
  
results = lm(data = detections_landing, landing ~n_detections + as.factor(skate_groups)  + as.factor(ESPECE) + year + month)

summary(results)


my_palette <- c("#E41A1C", "#900C3F", "#4DAF4A", "#FF7F00", "#FDBF6F","#8B4513",
                "#D98880", "#984EA3", "#999999", "#FFD700"
                ,"#377EB8", "#34495E")
p = ggplot() +
  geom_point(data =detections_landing, 
             aes(detections_landing$landing, 
                 results$fitted.values,
                 col=as.factor(detections_landing$skate_groups),
                 shape=ESPECE),alpha=0.8,size=4) +  
  scale_color_manual(values = my_palette) +
  labs(title = "",
       x = "Observed",
       y = "Predicted",
       color='Cluster') +
  theme_minimal()
p
ggsave(filename = here::here('figures', "lm_res.png"),width = 30, height = 20, units = "cm")


