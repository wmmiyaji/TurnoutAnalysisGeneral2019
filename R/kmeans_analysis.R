
source("./R/read_in_data.R")

NUM_CLUSTERS <- 6

clusters_prep  <- MCDC_Results_Municipal_2019_Turnout_long_filled %>% 
  select(District, Hour, Diff) %>% filter(Hour != 6) %>% 
  mutate(Hour = paste0("H", Hour)) %>% 
  group_by(District) %>% 
  mutate(District_Total = sum(Diff), 
         Percent_2_Hour = round(100 * Diff/District_Total,2) ) %>% 
  select(-Diff, -District_Total) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Hour, values_from = Percent_2_Hour)

set.seed(41)
cluster_results <-   kmeans(clusters_prep %>% 
                              select(-District)  , centers = NUM_CLUSTERS, iter.max=500 )

clusters_data  <- clusters_prep  %>% cbind(cluster_results$cluster) %>% 
  rename(Cluster = "cluster_results$cluster") %>% 
  pivot_longer(cols = starts_with("H"), names_to = "Hour", values_to = "Percent_2_Hour") %>% 
  mutate(Hour = str_replace(Hour, "H", ""), 
         Hour = as.numeric(Hour)) 

cluster_names <- data.frame(Cluster = 1:6, Cluster_Name = c("Mid Morning Peak",  "In the Afternoon",
"Steady", "An Exception", "Afternoon Peak", "On Commute Home") )

clusters_data <- clusters_data %>% left_join(cluster_names) 

clusters_data %>% 
  ggplot(aes(x= as.factor(Hour), y = Percent_2_Hour, color = as.factor(Cluster_Name))) + geom_boxplot() + 
  facet_wrap(~Cluster_Name, ncol = 2) +
  labs(title = "Turn Out Patterns", x = "Two Hour Intervals") + 
  scale_y_continuous(limits = c(0,40)) + 
  theme_bw() + 
  theme(legend.position = "none")


