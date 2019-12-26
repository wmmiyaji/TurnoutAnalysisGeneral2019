library(tidyverse)

clusters_by_muni <- clusters_data %>% 
  mutate(District = str_replace(District, "Ward", "ward"),
         District = str_replace(District, "District", "voting district"),
         District = str_replace(District, "Town", "town"), 
         District = str_replace(District, "Boro", "boro"),
         District = str_replace(District, "Parsippany - Troy Hills", "Parsippany-Troy Hills"), 
         NAME10 = District) %>% 
  rowwise() %>% 
  mutate(Municipality = ifelse(str_detect(District, "ward"), 
                               substr(District,  1, str_locate(District, "ward")-2), 
                               substr(District,  1, str_locate(District, "voting")-2)))

cluster_names_list =c( "Afternoon Peak" = hue_pal()(6)[1], 
                       "An Exception" = hue_pal()(6)[2], 
                       "In the Afternoon" = hue_pal()(6)[3],
                       "Mid Morning Peak" = hue_pal()(6)[4],
                       "On Commute Home" = hue_pal()(6)[5],
                       "Steady" = hue_pal()(6)[6])

muni_bar_plot <- function(MUNI){

plot_out <- clusters_by_muni %>% filter( Municipality == MUNI) %>% 
  mutate(NAME10 = str_squish(str_replace(NAME10, MUNI, "")))  %>% 
  ggplot(aes(x= Hour, y = Percent_2_Hour, fill = Cluster_Name)) + geom_bar(stat="identity") + 
  labs(title = MUNI, x="Hour of End of Two Hour Period", y = "Percent of 2 Hour Turnout") +
  facet_wrap(~NAME10, ncol=3) + 
  scale_fill_manual(values = cluster_names_list) +
  scale_x_continuous(breaks= seq(8,20, by=2)) + 
  theme_bw() + 
  theme(legend.position = "bottom")

return(plot_out)

}
