library(leaflet)
library(tidyverse)
library(htmlwidgets)
library(htmltools)
library(sf)
library(scales)
library(RColorBrewer)

temp <- tempfile()
URL <- "http://www2.census.gov/geo/tiger/TIGER2012/VTD/tl_2012_34_vtd10.zip"
download.file(URL,temp)
untar(temp, exdir= "data/tl_2012_34_vtd10")

temp1 <- tempfile()
URL <- "ftp://ftp2.census.gov/geo/tiger/TIGER2019/COUNTY/tl_2019_us_county.zip"
download.file(URL,temp1)
untar(temp1, exdir= "data/tl_2019_us_county")

vd_nj_shp <- "./data/tl_2012_34_vtd10/tl_2012_34_vtd10.shp"
vd_morris_sf <- st_read(vd_nj_shp) %>% filter(COUNTYFP10 == "027")

county_us_shp <- "./data/tl_2019_us_county/tl_2019_us_county.shp"
county_morris_sf <- st_read(county_us_shp) %>% filter(COUNTYFP == "027", STATEFP == "34")
county_morris_sf <- st_transform(county_morris_sf, crs ='+proj=longlat +datum=WGS84')

clusters_for_maps <- clusters_data %>% select(District, Cluster, Cluster_Name) %>% 
  distinct() %>% 
  mutate(District = str_replace(District, "Ward", "ward"),
         District = str_replace(District, "District", "voting district"),
         District = str_replace(District, "Town", "town"), 
         District = str_replace(District, "Boro", "boro"),
         District = str_replace(District, "Parsippany - Troy Hills", "Parsippany-Troy Hills"), 
         NAME10 = District)

vd_morris_clusters_sf <- vd_morris_sf %>% left_join(clusters_for_maps) %>% 
  select(District, Cluster, Cluster_Name, NAME10) %>% 
  filter(!is.na(Cluster)) %>% 
  mutate(color = Cluster, 
         label1 = paste0(Cluster_Name, "</br>", NAME10))

factpal <- colorFactor(hue_pal()(6),vd_morris_clusters_sf$Cluster_Name)
  
  vd_morris_clusters_sf <- st_transform(vd_morris_clusters_sf, crs ='+proj=longlat +datum=WGS84')
  
  labels <- sprintf(
    "<strong>%s</strong>",
    vd_morris_clusters_sf$label1
  ) %>% lapply(htmltools::HTML)
  
cluster_map <-   leaflet(vd_morris_clusters_sf) %>% addTiles() %>% 
    addPolygons(fillColor = ~factpal(Cluster_Name),
                fillOpacity = .75,
                color = "black",
                label = labels,
                weight = 1) %>%  
    addPolygons(data = county_morris_sf,fill = NA, 
                color = "black", 
                weight = 2) %>%
  addLegend("bottomleft", pal = factpal, values = ~Cluster_Name,
            title = "Cluster",
            #labFormat = labelFormat(prefix = "$"),
            opacity = 1)


