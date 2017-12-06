library(ggplot2)
library(sf)
library(dplyr)
library(rgeos)

poly_sf = sf::st_read('circle2016/spatial/regions_gcs.geojson') %>%
  mutate(rgn_nam = as.character(rgn_nam)) %>%
  st_transform(.,"+init=epsg:3572") %>% #laea crs
  mutate(Region = ifelse(rgn_id == 1, "Arctic Alaska",
                         ifelse(rgn_id== 3, "Canadian Beaufort Sea",
                         ifelse(rgn_id== 4, "Russian Arctic",
                         ifelse(rgn_id == 5, "Svalbard",
                         ifelse(rgn_id == 6, "Arctic Norway",
                                ifelse(rgn_id == 8, "Western Greenland",
                                       ifelse(rgn_id == 9, "Eastern Greenland", rgn_nam))))))))
poly_sf$Region<- factor(poly_sf$Region, levels= c("Arctic Alaska", "Canadian Beaufort Sea", "Nunavut","Western Greenland", "Eastern Greenland", "Jan Mayen", "Svalbard", "Arctic Norway","Russian Arctic"))

ggplot() +
  geom_sf(data = poly_sf, aes(fill = Region)) +
  theme_bw() +
  scale_fill_brewer(palette = 1)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank())
