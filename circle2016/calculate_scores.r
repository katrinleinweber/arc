## calculate_scores.R

## calculate_scores.R ensures all files are properly configured and calculates OHI scores.
  ## - configure_toolbox.r ensures your files are properly configured. It is a script in your repository.
  ## - CalculateAll() calculates OHI scores. It is a function in the `ohicore` R package
  ##   (this can be written in R as `ohicore::CalculateAll()`).

## When you begin, configure_toolbox.r and CalculateAll() will calculate scores using
## the 'templated' data and goal models provided. We suggest you work
## goal-by-goal as you prepare data in the prep folder and develop goal models
## in functions.r. Running configure_toolbox.r and a specific goal model line-by-line
## in functions.R is a good workflow.

## run the configure_toolbox.r script to check configuration
source('~/github/arc/circle2016/configure_toolbox.r')

## calculate scenario scores
scores = ohicore::CalculateAll(conf, layers)

## save scores as scores.csv
write.csv(scores, 'scores.csv', na='', row.names=F)

## make graphics (source until they are added as ohicore functions)
source('PrepSpatial.R')
source('PlotMap.r')
source('PlotMapMulti.r')
source('PlotFlowerMulti.R')

## Make Maps ---- JSL 15/05: not working because of issue with shape file :(. will follow up
# PlotMapMulti(scores       = readr::read_csv('scores.csv'),
#              spatial_poly = PrepSpatial('spatial/regions_gcs.geojson'),
#              path_figures = 'reports/figures/maps')



## Make Flower Plots ----
rgns_complete <- read.csv('spatial/regions_lookup.csv')
rgn_names <- rgns_complete

rgns_to_plot <- rgns_complete %>%
  bind_rows(data_frame(rgn_id = 0, type='global', label='Arctic'))
rgns_to_plot <- rgns_to_plot$rgn_id

PlotFlowerMulti(scores          = readr::read_csv('scores.csv'),# %>% filter(region_id %in% rgns_to_plot),
                rgns_to_plot    = rgns_to_plot,
                rgn_names       = rgn_names,
                name_fig        = 'reports/figures/flowers',
                assessment_name = 'The Arctic')

### From Jamie

install.packages('githubinstall')
githubinstall::gh_install_packages("ggplot2", ref = "sf")

library(ggplot2)
library(sf)
library(dplyr)
library(rgeos)
library(sp)
library(rgdal)


## JAMIE's COMPARISON https://twitter.com/jafflerbach/status/872144371608682496


#projection
laeaCRS <- sp::CRS("+init=epsg:3572")

mapfile_path <- path.expand('~/github/arc/circle2016/spatial/regions_gcs.geojson')
poly_rgn <- rgdal::readOGR(dsn = mapfile_path, "OGRGeoJSON") %>%
  sp::spTransform(poly_rgn,laeaCRS)
spydf <- rgeos::gSimplify(poly_rgn, tol = 0.00001)

# this is a well known R / GEOS hack (usually combined with the above) to
# deal with "bad" polygons
spydf_states <- gBuffer(spydf, byid=TRUE, width=0)

# any bad polys?
sum(gIsValid(spydf, byid=TRUE)==FALSE)

## [1] 0

plot(spydf)

poly_df <- broom::tidy(poly_rgn)

poly_rgn$polyID <- sapply(slot(poly_rgn, "polygons"), function(x) slot(x, "ID"))
poly_df <- merge(poly_df, poly_rgn, by.x = "id", by.y="polyID")
head(poly_df)

poly_rgn_df <-poly_df %>% # use broom::tidy() instead of ggplot2::fortify()
  dplyr::rename(region_id = id) %>% # during broom::tidy, 'rgn_id' was renamed to 'id'
  mutate(region_id = as.integer(region_id))


## comparing shapefile with ggplot
ggplot() +                                               # initialize ggplot object
  geom_polygon(                                          # make a polygon
    data = poly_rgn_df,                                    # data frame
    aes(x = long, y = lat, group = group,                # coordinates, and group them by polygons
        fill = region_id))


### to plot geoJSON with sf!
poly_sf = sf::st_read(dsn = mapfile_path, "OGRGeoJSON")%>%
  st_transform(.,"+init=epsg:3572") #laea crs

ggplot()+
  geom_sf(data = poly_sf,aes(fill = rgn_id))

