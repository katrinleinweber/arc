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


##### Visualize ----

## Make Flower Plots of all goals for a region ----
source('PlotFlowerMulti.R')

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


## Make Maps of all regions for a goal ----
source('PlotMap.r')
source('PlotMapMulti.r')


### install `sf`` branch of ggplot2 (in development) <-  one-time only!!
# install.packages('githubinstall')
# githubinstall::gh_install_packages("ggplot2", ref = "sf")
library(ggplot2)
library(sf)
library(dplyr)

### to plot geoJSON with sf!

## create the sf object
mapfile_path <- path.expand('~/github/arc/circle2016/spatial/regions_gcs.geojson')
poly_sf <- sf::st_read(dsn = mapfile_path, "OGRGeoJSON") %>%
  sf::st_transform(.,"+init=epsg:3572") #laea crs

## map!
PlotMap(scores  = readr::read_csv('scores.csv'),
        poly_sf = poly_sf,
        path_fig = 'reports/figures/maps_test.png')

PlotMapMulti(scores  = readr::read_csv('scores.csv'),
             poly_sf = poly_sf,
             path_fig = 'reports/figures')
