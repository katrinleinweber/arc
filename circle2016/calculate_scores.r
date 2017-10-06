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

## Flower plots for each region ----
## Will plot MAR/FIS as unequal weights based on the `fp_wildcaught_weight` layer


## source from ohibc until added to ohicore
source('plot_flower_local.R')

## regions info
regions <- bind_rows(
  data_frame(                # order regions to start with whole study_area
    region_id   = 0,
    region_name = 'Arctic'),
  read_csv('spatial/regions_list.csv') %>%
    dplyr::select(region_id   = rgn_id,
           region_name = rgn_name))

## set figure name
regions <- regions %>%
  mutate(flower_png = sprintf('reports/figures/flower_%s.png',
                              str_replace_all(region_name, ' ', '_')))
readr::write_csv(regions, 'reports/figures/regions_figs.csv')

## save flower plot for each region
for (i in regions$region_id) { # i = 0

  ## fig_name to save
  fig_save <- regions$flower_png[regions$region_id == i]

  ## scores info
  score_dfX <- scores %>%
    filter(dimension == 'score') %>%
    filter(region_id == i)

  ## create flower plot
  plot_obj <- PlotFlower(score_df = score_dfX, fig_save = fig_save)

}


## Maps for each goal ----
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
PlotMap(scores  = readr::read_csv('scores.csv') %>% filter(goal == 'Index', dimension == 'score'),
        poly_sf = poly_sf,
        path_fig = 'reports/figures/maps_test.png')

PlotMapMulti(scores  = readr::read_csv('scores.csv'),
             poly_sf = poly_sf,
             path_fig = 'reports/figures')
