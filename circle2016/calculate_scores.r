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



