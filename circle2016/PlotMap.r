#' PlotMap
#' Plots static maps of OHI scores.
#'
#' @param scores dataframe with at least 2 columns: rgn_id and values.Must have the same length as the number of regions to map
#' @param poly_sf sf dataframe of spatial boundaries; prepared sf::st_read(dsn = 'spatial/regions_gcs.geojson', "OGRGeoJSON")
#' @param map_title optional title for map
#' @param include_land whether or not to map land behind OHI regions. SEE TODOs BELOW
#' @param fld_rgn usually rgn_id or region_id; default 'region_id'
#' @param fld_score column name of value to plot; default 'score'
#' @param scale_label default to 'score' TODO: necessary?
#' @param scale_limits default to c(0, 100)
#' @param print_fig logical to print to display; default TRUE
#' @param path_fig file path to save png; default NULL
#'
#' @return (invisible) ggplot object
#' @export
#'
#' @examples
#'
#'

##### temporary (until added to ohicore)
library(ggplot2) # install.packages('ggplot2')
library(RColorBrewer) # install.packages('RColorBrewer')
library(dplyr)
library(tidyr)

## see: https://github.com/hadley/ggplot2/wiki/plotting-polygon-shapefiles

PlotMap <- function(scores,
                    poly_sf         = sf::st_read(dsn = 'spatial/regions_gcs.geojson', "OGRGeoJSON"),
                    map_title       = element_blank(),
                    # include_land    = TRUE, # need to develop if we want this
                    fld_rgn         = 'region_id',
                    fld_score       = 'score',
                    scale_label     = 'Region',
                    scale_limits    = c(1, 9),
                    print_fig       = TRUE, ### print to display
                    path_fig    = NULL) { ### path to save the plot as an image
                    # allow fig_png to be NULL and then pass it back as a list of ggplot objects so that you could modify it more on {
  ## DEBUG: source('PrepSpatial.R'); fld_rgn <- 'region_id'; fld_score <- 'score'; scale_limits <- c(0, 100);
  ## poly_sf = sf::st_read(dsn = 'spatial/regions_gcs.geojson', "OGRGeoJSON"),
  ## PlotMap(scores, poly_sf = poly_sf, scale_label = 'test1', map_title = 'test2')


  ### rename columns for convenience...
  names(scores)[names(scores) == fld_rgn]   <- 'rgn_id'
  # names(poly_sf)[names(poly_sf) == fld_rgn]   <- 'rgn_id'
  names(scores)[names(scores) == fld_score] <- 'score'

  ### join polygon with scores
  poly_sf <- poly_sf %>%
    left_join(scores %>%
                dplyr::select(rgn_id, score),
              by = 'rgn_id')

  ## plot with ggplot's sf branch
  # could also do source('https://raw.githubusercontent.com/OHI-Science/ohibc/master/src/R/common.R')
  # ggtheme_plot() +

  df_plot <- ggplot() +
    geom_sf(data = poly_sf, aes(fill = rgn_id)) +
    theme(axis.ticks = element_blank(),
          axis.text  = element_blank(),
          axis.title = element_blank(),
          text       = element_text(family = 'Helvetica', color = 'gray30', size = 12),
          plot.title = element_text(size = rel(1.5), hjust = 0, face = 'bold'),
          legend.position = 'right') +
    theme(
      # panel.grid.major = element_line(color='gray90', size = 0.25),
      # panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    scale_fill_gradientn(colours = brewer.pal(10, 'RdYlBu'),
                         na.value = 'gray80',
                         limits = scale_limits,
                         name = scale_label) +
    geom_polygon(color = 'gray80', size = 0.1) +
    labs(title = map_title)




 ## if(include_land) {
    ## TODO: plot optional land .shp
    ## consider:
      ## - downres-ing shapefiles like downres_polygons.r: https://github.com/OHI-Science/ohiprep/blob/9daf812e910b80cf3042b24fcb458cf62e359b1a/globalprep/spatial/downres_polygons.R; see calls from https://github.com/OHI-Science/ohi-global/blob/draft/global2015/Reporting/map_fxns.R
      ## - Hadleys' ggmap tiles that work with ggplot2 https://github.com/dkahle/ggmap#ggmap
      ## translating this whole function to tmap
 ## }

  if(print_fig) {
    print(df_plot)
  }

  if(!is.null(path_fig)) {
    ggsave(path_fig, plot = df_plot, width = 7, height = 7)
  }

  return(invisible(df_plot))
}
