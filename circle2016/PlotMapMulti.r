#' PlotMapMulti
#' Creates multiple static maps by calling PlotMap() by looping through listed goals in the input dataframe
#'
#' @param scores dataframe with regions, goals, dimensions, scores
#' @param spatial_poly dataframe of spatial boundaries; prepared by PrepSpatial()
#' @param fld_value_id usually rgn_id or region_id; default 'region_id'
#' @param fld_value_score column name of value to plot; default 'score'
#' @param dim_choice dimension to plot; default 'score'
#' @param print_fig default FALSE
#' @param save_fig default TRUE
#' @param path_figures # default 'reports/figures'
#' @param scale_label # default 'score' TODO: necessary?
#' @param scale_limits # default c(0, 100)
#'
#' @return
#' @export
#'
#' # example call after clone BHI repo and setwd('~/github/bhi/baltic2015')
#' @examples PlotMapMulti(scores = readr::read_csv('scores.csv') %>% filter(region_id < 300), spatial_poly = PrepSpatial('spatial/regions_gcs.geojson'), path_figures = 'reports/figures/BHI_regions')

PlotMapMulti <- function(scores          = read.csv('scores.csv'),
                         spatial_poly    = PrepSpatial('spatial/regions_gcs.geojson'),
                         fld_value_id    = 'region_id',
                         fld_value_score = 'score',
                         dim_choice      = 'score',
                         print_fig       = FALSE,
                         save_fig        = TRUE,
                         # active_plotly   = FALSE, ## takes too long, causes RStudio to crash
                         path_figures    = 'reports/figures',
                         scale_label     = 'score',
                         scale_limits    = c(0, 100)) {
                         # TODO: interactive = FALSE

  ## setup ----

  ## check field values in scores column names
  if ( !fld_value_score %in% names(scores) | !fld_value_id %in% names(scores) ) {
    stop(sprintf('Column name "%s" or "%s" not found in scores variable, please modify PlotMap() function call.',
                 fld_value_score, fld_value_id))
  }

  ## if exists, remove region_id == 0 for mapping
  if (0 %in% scores[[fld_value_id]]){
    scores <- scores[scores[[fld_value_id]] != 0, ] # figure this out with filter() someday
  }

  ## if exists, filter dimension for 'score'
  if ( 'dimension' %in% names(scores) ) {
    scores <- scores %>%
      filter(dimension == dim_choice)
  }

  ## loop over each goal and subgoal ----

  goals <- unique(scores$goal)
  for (g in goals){ # g ='AO'

    print(sprintf('Mapping %s . . .', g))

    ## filter scores for goal g
    scores_g <-  scores %>%
      filter(goal == g)


    ## plot map!
    PlotMap(scores       = scores_g,
            rgn_poly     = spatial_poly,
            fld_rgn      = fld_value_id,
            fld_score    = fld_value_score,
            print_fig    = print_fig,
            fig_path     = sprintf('%s/map_%s.png', path_figures, g),
            map_title    = sprintf('Ocean Health Index: %s', g),
            scale_label  = scale_label,
            scale_limits = scale_limits)
    #DEBUG scores = scores_g; rgn_poly = spatial_poly; fld_rgn= fld_value_id;fld_score = fld_value_score;print_fig = print_fig; fig_path = sprintf('%s/map_%s.png', path_figures, g); map_title = map_title;scale_label = scale_label;scale_limits = scale_limits

  }


}
