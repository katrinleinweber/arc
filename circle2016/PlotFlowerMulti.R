## PlotFlowerMulti.r

library(dplyr) #install.packages('dplyr')
# TODO: change to read_csv

PlotFlowerMulti <- function(scores          = read.csv('scores.csv'), # dataframe with regions, goals, dimensions, scores
                            rgns_to_plot, # vector of regions to plot. Can be single many, eg 1 or c(1, 5)
                            rgn_names       = read.csv('layers/rgn_global.csv'),
                            assessment_name = 'Global',
                            goals           = read.csv('conf/goals.csv'),
                            fld_value_id    = 'region_id', # header of scores variable; likely 'rgn_id' or 'region_id'
                            fld_value_score = 'score', # header of scores variable; likely 'score' or 'value'
                            dim_choice      = 'score', # header of scores variable; likely "future", "pressures", "resilience", "score", "status", "trend". This should be optional so if scores is only a 2-column dataframe it still works
                            print_fig       = FALSE,
                            save_fig        = TRUE,
                            name_fig        = 'reports/figures/flower',
                            overwrite       = TRUE,
                            color_scheme    = 'new' ) {
  # DEBUG: scores=read.csv('scores.csv'); rgn_names = read.csv('layers/rgn_global.csv'); goals = read.csv('conf/goals.csv'); fld_value_id = 'region_id'; fld_value_score = 'score';dim_choice = 'score'; print_fig = TRUE; save_fig = TRUE; name_fig = 'reports/figures/flower'; overwrite = TRUE; color_scheme = 'new'

  ## setup ----

  ## check field values in scores column names
  if ( !fld_value_score %in% names(scores) | !fld_value_id %in% names(scores) ) {
    stop(sprintf('PlotFlowerMulti() is attempting to plot `%s` and `%s` but one or both of these are not found.',
                 fld_value_score, fld_value_id))
  }

  ## if exists, filter dimension for 'score'
  if ( 'dimension' %in% names(scores) ) {
    scores <- scores %>%
      dplyr::filter(dimension == dim_choice)
  }


  ## weights for FIS vs. MAR
  # ##TODO: add this as variable to read in
  # weights_fis <- read_csv(sprintf("layers/fp_wildcaught_weight.csv")) %>%
  #   rbind(data.frame(rgn_id=0, w_fis=mean(weights$w_fis)))
  #

  ## identify goals to include in flower plot ----

  ## goals that have subgoals; not included in flower plot
  goals_supra <- goals %>%
    filter(parent != '') %>%
    unique()

  ## identify goals that will be plotted
  goals <- goals %>%
    dplyr::filter(!(goal %in% goals_supra$parent)) %>%
    dplyr::select(goal, order_color, order_hierarchy, weight, name_flower) %>%
    dplyr::mutate(name_flower = gsub("\\n", "\n", name_flower, fixed = TRUE)) %>%
    dplyr::mutate(goal = as.character(goal)) %>%
    dplyr::arrange(order_hierarchy)

  cat(sprintf('These will be included in the flower plot: %s. \nNote: "supragoals" (goals that have subgoals) are not plotted',
              paste(goals$goal, collapse = ', ')))

  ## TODO: add check to ensure goals columns are numeric with decimals (not strings with commas)

  scores <- scores %>%
    dplyr::mutate(goal = as.character(goal)) %>%
    dplyr::filter(region_id <= 250 | region_id > 300) %>%  # remove high seas but keep above eg BHI aggregates
    dplyr::filter(region_id != 213)


  ## add assessment area ('Global') to rgn_names
  rgn_names <- rgn_names %>%
    mutate(label = as.character(label)) %>%
    bind_rows(data_frame(rgn_id = as.integer(0), label = assessment_name)) %>%
    distinct(rgn_id, label) %>% # this will remove rgn_id 0 if was doubled up
    filter(rgn_id %in% rgns_to_plot)


  ## loop through regions
  for (r in rgns_to_plot){  # r=0 r=301

    # error if rgn_id is not listed in rgn_names variable
    if (!r %in% rgn_names$rgn_id){
      stop(sprintf('Cannot plot rgn_id %s; this region is not found in the rgn_names variable\n', r))
    }

    ## region name for title
    rgn_name <- rgn_names %>%
      dplyr::filter(rgn_id == r)
    rgn_name = rgn_name$label
    print(sprintf('Flower Plot for %s (rgn_id %s) . . .', rgn_name, r))

    ## combine to goal weighting
    scores_r = scores %>%
      dplyr::filter(region_id == r) %>%
      dplyr::inner_join(goals, by="goal") %>%
      dplyr::arrange(order_color)

    score_Index <-  subset(scores, region_id==r & goal == 'Index', score, drop=T)

    # get colors for aster, based on 10 colors, but extended to all goals. subselect for goals.wts
    if(color_scheme == "new"){
      scores_r$cols.goals.all = as.character(  # note! if color scheme is all crazy, make sure it's not a factor!
        cut(scores_r$score,
            breaks=seq(0, 100, by=10),
            include.lowest=TRUE,
            labels=RColorBrewer::brewer.pal(10, 'RdYlBu')))
    } else {
      # TODO: update this
      # scores_r$cols.goals.all = colorRampPalette(RColorBrewer::brewer.pal(11, 'Spectral'), space='Lab')(length(goals.all))

    }

    # #TODO: fix this weights after correcting for fisheries/mariculture contributions
    # scores_r$weight[scores_r$goal == "FIS"] <-   weights$w_fis[weights$rgn_id == rgn_id]
    # scores_r$weight[scores_r$goal == "MAR"] <- 1 - weights$w_fis[weights$rgn_id == rgn_id]

    ## flower plot ----

    ## fig name
    fig_png = sprintf('%s_%s.png', name_fig, gsub(' ','', rgn_name))
    res=72

    if (overwrite | !file.exists(fig_png)){

      png(fig_png, width=res*7, height=res*7, bg = "transparent")

      # TODO: also a pdf
      # fig_pdf = sprintf('%s/flowerPlots/flower_%s.pdf', path_figures, gsub(' ','_', rgn_name))
      # if (overwrite | !file.exists(fig_pdf)){
      #       pdf(fig_pdf)

      PlotFlower(main       = rgn_name,
                 lengths    = ifelse(is.na(scores_r$score), 100, scores_r$score),
                 widths     = scores_r$weight,
                 fill.col   = ifelse(is.na(scores_r$cols.goals.all), 'grey80', scores_r$cols.goals.all),
                 labels     = ifelse(is.na(scores_r$score),
                                     paste(scores_r$name_flower, '-', sep='\n'),
                                     paste(as.character(scores_r$name_flower), round(scores_r$score), sep='\n')),
                 center     = round(score_Index),
                 max.length = 100, disk=0.4, label.cex=0.9, label.offset=0.155, cex=2.2, cex.main=2.5)
      dev.off()

      # repeat it so that it prints in the Plot window; need to change PlotFlower so that this is not needed.
      PlotFlower(main       = rgn_name,
                 lengths    = ifelse(is.na(scores_r$score), 100, scores_r$score),
                 widths     = scores_r$weight,
                 fill.col   = ifelse(is.na(scores_r$cols.goals.all), 'grey80', scores_r$cols.goals.all),
                 labels     = ifelse(is.na(scores_r$score),
                                     paste(scores_r$name_flower, '-', sep='\n'),
                                     paste(as.character(scores_r$name_flower), round(scores_r$score), sep='\n')),
                 center     = round(score_Index),
                 max.length = 100, disk=0.4, label.cex=0.9, label.offset=0.155, cex=2.2, cex.main=2.5)

      # return(p)
      #system(sprintf('convert -density 150x150 %s %s', fig_pdf, fig_png)) # imagemagick's convert
    }

    # flower md this was for the rmd
    #   cat(sprintf('![flower plot of %s](figures/%s)\n\n', rgn_name, basename(fig_png)))




  }

}
