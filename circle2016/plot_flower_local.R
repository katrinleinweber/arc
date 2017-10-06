#' Flower plots for OHI scores
#' By Casey O'Hara, Julia Lowndes, Melanie Frazier @ohi-science
#'
#' @param score_df data frame of scores for each goal for one region
#' @param goals_csv filepath for config info, default is `conf/goals.csv`
#' @param score_ref scale (default is 0-100, could also be 0-1) ## TODO I don't think we need this as a variable. Could have a check/message to tell you so much.
#' @param fig_save provide a file name to save the plot (default is no save)
#' @param incl_legend show the legend? (default is TRUE)
#' @param show_plot show the plot? (default is TRUE)
#'
#' @return
#' @export
#'
#' @examples
#'
#' ## NOTE::::: ggtheme plot is a function at the bottom of this file
library(RColorBrewer)
#'
PlotFlower <- function(score_df,
                       goals_csv   = 'conf/goals.csv',
                       score_ref   = 100,
                       fig_save    = NULL,
                       incl_legend = TRUE,
                       show_plot   = TRUE) {

  ### set up goals.csv configuration information, if available
  if ( !is.null(goals_csv) ) {

    ## read in conf/goals.csv, deal with supra goals
    conf <-  readr::read_csv(goals_csv)
    goals_supra <- na.omit(unique(conf$parent))
    supra_lookup <- conf %>%
      filter(goal %in% goals_supra) %>%
      select(parent = goal, name_supra = name)

    ## extract conf info for labeling
    conf <- conf %>%
      left_join(supra_lookup, by = 'parent') %>%
      filter(!(goal %in% goals_supra)) %>%
      select(goal, order_color, order_hierarchy,
             weight, name_supra, name_flower) %>%
      mutate(name_flower = gsub("\\n", "\n", name_flower, fixed = TRUE)) %>%
      mutate(name_supra  = gsub("& ", "&\n", name_supra, fixed = TRUE),
             name_supra  = gsub("Coastal", "", name_supra, fixed = TRUE)) %>%
      arrange(order_hierarchy)

    # region scores
    score_df <- score_df %>%
      inner_join(conf, by="goal") %>%
      arrange(order_color)

  }

  ## set up positions for the bar centers:
  ## cumulative sum of weights (incl current) minus half the current weight
  score_df <- score_df %>%
    mutate(score = score * 100/score_ref,  # if 0-1, change to 0-100
           pos   = sum(weight) - (cumsum(weight) - 0.5 * weight)) %>%
    mutate(pos_end = sum(weight)) %>%
    group_by(name_supra) %>%
    mutate(pos_supra  = ifelse(!is.na(name_supra), mean(pos), NA)) %>%
    ungroup() %>%
    arrange(pos) %>%
    filter(weight != 0)

  ## weights for FIS vs. MAR
  w_fn <- list.files(path="layers", pattern = "fp_wildcaught_weight",
                     full.names = TRUE)

  if ( file.exists(w_fn) ) {
    w <- read_csv(w_fn)
    w <- rbind(w,
               data.frame(rgn_id = 0, w_fis = mean(w$w_fis))) %>%
      arrange(rgn_id)

    ## inject FIS/MAR weights
    region_id <- unique(score_df$region_id)
    score_df$weight[score_df$goal == "FIS"] <- w$w_fis[w$rgn_id == region_id]
    score_df$weight[score_df$goal == "MAR"] <- 1 - w$w_fis[w$rgn_id == region_id]

    ## recalculate pos with these injected weights
    score_df <- score_df %>%
      mutate(pos = sum(weight) - (cumsum(weight) - 0.5 * weight))

  } else {
    message('Cannot find `layers/fp_wildcaught_weight*.csv`...plotting FIS and MAR with equal weighting\n')
  }


  ## some labeling
  ## extract Index score for center labeling
  score_index  <- round(score_df$score[score_df$goal == 'Index'])

  goal_labels <- score_df %>%
    select(goal, name_flower)

  ## set up for displaying NAs
  score_df_na <- score_df %>%
    mutate(score = ifelse(is.na(score), 100, NA))


  ## Mel's color palette
  reds <-  grDevices::colorRampPalette(
    c("#A50026", "#D73027", "#F46D43", "#FDAE61", "#FEE090"),
    space="Lab")(65)
  blues <-  grDevices::colorRampPalette(
    c("#E0F3F8", "#ABD9E9", "#74ADD1", "#4575B4", "#313695"))(35)
  myPalette <-   c(reds, blues)

  ## some parameters for the plot
  p_limits <- c(0, score_df$pos_end[1])
  p_score  <- round(weighted.mean(score_df$score, score_df$weight, na.rm = TRUE), 0)
  blank_circle_rad <- 42
  supra_rad  <- 145
  light_line <- 'grey90'
  white_fill <- 'white'
  light_fill <- 'grey80'
  med_line   <- 'grey50'
  med_fill   <- 'grey52'
  dark_line  <- 'grey20'
  dark_fill  <- 'grey22'

  ## set up basic plot parameters
  plot_obj <- ggplot(data = score_df,
                     aes(x = pos, y = score, fill = score, width = weight))

  plot_obj <- plot_obj +
    ## sets up the background/borders to the external boundary (100%) of plot:
    geom_bar(aes(y = 100),
             stat = 'identity', color = light_line, fill = white_fill, size = .2) +
    geom_errorbar(aes(x = pos, ymin = 100, ymax = 100, width = weight),
                  size = 0.5, color = light_line, show.legend = NA)
  ## lays any NA bars on top of background, with darker grey:
  if(any(!is.na(score_df_na$score)))
    plot_obj <- plot_obj +
    geom_bar(data = score_df_na, aes(x = pos, y = score),
             stat = 'identity', color = light_line, fill = light_fill, size = .2)

  ## establish the basics of the flower plot...
  plot_obj <- plot_obj +
    ## plot the actual scores on top of background/borders:
    geom_bar(stat = 'identity', color = dark_line, size = .2) +
    ## emphasize edge of petal
    geom_errorbar(aes(x = pos, ymin = score, ymax = score),
                  size = 0.5, color = dark_line, show.legend = NA) +
    ## add supragoal arcs
    geom_errorbar(aes(x = pos_supra, ymin = supra_rad, ymax = supra_rad),
                  size = 0.25, show.legend = NA) +
    ## plot zero as a baseline:
    geom_errorbar(aes(x = pos, ymin = 0, ymax = 0),
                  size = 0.5, color = dark_line, show.legend = NA) +
    ## turn linear bar chart into polar coordinates:
    coord_polar(start = pi * 0.5) +
    ## set petal colors to the red-yellow-blue color scale:
    scale_fill_gradientn(colours=myPalette, na.value="black",
                         limits = c(0, 100)) +
    ### use weights to assign widths to petals:
    scale_x_continuous(labels = score_df$goal, breaks = score_df$pos, limits = p_limits) +
    scale_y_continuous(limits = c(-blank_circle_rad,
                                  ifelse(first(goal_labels == TRUE) | is.data.frame(goal_labels),
                                         150, 100)))

  plot_obj <- plot_obj +
    geom_text(aes(label = score_index),
              x = 0, y = -blank_circle_rad,
              hjust = .5, vjust = .5,
              size = 12,
              color = dark_line)

  ### clean up the theme
  plot_obj <- plot_obj +
    ggtheme_plot() +
    theme(panel.grid.major = element_blank(),
          axis.line  = element_blank(),
          axis.text  = element_blank(),
          axis.title = element_blank())

  ### include or exclude goal flower names; dynamic if no border
  ### if no outline, labels go near bars; otherwise place near outer edge
  # myAng0 <-
  #   seq(-20+270,-340+270,length.out = 13)

  plot_obj <- plot_obj +
    geom_text(aes(label = name_flower, x = pos, y = 120), #, angle = myAng0),
              hjust = .5, vjust = .5,
              size = 3,
              color = dark_line)

  ### position supra names outside the arc. x is angle, y is distance from center

  st2 <- score_df %>%
    mutate(name_supra2 = ifelse(is.na(name_supra), name_flower, name_supra)) %>%
    select(name_supra0 = name_supra, name_supra2, pos_supra0 = pos_supra) %>%
    unique() %>%
    as.data.frame() %>%
    mutate(myAng = seq(-20+270,-340+270,length.out = 9)) %>% # of goals that you have, fill in others with NAs)) %>%
    mutate(supra_rad = supra_rad) %>%
    filter(!is.na(name_supra0))

  plot_obj +
    geom_text(data = st2,
              inherit.aes = FALSE,
              aes(label = st2$name_supra0, x = st2$pos_supra0, y = supra_rad+9, angle = st2$myAng),
              hjust = .5, vjust = .5,
              size = 3,
              color = dark_line)

  ### include or exclude the legend
  if(!incl_legend) {
    plot_obj <- plot_obj +
      theme(legend.position = 'none')
  }


  ### display/save options: print to graphics, save to file
  if(show_plot) {
    print(plot_obj)
  }

  if(!is.null(fig_save)) {
    ggsave(filename = fig_save,
           height = 6, width = 8, units = 'in', dpi = 300,
           plot = plot_obj)
  }

  ### ...then return the plot object for further use
  return(invisible(plot_obj))
}

ggtheme_plot <- function(base_size = 9) {
  theme(axis.ticks = element_blank(),
        text             = element_text(family = 'Helvetica', color = 'gray30', size = base_size),
        plot.title       = element_text(size = rel(1.25), hjust = 0, face = 'bold'),
        panel.background = element_blank(),
        legend.position  = 'right',
        panel.border     = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = 'grey90', size = .25),
        # panel.grid.major = element_blank(),
        legend.key       = element_rect(colour = NA, fill = NA),
        axis.line        = element_blank()) # element_line(colour = "grey30", size = .5))
}

