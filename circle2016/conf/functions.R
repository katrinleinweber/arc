Setup = function(){
  if(file.exists('eez2013/temp/referencePoints.csv')){file.remove('temp/referencePoints.csv')}
  referencePoints <- data.frame(goal=as.character(),
                                method = as.character(),
                                reference_point = as.character())
  write.csv(referencePoints, 'temp/referencePoints.csv', row.names=FALSE)
}

FIS = function(layers, status_year){

  #catch data
  c = SelectLayersData(layers, layers='fis_meancatch_arc2016', narrow = TRUE) %>%
    dplyr::select(
      rgn_id    = id_num,
      stock_id_taxonkey = category,
      year,
      catch          = val_num)
  # b_bmsy data
  b = SelectLayersData(layers, layer='fis_b_bmsy_arc2016', narrow = TRUE) %>%
    dplyr::select(
      rgn_id         = id_num,
      stock_id      = category,
      year,
      bmsy           = val_num)

  #comsir data
  #f = SelectLayersData(layers, layer='fis_comsir_bmsy_arc2016', narrow = TRUE) %>%
    #dplyr::select(
      #rgn_id         = id_num,
      #stock_id      = category,
      #year,
      #bmsy           = val_num)

  # The following stocks are fished in multiple regions and have high b/bmsy values
  # Due to the underfishing penalty, this actually penalizes the regions that have the highest
  # proportion of catch of these stocks.  The following corrects this problem:
  #  filter(b, stock_id %in% c('Katsuwonus_pelamis-71', 'Clupea_harengus-27', 'Trachurus_capensis-47'))

  high_bmsy <- c('Katsuwonus_pelamis-71', 'Clupea_harengus-27', 'Trachurus_capensis-47', 'Sardinella_aurita-34', 'Scomberomorus_cavalla-31')

  b <- b %>%
    mutate(bmsy = ifelse(stock_id %in% high_bmsy, 1, bmsy))


  # separate out the stock_id and taxonkey:
  c <- c %>%
    mutate(stock_id_taxonkey = as.character(stock_id_taxonkey)) %>%
    mutate(taxon_key = stringr::str_sub(stock_id_taxonkey, -6, -1)) %>%
    mutate(stock_id = substr(stock_id_taxonkey, 1, nchar(stock_id_taxonkey)-7)) %>%
    mutate(catch = as.numeric(catch)) %>%
    mutate(year = as.numeric(as.character(year))) %>%
    mutate(rgn_id = as.numeric(as.character(rgn_id))) %>%
    mutate(taxon_key = as.numeric(as.character(taxon_key))) %>%
    dplyr::select(rgn_id, year, stock_id, taxon_key, catch)

  # general formatting:
  b <- b %>%
    mutate(bmsy = as.numeric(bmsy)) %>%
    mutate(rgn_id = as.numeric(as.character(rgn_id))) %>%
    mutate(year = as.numeric(as.character(year))) %>%
    mutate(stock_id = as.character(stock_id))


  # ------------------------------------------------------------------------
  # STEP 1. Calculate scores for Bbmsy values
  # -----------------------------------------------------------------------
  #  *************NOTE *****************************
  #  These values can be altered
  #  ***********************************************
  alpha <- 0.5
  beta <- 0.25
  lowerBuffer <- 0.95
  upperBuffer <- 1.05

  b$score = ifelse(b$bmsy < lowerBuffer, b$bmsy,
                   ifelse (b$bmsy >= lowerBuffer & b$bmsy <= upperBuffer, 1, NA))
  b$score = ifelse(!is.na(b$score), b$score,
                   ifelse(1 - alpha*(b$bmsy - upperBuffer) > beta,
                          1 - alpha*(b$bmsy - upperBuffer),
                          beta))


  # ------------------------------------------------------------------------
  # STEP 1. Merge the b/bmsy data with catch data
  # -----------------------------------------------------------------------
  data_fis <- c %>%
    left_join(b, by=c('rgn_id', 'stock_id', 'year')) %>%
    dplyr::select(rgn_id, stock_id, year, taxon_key, catch, bmsy, score)


  # ------------------------------------------------------------------------
  # STEP 2. Estimate scores for taxa without b/bmsy values
  # Median score of other fish in the region is the starting point
  # Then a penalty is applied based on the level the taxa are reported at
  # -----------------------------------------------------------------------

  ## this takes the median score within each region
  data_fis_gf <- data_fis %>%
    group_by(rgn_id, year) %>%
    mutate(Median_score = quantile(score, probs=c(0.5), na.rm=TRUE)) %>%
    ungroup()

  ## this takes the median score across all regions (when no stocks have scores within a region)
  data_fis_gf <- data_fis_gf %>%
    group_by(year) %>%
    mutate(Median_score_global = quantile(score, probs=c(0.5), na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(Median_score = ifelse(is.na(Median_score), Median_score_global, Median_score)) %>%
    dplyr::select(-Median_score_global)

  #  *************NOTE *****************************
  #  In some cases, it may make sense to alter the
  #  penalty for not identifying fisheries catch data to
  #  species level.
  #  ***********************************************

  penaltyTable <- data.frame(TaxonPenaltyCode=1:6,
                             penalty=c(0.1, 0.25, 0.5, 0.8, 0.9, 1))

  data_fis_gf <- data_fis_gf %>%
    mutate(TaxonPenaltyCode = as.numeric(substring(taxon_key, 1, 1))) %>%
    left_join(penaltyTable, by='TaxonPenaltyCode') %>%
    mutate(score_gf = Median_score * penalty) %>%
    mutate(score_gapfilled = ifelse(is.na(score), "Median gapfilled", "none")) %>%
    mutate(score = ifelse(is.na(score), score_gf, score))


  gap_fill_data <- data_fis_gf %>%
    mutate(gap_fill = ifelse(is.na(penalty), "none", "median")) %>%
    dplyr::select(rgn_id, stock_id, taxon_key, year, catch, score, gap_fill) %>%
    filter(year == status_year)
  write.csv(gap_fill_data, 'temp/FIS_summary_gf.csv', row.names=FALSE)

  status_data <- data_fis_gf %>%
    dplyr::select(rgn_id, stock_id, year, catch, score)


  # ------------------------------------------------------------------------
  # STEP 4. Calculate status for each region
  # -----------------------------------------------------------------------

  # 4a. To calculate the weight (i.e, the relative catch of each stock per region),
  # the mean catch of taxon i is divided by the
  # sum of mean catch of all species in region/year

  status_data <- status_data %>%
    group_by(year, rgn_id) %>%
    mutate(SumCatch = sum(catch)) %>%
    ungroup() %>%
    mutate(wprop = catch/SumCatch)

  status_data <- status_data %>%
    group_by(rgn_id, year) %>%
    summarize(status = prod(score^wprop)) %>%
    ungroup()

  # ------------------------------------------------------------------------
  # STEP 5. Get yearly status and trend
  # -----------------------------------------------------------------------

  status <-  status_data %>%
    group_by(rgn_id) %>% #group by rgn_id and add max year in to get status
    filter(year >= max(year, na.rm=T)) %>%
    mutate(
      score     = round(status*100, 1),
      dimension = 'status') %>%
    dplyr::select(region_id=rgn_id, score, dimension)

  status_year<- 2010
  trend_years <- status_year:(status_year-4)
  first_trend_year <- min(trend_years)

  trend <- status_data %>%
    filter(year %in% trend_years) %>%
    group_by(rgn_id) %>%
    do(mdl = lm(status ~ year, data=.),
       adjust_trend = .$status[.$year == first_trend_year]) %>%
    summarize(region_id = rgn_id,
              score = round(coef(mdl)['year']/adjust_trend * 5, 4),
              dimension = 'trend') %>%
    ungroup() %>%
    mutate(score = ifelse(score > 1, 1, score)) %>%
    mutate(score = ifelse(score < (-1), (-1), score))

  # return scores
  scores= full_join(status, trend)%>%
    mutate(goal='FIS')%>%
    data.frame()


  return(scores)
}


MAR = function(layers, status_year){

  # layers used: mar_harvest_tonnes, mar_harvest_species, mar_sustainability_score, mar_coastalpopn_inland25km, mar_trend_years
  harvest_tonnes <- SelectLayersData(layers, layers='mar_harvest_tonnes', narrow = TRUE) %>%
    dplyr::select(rgn_id=id_num, species_code=category, year, tonnes=val_num)

  harvest_species <- SelectLayersData(layers, layers='mar_harvest_species', narrow = TRUE) %>%
    dplyr::select(species_code=category, species=val_chr)

  sustainability_score <- SelectLayersData(layers, layers='mar_sustainability_score', narrow = TRUE) %>%
    dplyr::select(rgn_id=id_num, species_code=category, sust_coeff=val_num)

  popn_inland25mi <- SelectLayersData(layers, layers='mar_coastalpopn_inland25km', narrow = TRUE) %>%
    dplyr::select(rgn_id=id_num, year, popsum=val_num)


  rky <-  harvest_tonnes %>%
    left_join(harvest_species, by = 'species_code') %>%
    left_join(sustainability_score, by = c('rgn_id', 'species_code'))

  # fill in gaps with no data
  rky <- spread(rky, year, tonnes)
  rky <- gather(rky, "year", "tonnes", 5:dim(rky)[2])


  # 4-year rolling mean of data
  m <- rky %>%
    filter(!is.na(tonnes)) %>%
    mutate(year = as.numeric(as.character(year))) %>%
    group_by(rgn_id, species, species_code, sust_coeff) %>%
    arrange(rgn_id, species, species_code, year) %>%
    mutate(sm_tonnes = zoo::rollapply(tonnes, 4, mean, na.rm=TRUE, partial=TRUE)) %>%
    ungroup()

  # smoothed mariculture harvest * sustainability coefficient
  m <- m %>%
    mutate(sust_tonnes = sust_coeff * sm_tonnes)


  # aggregate all weighted timeseries per region, and divide by coastal human population
  ry = m %>%
    group_by(rgn_id, year) %>%
    summarize(sust_tonnes_sum = sum(sust_tonnes, na.rm=TRUE)) %>%  #na.rm = TRUE assumes that NA values are 0
    left_join(popn_inland25mi, by = c('rgn_id','year')) %>%
    mutate(mar_pop = sust_tonnes_sum / popsum) %>%
    filter(!is.na(popsum)) %>%
    ungroup()


  # get reference quantile based on argument years
  status_year<- max(ry$year)
  ref_95pct_data <- ry %>%
    group_by(rgn_id) %>%
    filter(year <= status_year) %>%
    ungroup()

  ref_95pct <- quantile(ref_95pct_data$mar_pop, 0.95, na.rm=TRUE)

  ry = ry %>%
    mutate(status = ifelse(mar_pop / ref_95pct > 1,
                           1,
                           mar_pop / ref_95pct))

  ## placeholder trend for ARC repo; see next for global status (ARC didn't have enough data)
  #status <- data.frame(rgn_id = 1:9,
                       #status = 30)

  ## global calculation for status
  status <- ry %>%
    group_by(rgn_id) %>% #group by rgn_id and add max year in to get status
  filter(year >= max(year, na.rm=T)) %>%
  dplyr::select(rgn_id, status) %>%
  mutate(status = round(status*100, 2)) %>%
    ungroup()




  # get MAR trend

  ## placeholder trend for ARC repo; see next for global trend (ARC didn't have enough data)
  #trend <- data.frame(rgn_id = 1:9,
                      #trend = 0)

  ## global calculation for trend
  trend = ry %>%
  group_by(rgn_id) %>%
  filter(!is.na(status)) %>%
  filter(year >= max(year, na.rm=T) - 4) %>%
  do(mdl = lm(status ~ year, data=.)) %>%
  summarize(rgn_id, trend = coef(mdl)['year'] * 5) %>%
  ungroup()

  trend <- trend %>%
  mutate(trend = ifelse(trend>1, 1, trend)) %>%
  mutate(trend = ifelse(trend<(-1), (-1), trend)) %>%
  mutate(trend = round(trend, 2))

  # return scores
  scores = status %>%
    dplyr::select(region_id = rgn_id,
           score     = status) %>%
    mutate(dimension='status') %>%
    rbind(
      trend %>%
        dplyr::select(region_id = rgn_id,
               score     = trend) %>%
        mutate(dimension = 'trend')) %>%
    mutate(goal='MAR')

  return(scores)
}

FP = function(layers, scores){

  # weights
  w <-  SelectLayersData(layers, layers='fp_wildcaught_weight', narrow = TRUE) %>%
    dplyr::select(region_id = id_num, w_FIS = val_num); head(w)

  # scores
  s <- scores %>%
    filter(goal %in% c('FIS', 'MAR')) %>%
    filter(!(dimension %in% c('pressures', 'resilience'))) %>%
    left_join(w, by="region_id")  %>%
    mutate(w_MAR = 1 - w_FIS) %>%
    mutate(weight = ifelse(goal == "FIS", w_FIS, w_MAR))


  ## Some warning messages due to potential mismatches in data:
  # NA score but there is a weight
  tmp <- filter(s, goal=='FIS' & is.na(score) & (!is.na(w_FIS) & w_FIS!=0) & dimension == "score")
  if(dim(tmp)[1]>0){
    warning(paste0("Check: these regions have a FIS weight but no score: ",
                   paste(as.character(tmp$region_id), collapse = ", ")))}

  tmp <- filter(s, goal=='MAR' & is.na(score) & (!is.na(w_MAR) & w_MAR!=0) & dimension == "score")
  if(dim(tmp)[1]>0){
    warning(paste0("Check: these regions have a MAR weight but no score: ",
                   paste(as.character(tmp$region_id), collapse = ", ")))}

  # score, but the weight is NA or 0
  tmp <- filter(s, goal=='FIS' & (!is.na(score) & score > 0) & (is.na(w_FIS) | w_FIS==0) & dimension == "score" & region_id !=0)
  if(dim(tmp)[1]>0){
    warning(paste0("Check: these regions have a FIS score but no weight: ",
                   paste(as.character(tmp$region_id), collapse = ", ")))}

  tmp <- filter(s, goal=='MAR' & (!is.na(score) & score > 0) & (is.na(w_MAR) | w_MAR==0) & dimension == "score" & region_id !=0)
  if(dim(tmp)[1]>0){
    warning(paste0("Check: these regions have a MAR score but no weight: ",
                   paste(as.character(tmp$region_id), collapse = ", ")))}

  s <- s  %>%
    group_by(region_id, dimension) %>%
    summarize(score = weighted.mean(score, weight, na.rm=TRUE)) %>%
    mutate(goal = "FP") %>%
    ungroup() %>%
    dplyr::select(region_id, goal, dimension, score) %>%
    data.frame()

  # return all scores
  return(rbind(scores, s))
}


AO = function(layers,
              status_year,
              Sustainability=1.0){

  # cast data
  layers_data = SelectLayersData(layers, targets='AO')

  year_min=max(min(layers_data$year, na.rm = TRUE), status_year - 10)

  r <- layers_data %>%
    filter(layer == 'ao_access') %>%
    dplyr::select(region_id=id_num, access=val_num)
  r <- na.omit(r)

  ry <- layers_data %>%
    filter(layer == 'ao_need') %>%
    dplyr::select(region_id = id_num, year, need=val_num) %>%
    left_join(r, by="region_id")


  # model

  ry <- ry %>%
    mutate(Du = (1 - need) * (1 - access)) %>%
    mutate(statusData = (1 - Du) * Sustainability)

  # status
  r.status <- ry %>%
    filter(year==status_year) %>%
    dplyr::select(region_id, statusData) %>%
    mutate(status=statusData*100)
  summary(r.status); dim(r.status)

  # trend
  r.trend <- ry %>%
    filter(year >= year_min) %>%
    filter(!is.na(statusData)) %>%
    group_by(region_id) %>%
    arrange(year) %>%
    top_n(5, year) %>%
    ungroup()


  r.trend <- r.trend %>%
    group_by(region_id) %>%
    do(mdl = lm(statusData ~ year, data=.)) %>%
    summarize( region_id = region_id,
               trend = coef(mdl)['year']*5) %>%
    ungroup()

  ## reference points
  rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
    rbind(data.frame(goal = "AO", method = "??",
                     reference_point = NA))
  write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)


  # return scores
  scores = r.status %>%
    dplyr::select(region_id, score=status) %>%
    mutate(dimension='status') %>%
    rbind(
      r.trend %>%
        dplyr::select(region_id, score=trend) %>%
        mutate(dimension='trend')) %>%
    mutate(goal='AO') # dlply(scores, .(dimension), summary)
  return(scores)
}

NP <- function(scores, layers, status_year, debug = FALSE){

  np_harvest = SelectLayersData(layers, layer ='np_harvest') %>%
    dplyr::select(rgn_id = id_num, year, species_code = category, score = val_num) %>%
    group_by(rgn_id, species_code)

  status = np_harvest %>%
    filter(year >= max(year, na.rm=T)) %>%
    mutate(score= round(score*100)) %>%
    dplyr::select(rgn_id, species_code, score) %>%
    data.frame() %>%
    tidyr::complete(rgn_id = full_seq(rgn_id, period = 1),  ## add regions without harvest, set as NA
                    fill = list(score = NA))

  trend = np_harvest %>%
    filter(year >= max(year, na.rm=T) - 4) %>%
    group_by(rgn_id, species_code) %>%
    do(mdl = lm(score ~ year, data=.)) %>%
    summarize(rgn_id, species_code,
              score = coef(mdl)['year'] * 5) %>%
    tidyr::complete(rgn_id = full_seq(rgn_id, period = 1),  ## add regions without harvest, set as NA
                    fill = list(score = NA)) %>%
    ungroup()

  trend <- trend %>%
    mutate(score = round(score, 2)) %>%
    mutate(dimension = 'trend') %>%
    mutate(score = ifelse(score < (-1), -1, score)) %>%
    mutate(score = ifelse(score > 1, 1, score)) %>%
    dplyr::select(rgn_id, species_code, dimension, score) %>%
    ungroup()%>%
    data.frame()

  # create scores variable, including summarizing by rgn_id, dimension
  scores = status %>%                              # combine status variable...
    dplyr::select(region_id=rgn_id, score) %>%
    mutate(dimension='status') %>%
    rbind(                                         # ...with trend variable
      trend %>%
        dplyr::select(region_id=rgn_id, score) %>%
        mutate(dimension='trend')) %>%
    group_by(region_id, dimension) %>%
    summarise(score = mean(score, na.rm=TRUE)) %>%  # summarize scores because of species_code
    mutate(goal='NP') %>%                           # add NP identifier
    dplyr::select(goal, dimension, region_id, score) %>%   # arrange as Toolbox expects
    data.frame()

  ## return scores
  return(scores)

}

CS <- function(layers){

  extent <- layers$data[['hab_extent']] %>%
    dplyr::select(rgn_id, habitat, km2) %>%
    mutate(habitat = as.character(habitat))

  health <-  layers$data[['hab_health']] %>%
    dplyr::select(rgn_id, habitat, health) %>%
    mutate(habitat = as.character(habitat))

  trend <-layers$data[['hab_trend']] %>%
    dplyr::select(rgn_id, habitat, trend) %>%
    mutate(habitat = as.character(habitat))

  # join layer data
  d <-  extent %>%
    full_join(health, by=c("rgn_id", "habitat")) %>%
    full_join(trend, by=c("rgn_id", "habitat"))

  # limit to CS habitats and add rank
  habitat.rank <- c('mangrove'         = 139,
                    'saltmarsh'        = 210,
                    'seagrass'         = 83)

  d <- d %>%
    filter(habitat %in% names(habitat.rank)) %>%
    mutate(
      rank = habitat.rank[habitat],
      extent = ifelse(km2==0, NA, km2))

  ## output file to temp folder that describes how much each habitat
  ## contributes to the score based on rank and extent
  ## this output is for the dataplayground website
  dp <- d %>%
    mutate(weighted_cont = rank*extent) %>%
    filter(!is.na(weighted_cont)) %>%
    group_by(rgn_id) %>%
    mutate(prop_score = weighted_cont/sum(weighted_cont)) %>%
    mutate(prop_score = round(prop_score, 3)) %>%
    dplyr::select(rgn_id, habitat, prop_score)
  write.csv(dp, 'temp/cs_hab_contributions.csv', row.names=FALSE)


  if (nrow(d) > 0){
    # status
    scores_CS <- d %>%
      filter(!is.na(rank) & !is.na(health) & !is.na(extent)) %>%
      group_by(rgn_id) %>%
      summarize(
        score = pmin(1, sum(rank * health * extent, na.rm=TRUE) / (sum(extent * rank, na.rm=TRUE)) ) * 100,
        dimension = 'status') %>%
      ungroup()

    # trend
    d_trend <- d %>%
      filter(!is.na(rank) & !is.na(trend) & !is.na(extent))
    if (nrow(d_trend) > 0 ){
      scores_CS <- rbind_list(
        scores_CS,
        d_trend %>%
          group_by(rgn_id) %>%
          summarize(
            score = sum(rank * trend * extent, na.rm=TRUE) / (sum(extent*rank, na.rm=TRUE)),
            dimension = 'trend')) %>%
        ungroup()
    } else { # if no trend score, assign NA
      scores_CS <- rbind_list(
        scores_CS,
        d %>%
          group_by(rgn_id) %>%
          summarize(
            score = NA,
            dimension = 'trend'))
    }

    ### output data file for checking and data review
    scores_check <- spread(scores_CS, dimension, score) %>%
      dplyr::select(rgn_id, status, trend_score=trend)

    d_check <- d %>%
      dplyr::select(rgn_id, habitat, extent, health, trend, rank) %>%
      arrange(rgn_id, habitat) %>%
      left_join(scores_check, by="rgn_id")

    ### end: output...

    scores_CS <- scores_CS %>%
      mutate(
        goal = 'CS') %>%
      dplyr::select(region_id=rgn_id, goal, dimension, score)
  } else {
    scores_CS <- data.frame(
      goal      = character(0),
      dimension = character(0),
      region_id = integer(0),
      score     = numeric())
  }

  ## reference points
  rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
    rbind(data.frame(goal = "CS", method = "Health/condition variable based on current vs. historic extent",
                     reference_point = "varies for each region/habitat"))
  write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)


  # return scores
  return(scores_CS)
}



CP <- function(layers){

  extent <- layers$data[['hab_extent']] %>%
    dplyr::select(rgn_id, habitat, km2) %>%
    mutate(habitat = as.character(habitat))

  # sum mangrove_offshore + mangrove_inland1km = mangrove to match with extent and trend
  mangrove_extent <- extent %>%
    filter(habitat %in% c('mangrove_inland1km','mangrove_offshore'))

  if (nrow(mangrove_extent) > 0){
    mangrove_extent <- mangrove_extent %>%
      group_by(rgn_id) %>%
      summarize(km2 = sum(km2, na.rm = TRUE)) %>%
      mutate(habitat='mangrove') %>%
      ungroup()
  }

  extent <- extent %>%
    filter(!habitat %in% c('mangrove','mangrove_inland1km','mangrove_offshore')) %>%  #do not use all mangrove
    rbind(mangrove_extent)  #just the inland 1km and offshore


  health <-  layers$data[['hab_health']] %>%
    dplyr::select(rgn_id, habitat, health) %>%
    mutate(habitat = as.character(habitat))

  trend <-layers$data[['hab_trend']] %>%
    dplyr::select(rgn_id, habitat, trend) %>%
    mutate(habitat = as.character(habitat))

  # join layer data
  d <-  extent %>%
    full_join(health, by=c("rgn_id", "habitat")) %>%
    full_join(trend, by=c("rgn_id", "habitat"))



  # limit to CP habitats and add rank
  habitat.rank <- c('coral'            = 4,
                    'mangrove'         = 4,
                    'saltmarsh'        = 3,
                    'seagrass'         = 1,
                    'seaice_shoreline' = 4)

  d <- d %>%
    filter(habitat %in% names(habitat.rank)) %>%
    mutate(
      rank = habitat.rank[habitat],
      extent = ifelse(km2==0, NA, km2))

  ## output file to temp folder that describes how much each habitat
  ## contributes to the score based on rank and extent
  ## this output is for the dataplayground website
  dp <- d %>%
    mutate(weighted_cont = rank*extent) %>%
    filter(!is.na(weighted_cont)) %>%
    group_by(rgn_id) %>%
    mutate(prop_score = weighted_cont/sum(weighted_cont)) %>%
    mutate(prop_score = round(prop_score, 3)) %>%
    dplyr::select(rgn_id, habitat, prop_score)
  write.csv(dp, 'temp/cp_hab_contributions.csv', row.names=FALSE)

  if (nrow(d) > 0){
    # status
    scores_CP <- d %>%
      filter(!is.na(rank) & !is.na(health) & !is.na(extent)) %>%
      group_by(rgn_id) %>%
      summarize(score = pmin(1, sum(rank * health * extent, na.rm=TRUE) / (sum(extent * rank, na.rm=TRUE)) ) * 100) %>%
      mutate(dimension = 'status') %>%
      ungroup()

    # trend
    d_trend <- d %>%
      filter(!is.na(rank) & !is.na(trend) & !is.na(extent))

    if (nrow(d_trend) > 0 ){
      scores_CP <- rbind_list(
        scores_CP,
        d_trend %>%
          group_by(rgn_id) %>%
          summarize(
            score = sum(rank * trend * extent, na.rm=TRUE) / (sum(extent*rank, na.rm=TRUE)),
            dimension = 'trend'))
    } else { # if no trend score, assign NA
      scores_CP <- rbind_list(
        scores_CP,
        d %>%
          group_by(rgn_id) %>%
          summarize(
            score = NA,
            dimension = 'trend'))
    }

    ### output data file for checking and data review
    scores_check <- spread(scores_CP, dimension, score) %>%
      dplyr::select(rgn_id, status, trend_score=trend)

    d_check <- d %>%
      dplyr::select(rgn_id, habitat, extent, health, trend, rank) %>%
      arrange(rgn_id, habitat) %>%
      left_join(scores_check, by="rgn_id")

    ### end: output...

    scores_CP <- scores_CP %>%
      mutate(
        goal = 'CP') %>%
      dplyr::select(region_id=rgn_id, goal, dimension, score)
  } else {
    scores_CP <- data.frame(
      goal      = character(0),
      dimension = character(0),
      region_id = integer(0),
      score     = numeric())
  }

  ## reference points
  rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
    rbind(data.frame(goal = "CP", method = "Health/condition variable based on current vs. historic extent",
                     reference_point = "varies for each region/habitat"))
  write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)


  # return scores
  return(scores_CP)
}


TR = function(layers, status_year, debug = FALSE, pct_ref = 90) {
  ## formula:
  ##  E   = Ep                         # Ep: % of direct tourism jobs. tr_jobs_pct_tourism.csv
  ##  S   = (S_score - 1) / (7 - 1)    # S_score: raw TTCI score, not normalized (1-7). tr_sustainability.csv
  ##  Xtr = E * S

  ## read in layers
  tr_data  <- full_join(
    layers$data[['tr_jobs_pct_tourism']] %>%
      dplyr::select(-layer),
    layers$data[['tr_sustainability']] %>%
      dplyr::select(-layer),
    by = c('rgn_id'))

  tr_model <- tr_data %>%
    mutate(
      E   = pct,
      S   = (S_score - 1) / (7 - 1), # scale score from 1 to 7.
      Xtr = E * S )
  # five data years for trend calcs



  ## No  travel warnings for arctic

  ## end if (exists('scenarios'))

  ### Calculate status based on quantile reference (see function call for pct_ref)
  pct_ref=90
  tr_model <- tr_model %>%
    dplyr::select(rgn_id, year, Xtr) %>%
    filter(year!=2015)%>% #only one with 2015 data so this automatically comes out as 1.0 in status
    left_join(tr_model %>%
                group_by(year) %>%
                summarize(Xtr_q = quantile(Xtr, probs = pct_ref/100, na.rm = TRUE)),
              by = 'year') %>%
    mutate(
      Xtr_rq  = ifelse(Xtr / Xtr_q > 1, 1, Xtr / Xtr_q)) # rescale to qth percentile, cap at 1

  ## reference points
  ref_point <- tr_model %>%
    group_by(rgn_id)%>%
    filter(year >= max(tr_model$year)) %>%
    dplyr::select(Xtr_q) %>%
    unique()
  rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
    rbind(data.frame(goal = "TR", method = paste0('spatial: ', pct_ref, "th quantile"),
                     reference_point = ref_point$Xtr_q))
  write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)



  # calculate trend
  tr_trend <- tr_model %>%
    group_by(rgn_id) %>% #group by rgn_id and add max year in to get status
    filter(year >= max(year, na.rm=T) -4)%>%
    ungroup()%>%
    filter(!is.na(Xtr_rq)) %>%
    arrange(year, rgn_id) %>%
    group_by(rgn_id) %>%
    do(mdl = lm(Xtr_rq ~ year, data=.)) %>%
    summarize(rgn_id, trend = coef(mdl)['year'] * 5)%>%
    ungroup() %>%
    mutate(trend = ifelse(trend>1, 1, trend)) %>%
    mutate(trend = ifelse(trend<(-1), (-1), trend)) %>%
    mutate(trend = round(trend, 4)) %>%
    dplyr::select(rgn_id, score = trend) %>%
    mutate(dimension = "trend")

  # get status (as last year's value)
  tr_status <- tr_model %>%
    arrange(year, rgn_id) %>%
    group_by(rgn_id) %>%
    summarize(
      dimension = 'status',
      score     = last(Xtr_rq) * 100)%>%
    ungroup()

  # bind status and trend by rows
  tr_score <- bind_rows(tr_status, tr_trend) %>%
    mutate(goal = 'TR')


  # return final scores
  scores = tr_score %>%
    dplyr::select(region_id=rgn_id, goal, dimension, score)

  return(scores)
}
LIV_ECO = function(layers, subgoal){

  ## read in all data:

  # gdp, wages, jobs and workforce_size data
  le_gdp   = SelectLayersData(layers, layers='le_gdp')  %>%
    dplyr::select(rgn_id = id_num, year, sector = category, gdp_usd = val_num) #add sector

  le_wages = SelectLayersData(layers, layers='le_wage_sector_year') %>%
    dplyr::select(rgn_id = id_num, year, sector = category, wage_usd = val_num)

  le_jobs  = SelectLayersData(layers, layers='le_jobs_sector_year') %>%
    dplyr::select(rgn_id = id_num, year, sector = category, jobs = val_num)

  le_workforce_size = SelectLayersData(layers, layers='le_workforcesize_adj') %>%
    dplyr::select(rgn_id = id_num, year, jobs_all = val_num)

  le_unemployment = SelectLayersData(layers, layers='le_unemployment') %>%
    dplyr::select(rgn_id = id_num, year, pct_unemployed = val_num)

  # debug
  #     le_gdp            = read.csv('eez2014/layers/le_gdp.csv')
  #     le_wages          = read.csv('eez2014/layers/le_wage_sector_year.csv')
  #     le_jobs           = read.csv('eez2014/layers/le_jobs_sector_year.csv')
  #     le_workforce_size = read.csv('eez2014/layers/le_workforcesize_adj.csv')
  #     le_unemployment   = read.csv('eez2014/layers/le_unemployment.csv')

  ##For reference - jobs_all = workforce size,

  # multipliers from Table S10 (Halpern et al 2012 SOM)
  #multipliers_jobs = data.frame('sector' = c('tour','cf', 'mmw', 'wte','mar'),
                                #'multiplier' = c(1, 1.582, 1.915, 1.88, 2.7)) # no multiplers for tour (=1)
  # multipliers_rev  = data.frame('sector' = c('mar', 'tour'), 'multiplier' = c(1.59, 1))
  # not used multipliers as not relevant for us.


  # calculate employment counts - this basically works out how many employed total in all sector (including non-marine)
  le_employed = le_workforce_size %>%
    left_join(le_unemployment, by = c('rgn_id', 'year')) %>%
    mutate(proportion_employed = (100 - pct_unemployed) / 100,
           employed            = jobs_all * proportion_employed)

  # reworded from SOM p.26-27
  #reference point for wages is the reference region (r) with the highest average wages across all sectors.
  #Reference points for jobs (j) and revenue (e) employ a moving baseline. The two metrics (j, e) are calculated
  #as relative values: the value in the current year (or most recent year), c, relative to the value in a recent
  #moving reference period, r, defined as 5 years prior to c. This reflects an implicit goal of maintaining coastal
  #livelihoods and economies (L&E) on short time scales, allowing for decadal or generational shifts in what people
  #want and expect for coastal L&E. The most recent year c must be 2000 or later in order for the data to be included.

  liv =
    # adjust jobs
    le_jobs %>%
    #left_join(multipliers_jobs, by = 'sector') %>%
    #mutate(jobs_mult = jobs * multiplier) %>%  # adjust jobs by multipliers # drop out multipliers
    left_join(le_employed, by= c('rgn_id', 'year')) %>%
    mutate(jobs_adj = jobs) %>% # adjust jobs by proportion employed - not sure why this is done?
    #remove * proportion unemployed as not accounting for multipliers
    left_join(le_wages, by=c('rgn_id','year','sector')) %>%
    arrange(year, sector, rgn_id)

  # LIV calculations ----

  # LIV status
  liv_status = liv %>%
    filter(!is.na(jobs_adj) & !is.na(wage_usd))
  # aia/subcountry2014 crashing b/c no concurrent wage data, so adding this check

 liv_status = liv_status %>%
      group_by(rgn_id)%>% #added group by rgn_id so that most recent years in each region are captured
      filter(year >= max(year, na.rm=T) - 4) %>%
   ungroup() %>%# reference point is 5 years ago Problem with this line of code is that different regions have different time periods
   #Talks about references just below?
      arrange(rgn_id, year, sector) %>%
      # summarize across sectors
      group_by(rgn_id, year) %>%
      summarize(
        # across sectors, jobs are summed
        jobs_sum  = sum(jobs_adj, na.rm=T),
        # across sectors, wages are averaged ##do we not want a weighted average?
        wages_avg = mean(wage_usd, na.rm=T)) %>% #this now then gives a score for marine jobs per rgn per year and average wage
      group_by(rgn_id) %>%
      arrange(rgn_id, year) %>%
      mutate(
        # reference for jobs [j]: value in the current year (or most recent year) [c], relative to the value in a recent moving reference period [r] defined as 5 years prior to [c]
        jobs_sum_first  = first(jobs_sum),                     # note:  `first(jobs_sum, order_by=year)` caused segfault crash on Linux with dplyr 0.3.0.2, so using arrange above instead
        # original reference for wages [w]: target value for average annual wages is the highest value observed across all reporting units
        # new reference for wages [w]: value in the current year (or most recent year) [c], relative to the value in a recent moving reference period [r] defined as 5 years prior to [c]
        wages_avg_first = first(wages_avg)) %>% # note:  `first(jobs_sum, order_by=year)` caused segfault crash on Linux with dplyr 0.3.0.2, so using arrange above instead
      # calculate final scores #first(jobs_sum) gives you the oldest not the newest.
      #ungroup() %>%
      mutate(
        x_jobs  = pmax(-1, pmin(1,  jobs_sum / jobs_sum_first)), #bounds answer between -1 and 1?
        x_wages = pmax(-1, pmin(1, wages_avg / wages_avg_first)),
        score   = mean(c(x_jobs, x_wages), na.rm=T) * 100) %>%
   ungroup()%>%
   group_by(rgn_id)%>% #add in group_by to ensure it doesn't just filter everything out
      # filter for most recent year
      filter(year == max(year, na.rm=T)) %>%
        ungroup()%>%
      # format
      dplyr::select(
        region_id = rgn_id,
        score) %>%
      mutate(
        goal      = 'LIV',
        dimension = 'status')

    # LIV trend
    # From SOM p. 29: trend was calculated as the slope in the individual sector values (not summed sectors)
    # over the most recent five years...
    # with the average weighted by the number of jobs in each sector
    # ... averaging slopes across sectors weighted by the revenue in each sector

    # get trend across years as slope of individual sectors for jobs and wages
    liv_trend = liv %>%
      filter(!is.na(jobs_adj) & !is.na(wage_usd)) %>%
      # TODO: consider "5 year time spans" as having 5 [(max(year)-4):max(year)] or 6 [(max(year)-5):max(year)] member years
      group_by(rgn_id)%>%
      filter(year >= max(year, na.rm=T) - 4) %>% # reference point is 5 years ago
      ungroup()%>%
      # get sector weight as total jobs across years for given region
      arrange(rgn_id, year, sector) %>%
      group_by(rgn_id, sector) %>%
      mutate(
        weight = sum(jobs_adj, na.rm=T)) %>%
      # reshape into jobs and wages columns into single metric to get slope of both with one do() call
      reshape2::melt(id=c('rgn_id','year','sector','weight'), variable='metric', value.name='value') %>%
      mutate(
        sector = as.character(sector),
        metric = as.character(metric)) %>%
      # get linear model coefficient per metric
      group_by(metric, rgn_id, sector, weight) %>%
      do(mdl = lm(value ~ year, data=.)) %>%
      summarize(
        metric = metric,
        weight = weight,
        rgn_id = rgn_id,
        sector = sector,
        # TODO: consider how the units affect trend; should these be normalized? cap per sector or later?
        sector_trend = pmax(-1, pmin(1, coef(mdl)[['year']] * 5))) %>%
      arrange(rgn_id, metric, sector) %>%
      # get weighted mean across sectors per region-metric
      group_by(metric, rgn_id) %>%
      summarize(
        metric_trend = weighted.mean(sector_trend, weight, na.rm=T)) %>%
      # get mean trend across metrics (jobs, wages) per region
      group_by(rgn_id) %>%
      summarize(
        score = mean(metric_trend, na.rm=T)) %>%
      # format
      mutate(
        goal      = 'LIV',
        dimension = 'trend') %>%
      dplyr::select(
        goal, dimension,
        region_id = rgn_id,
        score)



  # ECO calculations ----
  eco = le_gdp %>%
    mutate(
      rev_adj = gdp_usd)%>%
     # sector = 'gdp') %>%
    # adjust rev with national GDP rates if available. Example: (rev_adj = gdp_usd / ntl_gdp)
    dplyr::select(rgn_id, year, sector, rev_adj)

  # ECO status
  eco_status = eco %>%
    filter(!is.na(rev_adj)) %>%
    group_by(rgn_id)%>%
    filter(year >= max(year, na.rm=T) - 4) %>%
    ungroup() %>% # reference point is 5 years ago
    # across sectors, revenue is summed
    group_by(rgn_id, year) %>%
    summarize(
      rev_sum  = sum(rev_adj, na.rm=T)) %>%
    # reference for revenue [e]: value in the current year (or most recent year) [c], relative to the value in a recent moving reference period [r] defined as 5 years prior to [c]
    arrange(rgn_id, year) %>%
    group_by(rgn_id) %>%
    mutate(
      rev_sum_first  = first(rev_sum)) %>%
    # calculate final scores
    ungroup() %>%
    mutate(
      score  = pmin(rev_sum / rev_sum_first, 1) * 100) %>%
    # get most recent year
    group_by(rgn_id)%>%
    filter(year == max(year, na.rm=T)) %>%
    # format
    mutate(
      goal      = 'ECO',
      dimension = 'status') %>%
    dplyr::select(
      goal, dimension,
      region_id = rgn_id,
      score) %>%
    ungroup() # don't forget this! Was causing this Tbx error: Error in v$status/100 : non-numeric argument to binary operator

  # ECO trend
  eco_trend = eco %>%
    filter(!is.na(rev_adj)) %>%
    group_by(rgn_id)%>%
    filter(year >= max(year, na.rm=T) - 4 ) %>% # 5 year trend
    ungroup()%>%
    # get sector weight as total revenue across years for given region
    arrange(rgn_id, year, sector) %>%
    group_by(rgn_id, sector) %>%
    mutate(
      weight = sum(rev_adj, na.rm=T)) %>%
    # get linear model coefficient per region-sector
    group_by(rgn_id, sector, weight) %>%
    do(mdl = lm(rev_adj ~ year, data=.)) %>%
    summarize(
      weight = weight,
      rgn_id = rgn_id,
      sector = sector,
      # TODO: consider how the units affect trend; should these be normalized? cap per sector or later?
      sector_trend = pmax(-1, pmin(1, coef(mdl)[['year']] * 5))) %>%
    # get weighted mean across sectors per region
    group_by(rgn_id) %>%
    summarize(
      score = weighted.mean(sector_trend, weight, na.rm=T)) %>%
    # format
    mutate(
      goal      = 'ECO',
      dimension = 'trend') %>%
    dplyr::select(
      goal, dimension,
      region_id = rgn_id,
      score) %>%
    ungroup() # for good measure...

  # report LIV and ECO scores separately
  if (subgoal=='LIV'){
    d = rbind(liv_status, liv_trend)
  } else if (subgoal=='ECO'){
    d = rbind(eco_status, eco_trend)
  } else {
    stop('LIV_ECO function only handles subgoal of "LIV" or "ECO"')
  }
  return(d)

}


LE = function(scores, layers){

  # calculate LE scores
  scores.LE = scores %>%
    dplyr::filter(goal %in% c('LIV','ECO') & dimension %in% c('status','trend','score','future')) %>%
    tidyr::spread(key = goal, value = score) %>%
    dplyr::mutate(score = rowMeans(cbind(ECO, LIV), na.rm=T)) %>%
    dplyr::select(region_id, dimension, score) %>%
    dplyr::mutate(goal  = 'LE')

  # rbind to all scores
  scores = scores %>%
    rbind(scores.LE)

  # return scores
  return(scores)
}


ICO = function(layers){

  layers_data = SelectLayersData(layers, layers=c('ico_spp_extinction_status', 'ico_spp_popn_trend'))

  rk <- layers_data %>%
    dplyr::select(region_id = id_num, sciname = category, iucn_cat=val_chr, layer) %>%
    mutate(iucn_cat = as.character(iucn_cat))

  # lookup for weights status
  w.risk_category = data.frame(iucn_cat = c('LC', 'NT', 'VU', 'EN', 'CR', 'EX'),
                               risk_score = c(1,  0.8,   0.6,  0.4,  0.2,  0)) %>%
    mutate(iucn_cat = as.character(iucn_cat))

  # lookup for population trend
  w.popn_trend = data.frame(iucn_cat = as.character(c('decreasing', 'stable', 'increasing')),
                            trend_score = c(-0.5, 0, 0.5)) %>%
    mutate(iucn_cat = as.character(iucn_cat))

  ####### status
  # STEP 1: take mean of subpopulation scores
  r.status_spp <- rk %>%
    filter(layer == 'ico_spp_extinction_status') %>%
    left_join(w.risk_category, by = 'iucn_cat') %>%
    group_by(region_id, sciname) %>%
    summarize(spp_mean = mean(risk_score, na.rm=TRUE) * 100) %>%
    ungroup()

  # STEP 2: take mean of populations within regions
  r.status <- r.status_spp %>%
    group_by(region_id) %>%
    summarize(score = mean(spp_mean, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(dimension = "status")

  ####### trend
  # STEP 1: take mean of subpopulation scores
  r.trend_spp <- rk %>%
    filter(layer == 'ico_spp_popn_trend') %>%
    left_join(w.popn_trend ,by = 'iucn_cat') %>%
    group_by(region_id, sciname) %>%
    summarize(spp_mean = mean(trend_score, na.rm=TRUE)) %>%
    ungroup()

  # STEP 2: take mean of populations within regions
  r.trend <- r.trend_spp %>%
    group_by(region_id) %>%
    summarize(score = mean(spp_mean, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(dimension = "trend")

  ## reference points
  rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
    rbind(data.frame(goal = "ICO", method = "scaled IUCN risk categories",
                     reference_point = NA))
  write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)


  # return scores
  scores <-  rbind(r.status, r.trend) %>%
    mutate('goal'='ICO') %>%
    dplyr::select(goal, dimension, region_id, score) %>%
    data.frame()

  return(scores)

}

LSP = function(layers, ref_pct_cmpa=30, ref_pct_cp=30, status_year=2014){

  trend_years = (status_year-4):status_year

  # select data ----
  r = SelectLayersData(layers, layers=c('rgn_area_offshore3nm'))  #total offshore/inland areas
  ry = SelectLayersData(layers, layers=c('lsp_prot_area_offshore3nm')) #total protected areas

  r <- r %>%
    dplyr::select(region_id = id_num, year, val_num, layer) %>%
    spread(layer, val_num) %>%
    dplyr::select(region_id, area_offshore3nm = rgn_area_offshore3nm)%>%
    unique()

  ry <- ry %>%
    dplyr::select(region_id = id_num, year, val_num, layer) %>%
    spread(layer, val_num) %>%
    dplyr::select(region_id, year, cmpa = lsp_prot_area_offshore3nm)

  # fill in time series for all regions and generate cumulative sum
 # r.yrs <- expand.grid(region_id = unique(ry$region_id),
  #                     year = unique(ry$year)) %>%
   # left_join(ry, by=c('region_id', 'year')) %>%
    #arrange(region_id, year) %>%
    #mutate(cmpa = ifelse(is.na(cmpa), 0, cmpa)) %>%
    #group_by(region_id) %>%
    #mutate(cmpa_cumsum  = cumsum(cmpa)) %>%
    #ungroup() %>%
    #mutate(pa_cumsum     = cmpa_cumsum)

  # get percent of total area that is protected for offshore3nm (cmpa) per year
  # and calculate status score
  r.yrs = ry %>%
    full_join(r, by="region_id") %>%
    mutate(pct_cmpa  = pmin(cmpa / area_offshore3nm * 100, 100),
           prop_protected    = (pmin(pct_cmpa / ref_pct_cmpa, 1)))

  # extract status based on specified year

  r.status = r.yrs %>%
    filter(year==status_year) %>%
    dplyr::select(region_id, status=prop_protected) %>%
    mutate(status=status*100)

  # calculate trend
  r.trend =   r.yrs %>%
    filter(year %in% trend_years) %>%
    group_by(region_id) %>%
    do(mdl = lm(prop_protected ~ year, data=.)) %>%
    summarize(
      region_id = region_id,
      trend = min(1, max(0, 5*coef(mdl)['year']))) %>% # set boundaries so trend does not go below 0 or above 1
    ungroup()

  ## reference points
  rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
    rbind(data.frame(goal = "LSP", method = paste0(ref_pct_cmpa, "% marine protected area; ",
                                                   ref_pct_cp, "% coastal protected area"),
                     reference_point = "varies by area of region's eez and 1 km inland"))
  write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)


  # return scores
  scores = bind_rows(
    within(r.status, {
      goal      = 'LSP'
      dimension = 'status'
      score     = status}),
    within(r.trend, {
      goal      = 'LSP'
      dimension = 'trend'
      score     = trend}))
  return(scores[,c('region_id','goal','dimension','score')])
}

SP = function(scores){

  s <- scores %>%
    filter(goal %in% c('ICO','LSP'),
           dimension %in% c('status', 'trend', 'future', 'score')) %>%
    group_by(region_id, dimension) %>%
    summarize(score = mean(score, na.rm=TRUE)) %>%
    ungroup() %>%
    arrange(region_id) %>%
    mutate(goal = "SP") %>%
    dplyr::select(region_id, goal, dimension, score) %>%
    data.frame()

  # return all scores
  return(rbind(scores, s))

}


CW = function(layers){

  # layers
  lyrs <- c('po_pathogens', 'po_nutrients_3nm', 'po_chemicals_3nm', 'po_trash',
            'cw_chemical_trend', 'cw_nutrient_trend', 'cw_trash_trend', 'cw_pathogen_trend')

  d <-  SelectLayersData(layers, layers=lyrs)  %>%
    dplyr::select(region_id = id_num, layer, value = val_num)

  ### function to calculate geometric mean:
  geometric.mean2 <- function (x, na.rm = TRUE) {
    if (is.null(nrow(x))) {
      exp(mean(log(x), na.rm = TRUE))
    }
    else {
      exp(apply(log(x), 2, mean, na.rm = na.rm))
    }
  }


  d_pressures <- d %>%
    filter(layer %in% grep('po_', lyrs, value=TRUE))  %>%
    mutate(pressure = 1 - value) %>%  # invert pressures
    group_by(region_id) %>%
    summarize(score = geometric.mean2(pressure, na.rm=TRUE)) %>% # take geometric mean
    mutate(score = score * 100) %>%
    mutate(dimension = "status") %>%
    ungroup()

  d_trends <- d %>%
    filter(layer %in% grep('_trend', lyrs, value=TRUE)) %>%
    mutate(trend = -1 * value)  %>%  # invert trends
    group_by(region_id) %>%
    summarize(score = mean(trend, na.rm = TRUE)) %>%
    mutate(dimension = "trend") %>%
    ungroup()


  # return scores
  scores = rbind(d_pressures, d_trends) %>%
    mutate(goal = "CW") %>%
    dplyr::select(region_id, goal, dimension, score) %>%
    data.frame()

  ## reference points
  rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
    rbind(data.frame(goal = "CW", method = "spatial: pressures scaled from 0-1 at raster level",
                     reference_point = NA))
  write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)

  return(scores)
}


HAB = function(layers){

  ## get the data:
  health <-  layers$data[['hab_health']] %>%
    dplyr::select(rgn_id, habitat, health) %>%
    mutate(habitat = as.character(habitat))

  trend <-  layers$data[['hab_trend']] %>%
    dplyr::select(rgn_id, habitat, trend) %>%
    mutate(habitat = as.character(habitat))

  extent <- layers$data[['hab_extent']] %>%
    dplyr::select(rgn_id, habitat, extent=km2) %>%
    mutate(habitat = as.character(habitat))

  # join and limit to HAB habitats
  d <- health %>%
    full_join(trend, by = c('rgn_id', 'habitat')) %>%
    full_join(extent, by = c('rgn_id', 'habitat')) %>%
    filter(habitat %in% c('coral','mangrove','saltmarsh','seaice_edge','seagrass','soft_bottom')) %>%
    mutate(w  = ifelse(!is.na(extent) & extent > 0, 1, NA)) %>%
    filter(!is.na(w))

  if(sum(d$w %in% 1 & is.na(d$trend)) > 0){
    warning("Some regions/habitats have extent data, but no trend data.  Consider estimating these values.")
  }

  if(sum(d$w %in% 1 & is.na(d$health)) > 0){
    warning("Some regions/habitats have extent data, but no health data.  Consider estimating these values.")
  }

  d <- d %>%
    group_by(rgn_id)


  # calculate scores

  status <- d %>%
    filter(!is.na(health)) %>%
    summarize(
      score = pmin(1, sum(health) / sum(w)) * 100,
      dimension = 'status')

  trend <- d %>%
    filter(!is.na(trend)) %>%
    summarize(
      score =  sum(trend) / sum(w),
      dimension = 'trend')

  scores_HAB <- rbind(status, trend) %>%
    mutate(goal = "HAB") %>%
    dplyr::select(region_id=rgn_id, goal, dimension, score)

  ## reference points
  rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
    rbind(data.frame(goal = "HAB", method = "Health/condition variable based on current vs. historic extent",
                     reference_point = "varies for each region/habitat"))
  write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)

  # return scores
  return(scores_HAB)
}


SPP = function(layers){
  scores <-   SelectLayersData(layers, layers=c('spp_status'='status','spp_trend'='trend'), narrow = TRUE) %>%
    dplyr::select(region_id = id_num, dimension = layer, score = val_num) %>%
    mutate(goal = 'SPP') %>%
    mutate(score = ifelse(dimension == 'status', score*100, score))

  ## reference points
  rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
    rbind(data.frame(goal = "SPP", method = "Average of IUCN risk categories, scaled to historic extinction",
                     reference_point = NA))
  write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)


  return(scores)
}

BD = function(scores){
  d <- scores %>%
    filter(goal %in% c('HAB', 'SPP')) %>%
    filter(!(dimension %in% c('pressures', 'resilience'))) %>%
    group_by(region_id, dimension) %>%
    summarize(score = mean(score, na.rm=TRUE)) %>%
    mutate(goal = 'BD') %>%
    data.frame()

  # return all scores
  return(rbind(scores, d[,c('region_id','goal','dimension','score')]))
}

PreGlobalScores = function(layers, conf, scores){

  # get regions
  rgns = SelectLayersData(layers, layers=conf$config$layer_region_labels, narrow = TRUE)

  # limit to just desired regions and global (region_id==0)
  scores = subset(scores, region_id %in% c(rgns[,'id_num'], 0))

  # apply NA to Antarctica
  id_ant = subset(rgns, val_chr=='Antarctica', id_num, drop = TRUE)
  scores[scores$region_id==id_ant, 'score'] = NA

  return(scores)
}

FinalizeScores = function(layers, conf, scores){

  # get regions
  rgns = SelectLayersData(layers, layers=conf$config$layer_region_labels, narrow = TRUE)

  # add NAs to missing combos (region_id, goal, dimension)
  d = expand.grid(list(score_NA  = NA,
                       region_id = c(rgns[,'id_num'], 0),
                       dimension = c('pressures','resilience','status','trend','future','score'),
                       goal      = c(conf$goals$goal, 'Index')), stringsAsFactors = FALSE); head(d)
  d = subset(d,
             !(dimension %in% c('pressures','resilience','trend') & region_id==0) &
               !(dimension %in% c('pressures','resilience','status','trend') & goal=='Index'))
  scores = merge(scores, d, all = TRUE)[,c('goal','dimension','region_id','score')]

  # order
  scores = arrange(scores, goal, dimension, region_id)

  # round scores
  scores$score = round(scores$score, 2)

  return(scores)
}
