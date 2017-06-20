AO = function(layers,
              status_year,
              Sustainability=1.0){

  ### AO consists of 1. sustainability of artisinally targetted fish species 2. Extinction risk of artisinally targetted marine mammals 3. Amount of sea ice edge

  ##Same model for artisinal fish as FP - do this first
  #catch data
  c<- read.csv('prep/AO/fish/ao_meancatch_arc2016.csv') %>%
    dplyr::select(
      rgn_id,
      stock_id_taxonkey,
      year,
      catch          = mean_catch)
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
  b$score = ifelse(b$rgn_id >= 1 & b$bmsy >1, 1, b$score)


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
    mutate(gap_fill = ifelse(is.na(penalty), "none", "Median gapfilled")) %>%
    dplyr::select(rgn_id, stock_id, taxon_key, year, catch, score, gap_fill) %>%
    filter(year == status_year)
  write.csv(gap_fill_data, 'temp/AO_summary_gf.csv', row.names=FALSE)

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
    summarize(status = weighted.mean(score, wprop)) %>%
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
  scores_ao_fis= full_join(status, trend)%>%
    mutate(goal='AO_fis')%>%
    data.frame()

  ######ARTISINALLY TARGETTED MARINE MAMMALS#####

  layers_data = SelectLayersData(layers, layers=c('ao_mammals_iucn_status_arc2016'))
  status_year<- 2016
  rk <- layers_data %>%
    select(region_id = id_num, sciname = category, iucn_cat=val_chr, year, layer) %>%
    mutate(iucn_cat = as.character(iucn_cat))

  # lookup for weights status
  #  LC <- "LOWER RISK/LEAST CONCERN (LR/LC)"
  #  NT <- "LOWER RISK/NEAR THREATENED (LR/NT)"
  #  T  <- "THREATENED (T)" treat as "EN"
  #  VU <- "VULNERABLE (V)"
  #  EN <- "ENDANGERED (E)"
  #  LR/CD <- "LOWER RISK/CONSERVATION DEPENDENT (LR/CD)" treat as between VU and NT
  #  CR <- "VERY RARE AND BELIEVED TO BE DECREASING IN NUMBERS"
  #  DD <- "INSUFFICIENTLY KNOWN (K)"
  #  DD <- "INDETERMINATE (I)"
  #  DD <- "STATUS INADEQUATELY KNOWN-SURVEY REQUIRED OR DATA SOUGHT"
  w.risk_category = data.frame(iucn_cat = c('LC', 'NT', 'CD', 'VU', 'EN', 'CR', 'EX', 'DD'),
                               risk_score = c(0,  0.2,  0.3,  0.4,  0.6,  0.8,  1, NA)) %>%
    mutate(status_score = 1-risk_score) %>%
    mutate(iucn_cat = as.character(iucn_cat))

  ####### status
  # STEP 1: take mean of subpopulation scores
  r.status_spp <- rk %>%
    left_join(w.risk_category, by = 'iucn_cat') %>%
    group_by(region_id, sciname, year) %>%
    summarize(spp_mean = mean(status_score, na.rm=TRUE)) %>%
    ungroup()

  # STEP 2: take mean of populations within regions
  r.status <- r.status_spp %>%
    group_by(region_id, year) %>%
    summarize(score = mean(spp_mean, na.rm=TRUE)) %>%
    ungroup()

  ####### trend
  trend_years <- c(status_year:(status_year - 9)) # trend based on 10 years of data, due to infrequency of IUCN assessments
  adj_trend_year <- min(trend_years)


  r.trend <- r.status %>%
    group_by(region_id) %>%
    do(mdl = lm(score ~ year, data=., subset=year %in% trend_years),
       adjust_trend = .$score[.$year == adj_trend_year]) %>%
    summarize(region_id,
              trend = ifelse(coef(mdl)['year']==0, 0, coef(mdl)['year']/adjust_trend * 5)) %>%
    ungroup() %>%
    mutate(trend = ifelse(trend>1, 1, trend)) %>%
    mutate(trend = ifelse(trend<(-1), (-1), trend)) %>%
    mutate(trend = round(trend, 4)) %>%
    select(region_id, score = trend) %>%
    mutate(dimension = "trend")


  ####### status
  r.status <- r.status %>%
    filter(year == status_year) %>%
    mutate(score = score * 100) %>%
    mutate(dimension = "status") %>%
    select(region_id, score, dimension)

  ## reference points
  rp <- read.csv('temp/referencePoints.csv', stringsAsFactors=FALSE) %>%
    rbind(data.frame(goal = "AO", method = "scaled IUCN risk categories",
                     reference_point = NA))
  write.csv(rp, 'temp/referencePoints.csv', row.names=FALSE)


  # return scores
  scores_ao_mammals <-  rbind(r.status, r.trend) %>%
    mutate('goal'='AO_mammals') %>%
    select(goal, dimension, region_id, score) %>%
    data.frame()

  ###Finally use sea ice edge scores for coastal protection#######

  scores_ao_ice<- read_csv('prep/AO/scores_cp.csv')
  scores_ao_ice<- dplyr::rename(scores_ao_ice, region_id=rgn_id)
  scores_ao_ice$goal<- 'ao_ice'


  write_csv(scores_ao_fis, 'prep/AO/scores_ao_fis.csv')
  write_csv(scores_ao_ice, 'prep/AO/scores_ao_ice.csv')
  write_csv(scores_ao_mammals, 'prep/AO/scores_ao_mammals.csv')
