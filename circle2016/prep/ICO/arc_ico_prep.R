####ICONIC SPECIES PREP####



knitr::opts_chunk$set(fig.width = 6, fig.height = 4, fig.path = 'Figs/',
                      echo = FALSE, message = FALSE, warning = FALSE)

source('~/github/ohiprep/src/R/common.R')
library(readr)

goal      <- 'globalprep/spp_ico'
scenario  <- 'v2016'
dir_anx   <- file.path(dir_M, 'git-annex', goal)
dir_goal  <- file.path('~/github/ohiprep', goal, scenario)

##Identify countries with extant ICO species populations

source(file.path(dir_goal, 'ico_fxn.R'))


get_from_api <- function(url, param) {
  api_info <- fromJSON(sprintf(url, param, api_key)) %>%
    data.frame(stringsAsFactors = FALSE)
}

mc_get_from_api <- function(url, param_vec) {
  numcores = ifelse(Sys.info()[['nodename']] == 'mazu', 12, 1)
  out_df <- parallel::mclapply(param_vec,
                               function(x) get_from_api(url, x),
                               mc.cores = numcores) %>%
    bind_rows()
  out_df <- out_df %>%
    setNames(names(.) %>%
               str_replace('result.', ''))
}

library(jsonlite)
### api_key stored on git-annex so outside users can use their own key
api_key <- scan(file.path(dir_anx, 'api_key.csv'), what = 'character')


###Filtering the complete IUCN species list to include only the identified Iconic Species, we then use the IUCN API
##to access the list of countries in which each species occurs,
##from http://apiv3.iucnredlist.org/api/v3/species/countries/id/?token=.
##The country list identifies whether the species' presence in that country is "Extant", "Extinct Post-1500", or "Possibly Extinct";
##the "Extinct Post-1500" presence will be used later to identify locally extinct populations.


spp_df_all <- read_csv(file.path(dir_goal, 'int/spp_list_from_api.csv'))
ico_list_raw <- read_csv(file.path('prep/ICO/ico_list_raw.csv'))

spp_ico <- spp_df_all %>%
  filter(sciname %in% ico_list_raw$sciname)

spp_missing <- ico_list_raw %>%
  filter(!sciname %in% spp_ico$sciname)
# after adding in BHI species, Sprat (Sprattus sprattus) is not found.  Identify with a different species name?

ico_list <- ico_list_raw %>%
  left_join(spp_ico %>%
              dplyr::select(iucn_sid, sciname, subpop = population, cat = category),
            by = 'sciname') %>%
  filter(!is.na(iucn_sid))

write_csv(ico_list, file.path('prep/ICO/ico_list_prepped.csv'))

### for each species ID, get country list
ico_country_url <- 'http://apiv3.iucnredlist.org/api/v3/species/countries/id/%s?token=%s'

ico_spp_countries <- mc_get_from_api(ico_country_url, ico_list$iucn_sid)

rgn_iucn2ohi <- read_csv(file.path('prep/ICO/rgns_iucn2arc.csv'))

ico_spp_rgn_raw <- ico_spp_countries %>%
  dplyr::select(-code, -count, iucn_sid = name, iucn_rgn_name = country) %>%
  mutate(iucn_sid = as.integer(iucn_sid),
         iucn_rgn_name  = str_trim(iucn_rgn_name)) %>%
  left_join(rgn_iucn2ohi,
            by = 'iucn_rgn_name')

### Error check on region name matching
non_match <- ico_spp_rgn_raw %>%
  filter(is.na(ohi_rgn_name))
if(nrow(non_match) > 0) {
  cat('The following IUCN countries did not match with OHI region names:\n  ')
  print(paste(non_match$iucn_rgn_name %>% unique(), collapse = ', '))
}

ico_spp_rgn_raw <- ico_spp_rgn_raw %>%
  rename(rgn_name = ohi_rgn_name) %>%
  select(-iucn_rgn_name) %>%
  filter(!is.na(rgn_id)) %>%
  distinct()

write_csv(ico_spp_rgn_raw, file.path('prep/ICO/ico_spp_rgn_raw.csv'))

ico_spp_rgn_raw <- read_csv(file.path('prep/ICO/ico_spp_rgn_raw.csv'))
ico_list <- read_csv(file.path('prep/ICO/ico_list_prepped.csv'))

ico_spp_rgn_prepped <- ico_spp_rgn_raw %>%
  left_join(ico_list,
            by = 'iucn_sid')

### filter this for species who are global (so all instances are iconic)
###   OR ico_rgn_id matches rgn_id (so locally iconic matches with location)
ico_spp_rgn_prepped <- ico_spp_rgn_prepped %>%
  filter(ico_gl == TRUE | ico_rgn_id == rgn_id)

write_csv(ico_spp_rgn_prepped, file.path('prep/ICO/ico_spp_rgn_prepped.csv'))

### for each species ID, get past assessments
ico_past_assess_url <- 'http://apiv3.iucnredlist.org/api/v3/species/history/id/%s?token=%s'
ico_list <- read_csv(file.path('prep/ICO/ico_list_prepped.csv'))

ico_assess_raw <- mc_get_from_api(ico_past_assess_url, ico_list$iucn_sid)

ico_assess_raw <- ico_assess_raw %>%
  rename(iucn_sid = name) %>%
  mutate(iucn_sid = as.integer(iucn_sid),
         year     = as.integer(year)) %>%
  left_join(ico_list %>%
              dplyr::select(iucn_sid, sciname) %>%
              distinct(),
            by = 'iucn_sid')

write_csv(ico_assess_raw, file.path('prep/ICO/ico_assessments_raw.csv'))

DT::datatable(ico_assess_raw, caption = 'ICO species and past IUCN assessments')
#New category <- original category/description
#     NT     <- "LOWER RISK/NEAR THREATENED (LR/NT)"
#      T     <- "THREATENED (T)" treat as "EN"
#     VU     <- "VULNERABLE (V)"
#     EN     <- "ENDANGERED (E)"
#  LR/CD     <- "LOWER RISK/CONSERVATION DEPENDENT (LR/CD)" treat as between VU and NT
#     CR     <- "VERY RARE AND BELIEVED TO BE DECREASING IN NUMBERS"
#      T     <- "LESS RARE BUT BELIEVED TO BE THREATENED-REQUIRES WATCHING"
#     DD     <- "INSUFFICIENTLY KNOWN (K)"
#     DD     <- "INDETERMINATE (I)"
#     DD     <- "STATUS INADEQUATELY KNOWN-SURVEY REQUIRED OR DATA SOUGHT"
#     NE     <- "NOT RECOGNIZED (NR)"

### Clean up the time series
### iucn_sid | year | code | category | sciname

ico_assess_raw <- read_csv(file.path('prep/ICO/ico_assessments_raw.csv'))

ico_assess <- ico_assess_raw %>%
  rename(cat = code, cat_txt = category) %>%
  mutate(cat = toupper(cat),
         cat = str_replace(cat, 'LR/', ''),
         cat = ifelse(cat %in% c('K', 'I'), 'DD', cat),
         cat = ifelse(cat == 'NR', 'NE', cat),
         cat = ifelse(str_detect(toupper(cat_txt), 'VERY RARE'), 'CR', cat),
         cat = ifelse(str_detect(toupper(cat_txt), 'LESS RARE'), 'T', cat),
         cat = ifelse(str_detect(toupper(cat_txt), 'STATUS INADEQUATELY KNOWN'), 'DD', cat),
         cat = ifelse(cat == 'V', 'VU', cat),
         cat = ifelse(cat == 'E', 'EN', cat))


pop_cat <- data.frame(cat       = c("LC", "NT", "VU", "EN", "CR", "EX", "T", "CD", "NE", "DD"),
                      cat_score = c(   0,  0.2,  0.4,  0.6,  0.8,  1.0, 0.6,  0.3,   NA,  NA),
                      stringsAsFactors = FALSE)

ico_assess <- ico_assess %>%
  left_join(pop_cat, by = 'cat') %>%
  filter(!is.na(cat_score)) %>%
  distinct() %>%
  arrange(iucn_sid, year)

write_csv(ico_assess, file.path('prep/ICO/ico_assess_clean.csv'))

#####Using tidyr::complete() and tidyr::fill(), we create a full time series for all species from the earliest assessment to the most recent year.###
ico_assess <- read_csv(file.path('prep/ICO/ico_assess_clean.csv'))
ico_list <- read_csv(file.path('prep/ICO/ico_list_prepped.csv'))

ico_assess_full <- ico_assess %>%
  dplyr::select(-sciname) %>%
  arrange(iucn_sid, year) %>%
  complete(year = full_seq(year, 1), nesting(iucn_sid)) %>%
  group_by(iucn_sid) %>%
  fill(cat, cat_txt, cat_score) %>% ### fills all the way to latest year (2015)
  ungroup()

ico_spp_cat <- ico_list %>%
  rename(cat_2016 = cat) %>%
  left_join(ico_assess_full, by = c('iucn_sid'))

### if no time series available, time series years will be NA.  Assign a list to
### those NAs, then unnest it to create observations for those years.
ico_spp_cat <- ico_spp_cat %>%
  mutate(year = ifelse(is.na(year),
                       list(c(min(year, na.rm = TRUE):max(year, na.rm = TRUE))),
                       year)) %>%
  unnest(year)

### NAs will be filled backward in time by starting from the most recent non-NA.
### To do this, we'll swap any current-year NAs with the cat_score (meaning no
### time series fill), and fill upwards instead of downwards.
ico_spp_cat <- ico_spp_cat %>%
  left_join(pop_cat %>%
              rename(cat_2016 = cat, cat_2016_score = cat_score),
            by = 'cat_2016') %>%
  mutate(cat_score = ifelse(year == max(year, na.rm = TRUE) & is.na(cat),
                            cat_2016_score,
                            cat_score)) %>%
  arrange(iucn_sid, year) %>%
  group_by(iucn_sid) %>%
  fill(cat, cat_score, cat_txt, .direction = 'up') %>%
  ungroup() %>%
  distinct()

write_csv(ico_spp_cat, file.path('prep/ICO/ico_spp_cat.csv'))

ico_cat_ts_abbr <- read_csv(file.path('prep/ICO/ico_spp_cat.csv')) %>%
  dplyr::select(iucn_sid, sciname, year, cat, cat_score) %>%
  filter(year >= 2000)

ico_spp_rgn <- read_csv(file.path('prep/ICO/ico_spp_rgn_prepped.csv')) %>%
  dplyr::select(rgn_id, ohi_rgn_name, iucn_sid, comname, sciname, ico_gl, ico_rgn_id, presence)

ico_spp_rgn_cat <- ico_cat_ts_abbr %>%
  full_join(ico_spp_rgn, by = c('iucn_sid', 'sciname'))

# ico_2015 <- ico_spp_rgn_cat %>%
#   filter(year == 2015)
#
# ex <- ico_spp_rgn_cat %>%
#   filter(str_detect(tolower(distribution_code), 'extinct')) %>%
#   filter(year == 2015)
#
# ex2 <- ico_spp_rgn_cat %>%
#   filter(sciname %in% ex$sciname) %>%
#   filter(rgn_name %in% ex$rgn_name) %>%
#   filter(year == 2015)
### How to deal with "extinct" locally?  when did species go extinct?
### But we're only really looking at the last ten-fifteen years, so
### maybe not important - just set all years to extinct for that region

ico_spp_rgn_cat <- ico_spp_rgn_cat %>%
  mutate(cat = ifelse(str_detect(presence, '^Extinct'), 'EX', cat), ### ^ indicates start of string
         cat_score = ifelse(cat == 'EX', 1, cat_score)) %>%
  filter(ico_gl | ico_rgn_id == rgn_id) %>% ### Keep (all globally iconic) and (regionally iconic in region only)
  distinct()

write_csv(ico_spp_rgn_cat, file.path('prep/ICO/ico_spp_rgn_cat.csv'))

### Report and summarize estimate of regional iconic species status

ico_spp_rgn_cat <- read_csv(file.path('prep/ICO/ico_spp_rgn_cat.csv'))

# Report out for toolbox format (rgn_id | sciname | category or popn_trend for each species within a region).
# Note: in toolbox, group_by(rgn_id, sciname) and then summarize(category = mean(category)) to
#   average any parent/subpop species listings before aggregating to overall average per region.

ico_status_raw <- ico_spp_rgn_cat %>%
 dplyr::select(rgn_id, ohi_rgn_name, sciname, iucn_sid, cat, cat_score, year) %>%
  arrange(rgn_id, desc(year), sciname) %>%
  ungroup()

ico_status_calc <- ico_status_raw %>%
  group_by(rgn_id, ohi_rgn_name, sciname, year) %>%
  filter(!is.na(cat_score)) %>% ### remove any DDs
  summarize(cat_score = mean(cat_score)) %>%
  group_by(rgn_id, ohi_rgn_name, year) %>%
  summarize(mean_cat = round(mean(cat_score), 5),
            ico_status = (1 - mean_cat) * 100,
            n_spp = n()) %>%
  ungroup()%>%
  filter(!is.na(rgn_id))


ico_trend <- data.frame()
for (i in 2010:max(ico_status_calc$year, na.rm = TRUE)) { # i <- 2013
  tmp_status <- ico_status_calc %>%
    filter(year <= i & year > (i - 10)) ### trend based on 10-year average since assessments are sporadic
  tmp_trend <- tmp_status %>%
    group_by(rgn_id) %>%
    do(trend_lm = lm(ico_status ~ year, data = .)$coefficients[2]) %>%
    mutate(year  = i,
           trend_lm  = as.numeric(trend_lm)/100, ### status is 0 - 100; trend should be +1 to -1
           ico_trend = round(trend_lm * 5, 5)) %>%   ### trend prediction five years out
    ungroup()
  ico_trend <- ico_trend %>%
    bind_rows(tmp_trend)
}

ico_sum <- ico_status_raw %>%
  left_join(ico_status_calc, by = c('rgn_id', 'ohi_rgn_name', 'year')) %>%
  left_join(ico_trend, by = c('rgn_id', 'year'))

write_csv(ico_sum, file.path('prep/ICO/ico_summary.csv'))
# Report out for finalized status and trend values per region.

ico_status_raw1 <- ico_status_raw %>%
  dplyr::select(rgn_id, sciname, iucn_sid, year, category = cat) %>%
  filter(!is.na(rgn_id))

ico_trend<- ico_trend%>%
  filter(!is.na(rgn_id))

write_csv(ico_status_raw1, file.path('prep/ICO/output/ico_spp_iucn_status.csv'))
write_csv(ico_status_calc, file.path('prep/ICO/output/ico_status_calc.csv'))
write_csv(ico_trend,       file.path('prep/ICO/output/ico_trend.csv'))


ico_status_raw1[duplicated(ico_status_raw1 ), ]
### NOTE: if iucn_sid were removed, this would show duplicates due to subpops
### with same category.
table(ico_status_raw1$category)
DT::datatable(ico_status_raw %>% filter(year == 2015))

library(ggplot2)
library(plotly)

status_ts_plot <- ggplot(ico_sum %>%
                           filter(!is.na(rgn_id)),
                         aes(x = year, y = ico_status, color = rgn_id, group = rgn_id)) +
  #  ggtheme_plot +
  geom_line(size = 2, alpha = .6) +
  #  scale_colour_brewer(palette = 'PRGn') +
  labs(x = 'year',
       y = 'ICO status',
       title = 'ICO status over time',
       color = 'Region')

ggplotly(status_ts_plot)

trend_ts_plot <- ggplot(ico_sum %>%
                          filter(!is.na(rgn_id) &!is.na(ico_trend)),
                        aes(x = year, y = ico_trend, color = rgn_id, group = rgn_id)) +
  #  ggtheme_plot +
  geom_line(size = 2, alpha = .6) +
  #  scale_colour_brewer(palette = 'PRGn') +
  labs(x = 'year',
       y = 'ICO trend',
       title = 'ICO trend over time',
       color = 'Region')

ggplotly(trend_ts_plot)
