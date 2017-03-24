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
              select(iucn_sid, sciname, subpop = population, cat = category),
            by = 'sciname') %>%
  filter(!is.na(iucn_sid))

write_csv(ico_list, file.path('prep/ICO/ico_list_prepped.csv'))

### for each species ID, get country list
ico_country_url <- 'http://apiv3.iucnredlist.org/api/v3/species/countries/id/%s?token=%s'

ico_spp_countries <- mc_get_from_api(ico_country_url, ico_list$iucn_sid)

rgn_iucn2ohi <- read_csv(file.path(dir_goal, 'raw/rgns_iucn2ohi.csv'))

ico_spp_rgn_raw <- ico_spp_countries %>%
  select(-code, -count, iucn_sid = name1, iucn_rgn_name = country) %>%
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

write_csv(ico_spp_rgn_raw, file.path(dir_goal, 'int/ico_spp_rgn_raw.csv'))
