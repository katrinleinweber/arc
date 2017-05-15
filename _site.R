## load any libraries needed across website pages
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(knitr)
})

## brewed vars
study_area      = "The Arctic"
gh_repo         = "arc"
gh_branch_data  = "master"
scenario_dir    = "circle2016"


## derived vars
dir_data        = sprintf('%s_%s', gh_repo, gh_branch_data)
dir_scenario    = sprintf('%s/%s', dir_data, scenario_dir)
gh_url          = sprintf('https://github.com/OHI-Science/%s.git', gh_repo)

## knitr options for all webpages
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)


run_cmd = function(cmd){
 # cat(sprintf('running command:\n  %s\n', cmd)) #comment out so not displayed on web pages
  system.time(system(cmd))
}

# data branch: fetch existing, or clone new
if (!file.exists(dir_data)){

  # clone data branch, shallowly and quietly
  run_cmd(sprintf('git clone -q --depth 1 --branch %s %s %s', gh_branch_data, gh_url, dir_data))
} else {

  # git fetch & overwrite
  run_cmd(sprintf('cd %s; git fetch -q; git reset -q --hard origin/%s; git checkout -q %s; git pull -q', dir_data, gh_branch_data, gh_branch_data))
}


# read config
config = new.env()
source(file.path(dir_scenario, 'conf/config.R'), config)


