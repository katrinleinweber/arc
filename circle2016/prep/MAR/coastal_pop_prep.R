source('src/R/common.R')

goal     <- 'globalprep/coastal_population'
scenario <- 'v2015'
dir_git  <- file.path('~/github/ohiprep', goal)
dir_int  <- file.path(dir_git, scenario, 'int')
dir_data <- file.path(dir_git, scenario, 'data')
dir_anx  <- file.path(dir_neptune_data, 'git-annex', goal)
