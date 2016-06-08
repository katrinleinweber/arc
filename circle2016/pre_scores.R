# pre_calculate_scores

## This script does the pre-checks before running goal models and calculate
## dimension scores. It loads "ohicore", calls all goal functions and data
## layers in the "conf" folder, and check that all data layers are registered
## properly.

## It is sourced from calculate_scores.r as well, but you are encouraged to use this script when
## you're working on individual goal models. After you register data layers for
## a goal, or make any changes to the data layers, source this script before
## running model-specific functions in functions.R.

if (!"ohicore" %in% (.packages())) {

  # load required libraries for functions.r
  suppressWarnings(require(ohicore))

  library(tidyr)
  library(dplyr)
  library(reshape2)
  library(psych)

}

# set working directory to the scenario directory, ie containing conf and layers directories
setwd('~/github/bhi/baltic2015')

# load scenario configuration. "Conf" is an ohicore function that loads all materials from "conf" folder
conf = Conf('conf')

# run checks on data layers. this ohicore function checks that the data files in "layers"
# folder match their registration information in "layers.csv". But it doesn't
# manipulate the data in anyway.

CheckLayers('layers.csv', 'layers', flds_id=conf$config$layers_id_fields)

# load scenario layers. this ohicore function compiles all data layers into one large data file for
# easy extraction of individual layers for calculation. It doesn't manipulate
# the data themselves.

layers = Layers('layers.csv', 'layers')

