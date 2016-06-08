## Overview
## calculate_scores.R calculates scores for all OHI dimensions (status, trend, pressures, resilience, likely future state, and overall Index scores).

## When you begin, this script will calculate all dimensions using the 'templated' data and goal models provided.
## As you develop goal models with your own data, we recommend that you work on one goal at a time with pre_scores.R and functions.R
## instead of calculating scores for all dimensions using CalculateAll(). Goal and subgoal models are individual R functions
## in functions.R. You can run them individually from functions.r as you modify them
## calculate "current status" and "trend".

## When you are done with all the goal model modifications, you can come back here, and run the following scripts, which combines "current status" and "trend"
## with pressures and resilience to finish your OHI scores calculations.

source('~/github/arc/circle2016/pre_scores.R')

## calculate scenario scores
scores = CalculateAll(conf, layers, debug=T)
write.csv(scores, 'scores.csv', na='', row.names=F)
