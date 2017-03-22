###############################################
## Combining the most current data for each habitat (2016 analysis)

#############################################

source("src/R/common.R")
pathSoftbottom <- "prep/pressures/habitat/output"
pathSeaice <- "prep/CP/output"

## Extent data ----
softbottom_extent <- read.csv(file.path(pathSoftbottom, "habitat_extent_softbottom.csv"))
seaice_extent <- read.csv(file.path(pathSeaice, "hab_ice_extent_eez.csv"))


head(softbottom_extent)
head(seaice_extent)


habitat_extent_v2015 <- rbind(softbottom_extent, seaice_extent)
table(habitat_extent_v2015$habitat)
write.csv(habitat_extent_v2015, "prep/HAB/output/habitat_extent_v2015.csv", row.names=FALSE)
## habitat extent uses the same data for all years of analysis




# health function
habitat_summary <- function(ScenarioYear, dataYear_si, dataYear_sb){
  seaice_health <- read.csv(file.path(pathSeaice, sprintf("hab_ice_health_eez_%s.csv", dataYear_si)))
  softbottom_health <- read.csv(file.path(pathSoftbottom, sprintf("habitat_health_softbottom_v%s.csv", dataYear_sb)))
  habitat_health <- rbind(seaice_health, softbottom_health)
  write.csv(habitat_health, sprintf("prep/HAB/output/habitat_health_%s.csv", ScenarioYear), row.names=FALSE)
}

habitat_summary(ScenarioYear=2016, dataYear_si=2015, dataYear_sb=2010)
habitat_summary(ScenarioYear=2015, dataYear_si=2014, dataYear_sb=2009)
habitat_summary(ScenarioYear=2014, dataYear_si=2013, dataYear_sb=2008)
habitat_summary(ScenarioYear=2013, dataYear_si=2012, dataYear_sb=2007)
habitat_summary(ScenarioYear=2012, dataYear_si=2011, dataYear_sb=2006)


# trend 2015
trend_summary <- function(ScenarioYear, dataYear_si, dataYear_man, dataYear_sb){
  seaice_trend <- read.csv(file.path(pathSeaice, sprintf("hab_ice_trend_eez_%s.csv", dataYear_si)))
  softbottom_trend <- read.csv(file.path(pathSoftbottom, sprintf('habitat_trend_softbottom_v%s.csv', dataYear_sb)))
  habitat_trend <- rbind(seaice_trend, softbottom_trend)
  summary(habitat_trend)
  write.csv(habitat_trend, sprintf("prep/HAB/output/habitat_trend_%s.csv", ScenarioYear), row.names=FALSE)
}

trend_summary(ScenarioYear=2016, dataYear_si=2015, dataYear_man=2015, dataYear_sb=2010)
trend_summary(ScenarioYear=2015, dataYear_si=2014, dataYear_man=2015, dataYear_sb=2009)
trend_summary(ScenarioYear=2014, dataYear_si=2013, dataYear_man=2014, dataYear_sb=2008)
trend_summary(ScenarioYear=2013, dataYear_si=2012, dataYear_man=2013, dataYear_sb=2007)
trend_summary(ScenarioYear=2012, dataYear_si=2011, dataYear_man=2012, dataYear_sb=2006)
