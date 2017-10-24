## Codes
## WAL = Walrus, PWA = Pacific Walrus, HAR = Harp Seal, NAR= Narwhal, HOO = Hooded Sea, SPO=Spotted Seal.
###Prepping NP for entry in to layers.csv based on Antarctic Krill code

catch <- read.csv('circle2016/prep/NP/np_harvest_individuals_arc2016.csv')
limit <- read.csv('circle2016/prep/NP/np_harvest_reference_arc2016.csv') #Norway 108 reference for hooded seal based on average catches over 0 quota time (small take allowed for scientific purposes)
#Russia quota changed from 0 to 1 for 2010/2011 as otherwise it was not being counted. Catch also changed to 1 from 0 for 2011 (2010 catch=10)
# STEP 1. Calculate proportion of catch relative to reference
cmsy <- catch %>%
  left_join(limit, by=c("rgn_id", "species_code", "year")) %>%
  #filter(catch>0, reference>0) %>%
  mutate(c_cmsy = catch/reference) %>%
  dplyr::distinct() #PWA for Russia wasn't working for some reason. This gets rid of the duplicate that was being produced

# ------------------------------------------------------------------------
# STEP 2. Calculate score
eps <- .25
score_range  <- 1-0.25
value_range <- 0.90-0

scores <- cmsy %>%
  mutate(score = ifelse(c_cmsy > 1.1 , 2.1-c_cmsy,
                        ifelse(c_cmsy < 0.9, eps + score_range/value_range * c_cmsy, 1))) %>%
  mutate(score = ifelse(score <= 0, 0.1, score))

scores<- scores %>%
  dplyr::select(rgn_id, year, species_code, score)

write.csv(scores, 'np_harvest_arc2016_new.csv', row.names = FALSE)
## plot a figure to show how c/cmsy is converted to a score
png('temp/c_cmsyVSscore.png')
tmp <- data.frame(c_cmsy = seq(0,3, by=0.01)) %>%
  mutate(score = ifelse(c_cmsy > 1.1, 2.1-c_cmsy,
                        ifelse(c_cmsy < 0.9, eps + score_range/value_range * c_cmsy, 1)))%>%
  mutate(score = ifelse(score < 0, 0, score))
plot(score ~ c_cmsy, data=tmp, xlab='C/CL', ylab="Stock status score", type="l")
dev.off()
