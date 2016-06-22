library(tidyr)
library(dplyr)
Troms_Employment = read_excel("Livelihoods/Employment_Figures/Norway/Troms_Employment_2009_2014.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Troms_Employment2 = data.frame(Troms_Employment)
Troms_Fish = Troms_Employment2 %>% filter(Job.Type == "03 Fishing and aquaculture")
Troms_Fish$Job.Type<-"fishing"
Troms_Food = Troms_Employment2 %>% filter(Job.Type == "10 Food products")
Troms_Food$Job.Type<-"food"
Troms_Water = Troms_Employment2 %>% filter(Job.Type == "50 Water transport")
Troms_Water$Job.Type<- "transport"
Troms_Travel = Troms_Employment2 %>% filter(Job.Type == "79 Travel agency, tour operators")
Troms_Travel$Job.Type<- "tourism"

## Ocean based employment considering food products likely to be seafood related, travel tourism likely be ocean related
# Join together


Troms_Group = rbind(Troms_Fish, Troms_Food, Troms_Water, Troms_Travel)
Troms_Group<-plyr::rename(Troms_Group, c("Job.Type"="sector"))
Troms_Group2 = gather(Troms_Group, "Year", "value", 3:8)

Troms_Jobs = Troms_Group2 %>% separate(Year,c("X","Year"),remove=T,sep="X")%>%  #(1) First strip away the X from the years (this creates a new column called "X" that is empty)
  select(Region,Year,sector, value)%>%
  group_by(Year)%>%
  ungroup()
troms_jobs_final=Troms_Jobs[c(1,3,2,4)]
troms_jobs_final

# Nordland Jobs -----------------------------------------------------------

Nordland_Employment = read_excel("Livelihoods/Employment_Figures/Norway/Nordland_Employment_2009_2014.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Nord_Employment2 = data.frame(Nordland_Employment)
Nord_Fish = Nord_Employment2 %>% filter(Job.Type == "03 Fishing and aquaculture")
Nord_Fish$Job.Type<-"fishing"
Nord_Food = Nord_Employment2 %>% filter(Job.Type == "10 Food products")
Nord_Food$Job.Type<-"food"
Nord_Water = Nord_Employment2 %>% filter(Job.Type == "50 Water transport")
Nord_Water$Job.Type<- "transport"
Nord_Travel = Nord_Employment2 %>% filter(Job.Type == "79 Travel agency, tour operators")
Nord_Travel$Job.Type<- "tourism"
## Ocean based employment considering food products likely to be seafood related, travel tourism likely be ocean related

# Join together
Nord_Group = rbind(Nord_Fish, Nord_Food, Nord_Water, Nord_Travel)
Nord_Group<-plyr::rename(Nord_Group, c("Job.Type"="sector"))
Nord_Group2 = gather(Nord_Group, "Year", "value", 3:8)

Nord_Jobs = Nord_Group2 %>% separate(Year,c("X","Year"),remove=T,sep="X")%>%  #(1) First strip away the X from the years (this creates a new column called "X" that is empty)
  select(Region,Year,sector, value)%>%
  group_by(Year)%>%
  ungroup()
nord_jobs_final=Nord_Jobs[c(1,3,2,4)]
nord_jobs_final$Region<-"Nordland"

# Finnmark Jobs -----------------------------------------------------------

Finnmark_Employment = read_excel("Livelihoods/Employment_Figures/Norway/Finnmark_Employment_2009_2014.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Finn_Employment2 = data.frame(Finnmark_Employment)
Finn_Fish = Finn_Employment2 %>% filter(Job.Type == "03 Fishing and aquaculture")
Finn_Fish$Job.Type<-"fishing"
Finn_Food = Finn_Employment2 %>% filter(Job.Type == "10 Food products")
Finn_Food$Job.Type<-"food"
Finn_Water = Finn_Employment2 %>% filter(Job.Type == "50 Water transport")
Finn_Water$Job.Type<- "transport"
Finn_Travel = Finn_Employment2 %>% filter(Job.Type == "79 Travel agency, tour operators")
Finn_Travel$Job.Type<- "tourism"

## Ocean based employment considering food products likely to be seafood related, travel tourism likely be ocean related

# Join together
Finn_Group = rbind(Finn_Fish, Finn_Food, Finn_Water, Finn_Travel)
Finn_Group<-plyr::rename(Finn_Group, c("Job.Type"="sector"))
Finn_Group2 = gather(Finn_Group, "Year", "value", 3:8)

Finn_Jobs = Finn_Group2 %>% separate(Year,c("X","Year"),remove=T,sep="X")%>%  #(1) First strip away the X from the years (this creates a new column called "X" that is empty)
  select(Region,Year,sector, value)%>%
  group_by(Year)%>%
  ungroup()
Finn_jobs_final=Finn_Jobs[c(1,3,2,4)]
Finn_jobs_final$Region<-"Finnmark"

# Norway Mainland Jobs ----------------------------------------------------

norway_main_jobs = rbind(troms_jobs_final, nord_jobs_final, Finn_jobs_final)%>%
  group_by(Year, sector)%>%
  dplyr::summarize(value = sum(value, na.rm=T))%>%
  ungroup()
norway_main_jobs$rgn_id <- "Norway Mainland"
norway_mainland_jobs=norway_main_jobs[c(4,2,1,3)]
norway_mainland_jobs

# Svalbard ----------------------------------------------------------------
Svalbard_Employment = read_excel("Livelihoods/Employment_Figures/Norway/Svalbard_Employment_2008_2014.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Sval_Employment2 = data.frame(Svalbard_Employment)%>%
  rename(c("Sector"="sector"))
Sval_Employment2<-filter(Sval_Employment2, !(sector %in% c('B Mining and quarrying', 'C-D-E Manufacturing', 'F Construction',
                                                             'G Wholesale and retail trade: repair of motor vehicles and motorcycles',
                                                             'J-K Information and communication', 'L Real estate activities',
                                                             'M Professional, scientific and technical activities', 'N Administrative and support service activities',
                                                             'O Public administration and defence', 'Q Human health and social work activities',
                                                             'S Other service activities', '00 Unspecified')))
## Ocean based employment considering main ocean related employment on Svalbard is tourism and science.

# Join together

Sval_Employment2<-gather(Sval_Employment2, "Year", "value", 2:8)


Sval_Jobs = Sval_Employment2 %>% separate(Year,c("X","Year"),remove=T,sep="X")%>%  #(1) First strip away the X from the years (this creates a new column called "X" that is empty)
  select(Year,sector,value)
  
Sval_Jobs$rgn_id <- "Svalbard"
sval_jobs_final=Sval_Jobs[c(4,2,1,3)]
sval_jobs_final$sector<-as.character(sval_jobs_final$sector) ## change to character
sval_jobs_final[sval_jobs_final=="P Education"]<- "education"
sval_jobs_final[sval_jobs_final=="H Transportation and storage"]<-"transport"
sval_jobs_final[sval_jobs_final=="I Accommodation and food service activities"]<-"hospitality"
sval_jobs_final[sval_jobs_final=="R Arts, entertainment and recreation"]<-"tourism"

# Norway Jobs bind --------------------------------------------------------


norway_jobs = rbind(sval_jobs_final, norway_mainland_jobs)
norway_jobs$value<- round(norway_jobs$value, digits=0)
norway_jobs


# Canada ------------------------------------------------------------------


# NWT ---------------------------------------------------------------------
NWT_Employment = read.csv('Livelihoods/Employment_Figures/Canada/NWT_Employment_2001_2015.csv')
NWT_Employment2 = data.frame(NWT_Employment)%>%
  rename(c("Job.Type"="sector"))
NWT_Employment2<-dplyr::filter(NWT_Employment2, (sector %in% c("Transportation and warehousing", "Accommodation and food services", "Information, culture and recreation")))
## Assume related to ship transport, tourism, ship business

NWT_Employment2<-gather(NWT_Employment2, "Year", "value", 2:16)

NWT_Employment2

NWT_Jobs = NWT_Employment2 %>% separate(Year,c("X","Year"),remove=T,sep="X")%>%  #(1) First strip away the X from the years (this creates a new column called "X" that is empty)
  select(Year,sector,value)
NWT_Jobs$Region <- "Beaufort"
NWT_jobs_final=NWT_Jobs[c(4,2,1,3)]
NWT_jobs_final$sector<-as.character(NWT_jobs_final$sector) 
NWT_jobs_final[NWT_jobs_final=="Transportation and warehousing"]<- "transport"
NWT_jobs_final[NWT_jobs_final=="Information, culture and recreation"]<-"tourism"
NWT_jobs_final[NWT_jobs_final=="Accommodation and food services"]<-"hospitality"

##Beaufort Delta area accounts for ~14.2% of NWT employment based on data from
##C:\Users\MB4514\Documents\github\pre-proc\Livelihoods\Employment_Figures\Raw files\Canada\Community Labour Force Activity, 1986 to 2014.xlsx
##Now calculate Beaufort Delta region based on 14.2% of NWT_jobs_final

NWT_jobs_final=dplyr::mutate(NWT_jobs_final, value = (NWT_jobs_final$value/100)*14.2)
NWT_jobs_final$value<- round(NWT_jobs_final$value, digits=0)

# Nunavut -----------------------------------------------------------------

Nunavut_Employment = read_excel("Livelihoods/Employment_Figures/Canada/Nunavut_Employment_2008_2014.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Nunavut_Employment<-data.frame(Nunavut_Employment)%>% rename(c("Job.Type"="sector"))

Nunavut_Employment2 = dplyr::filter(Nunavut_Employment, (sector %in% c("Fishing, Hunting, Trapping, Mining and Quarrying", "Transportation and Warehousing", "Accommodation and Food Services")))
## Job types quite general - taken ones with most likely marine connections.

Nunavut_Employment2<-gather(Nunavut_Employment2, "Year", "value", 2:8)

Nunavut_Jobs = Nunavut_Employment2 %>% separate(Year,c("X","Year"),remove=T,sep="X")%>%
  select(Year, sector, value)
Nunavut_Jobs$Region <- "Nunavut"
Nunavut_jobs_final=Nunavut_Jobs[c(4,2,1,3)]
Nunavut_jobs_final$sector<-as.character(Nunavut_jobs_final$sector) 
Nunavut_jobs_final[Nunavut_jobs_final=="Transportation and Warehousing"]<- "transport"
Nunavut_jobs_final[Nunavut_jobs_final=="Fishing, Hunting, Trapping, Mining and Quarrying"]<-"fishing"
Nunavut_jobs_final[Nunavut_jobs_final=="Accommodation and Food Services"]<-"hospitality"


# Canada Join -------------------------------------------------------------

canada_jobs = rbind(NWT_jobs_final, Nunavut_jobs_final)
canada_jobs<- rename(canada_jobs, c("Region"="rgn_id"))


# Greenland ---------------------------------------------------------------

## Only figures for whole country
Greenland_Employment = read_excel("Livelihoods/Employment_Figures/Greenland/Greenland_Employment_2008_2014.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Greenland_Employment<-data.frame(Greenland_Employment)%>% rename(c("Job.Type"="sector"))
Greenland_Employment2 = dplyr::filter(Greenland_Employment, (sector %in% c("Fishing, hunting & agriculture", "Transportation", "Hotels and restaurants")))
##Jobs quite general - kept with theme of fishing/ship transport and tourism
Greenland_Employment2=rename(Greenland_Employment2, c("Jobs"="value"))

Greenland_Employment2$rgn_id <- "Greenland"
Greenland_jobs_final=Greenland_Employment2[c(4,2,1,3)]
Greenland_jobs_final[Greenland_jobs_final=="Transportation"]<- "transport"
Greenland_jobs_final[Greenland_jobs_final=="Fishing, hunting & agriculture"]<-"fishing"
Greenland_jobs_final[Greenland_jobs_final=="Hotels and restaurants"]<-"hospitality"

##Check how best to separate out between east and west Greenland? Roughly 80% of people live in West Greenland



# USA/Alaska --------------------------------------------------------------


# 2014 --------------------------------------------------------------------

Alaska_Employment14 = read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 3, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_Employment14<-data.frame(Alaska_Employment14)
Alaska_Employment14 = dplyr::filter(Alaska_Employment, (AREA.CODE %in% c("000185", "000188")))
Alaska_Employment14 <- dplyr::tbl_df(Alaska_Employment14)
Alaska_Employment14 <- dplyr::select(Alaska_Employment14, AREANAME, NAICS.DESCRIPTION, NA., NA..1, NA..2, AVERAGE.EMPLOYMENT)
Alaska_Employment14 <- dplyr::filter(Alaska_Employment14, AVERAGE.EMPLOYMENT > 0)
Alaska_Employment14 <- dplyr::filter(Alaska_Employment14, (NA. %in% c("NATURAL RESOURCES AND MINING", "MANUFACTURING", "TRADE, TRANSPORTATION AND UTILITIES", "LEISURE AND HOSPITALITY" )))
Alaska_Employment14$Year <- "2014"
Alaska_Employment14

##Not really any marine jobs listed in these areas. National figures don't seem to be matched by the regional figures though..?

# 2013 --------------------------------------------------------------------
Alaska_Employment13 = read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 4, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_Employment13<-data.frame(Alaska_Employment13)
Alaska_Employment13 = dplyr::filter(Alaska_Employment13, (AREA.CODE %in% c("000185", "000188")))
Alaska_Employment13 <- dplyr::tbl_df(Alaska_Employment13)
Alaska_Employment13 <- dplyr::select(Alaska_Employment13, AREANAME, NAICS.DESCRIPTION, NA., NA..1, NA..2, AVERAGE.EMPLOYMENT)
Alaska_Employment13 <- dplyr::filter(Alaska_Employment13, AVERAGE.EMPLOYMENT > 0)
Alaska_Employment13 <- dplyr::filter(Alaska_Employment13, (NA. %in% c("NATURAL RESOURCES AND MINING", "MANUFACTURING", "TRADE, TRANSPORTATION AND UTILITIES", "LEISURE AND HOSPITALITY" )))
Alaska_Employment13$Year <- "2013"
Alaska_Employment13


# 2012 --------------------------------------------------------------------
Alaska_Employment12 = read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 5, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_Employment12<-data.frame(Alaska_Employment12)
Alaska_Employment12 = dplyr::filter(Alaska_Employment12, (AREA.CODE %in% c("000185", "000188")))
Alaska_Employment12 <- dplyr::tbl_df(Alaska_Employment12)
Alaska_Employment12 <- dplyr::select(Alaska_Employment12, AREANAME, NAICS.DESCRIPTION, NA., NA..1, NA..2, AVERAGE.EMPLOYMENT)
Alaska_Employment12 <- dplyr::filter(Alaska_Employment12, AVERAGE.EMPLOYMENT > 0)
Alaska_Employment12 <- dplyr::filter(Alaska_Employment12, (NA. %in% c("NATURAL RESOURCES AND MINING", "MANUFACTURING", "TRADE, TRANSPORTATION AND UTILITIES", "LEISURE AND HOSPITALITY" )))
Alaska_Employment12$Year <- "2012"
Alaska_Employment12

# 2011 --------------------------------------------------------------------
Alaska_Employment11 = read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 6, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_Employment11<-data.frame(Alaska_Employment11)
Alaska_Employment11 = dplyr::filter(Alaska_Employment11, (AREA.CODE %in% c("000185", "000188")))
Alaska_Employment11 <- dplyr::tbl_df(Alaska_Employment11)
Alaska_Employment11 <- dplyr::select(Alaska_Employment11, AREANAME, NAICS.DESCRIPTION, NA., NA..1, NA..2, AVERAGE.EMPLOYMENT)
Alaska_Employment11 <- dplyr::filter(Alaska_Employment11, AVERAGE.EMPLOYMENT > 0)
Alaska_Employment11 <- dplyr::filter(Alaska_Employment11, (NA. %in% c("NATURAL RESOURCES & MINING", "MANUFACTURING", "TRADE, TRANS. & UTILITIES", "LEISURE & HOSPITALITY" )))
##Note slight difference in Job types i.e. use of & rather than and. To correct later
Alaska_Employment11$Year <- "2011"
Alaska_Employment11

# 2010 --------------------------------------------------------------------
Alaska_Employment10 = read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 7, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_Employment10<-data.frame(Alaska_Employment10)
Alaska_Employment10 = dplyr::filter(Alaska_Employment10, (AREA.CODE %in% c("000185", "000188")))
Alaska_Employment10 <- dplyr::tbl_df(Alaska_Employment10)
Alaska_Employment10 <- dplyr::select(Alaska_Employment10, AREANAME, NAICS.DESCRIPTION, NA., NA..1, NA..2, AVERAGE.EMPLOYMENT)
Alaska_Employment10 <- dplyr::filter(Alaska_Employment10, AVERAGE.EMPLOYMENT > 0)
Alaska_Employment10 <- dplyr::filter(Alaska_Employment10, (NA. %in% c("NATURAL RESOURCES & MINING", "MANUFACTURING", "TRADE, TRANS. & UTILITIES", "LEISURE & HOSPITALITY" )))
Alaska_Employment10$Year <- "2010"
Alaska_Employment10

# 2009 --------------------------------------------------------------------
Alaska_Employment09 = read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 8, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_Employment09<-data.frame(Alaska_Employment09)
Alaska_Employment09 = dplyr::filter(Alaska_Employment09, (AREA.CODE %in% c("000185", "000188")))
Alaska_Employment09 <- dplyr::tbl_df(Alaska_Employment09)
Alaska_Employment09 <- dplyr::select(Alaska_Employment09, AREANAME, NAICS.DESCRIPTION, NA., NA..1, NA..2, AVERAGE.EMPLOYMENT)
Alaska_Employment09 <- dplyr::filter(Alaska_Employment09, AVERAGE.EMPLOYMENT > 0)
Alaska_Employment09 <- dplyr::filter(Alaska_Employment09, (NA. %in% c("NATURAL RESOURCES & MINING", "MANUFACTURING", "TRADE, TRANS. & UTILITIES", "LEISURE & HOSPITALITY" )))
Alaska_Employment09$Year <- "2009"
Alaska_Employment09

# 2008 --------------------------------------------------------------------
Alaska_Employment08 = read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 9, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_Employment08<-data.frame(Alaska_Employment08)
Alaska_Employment08 = dplyr::filter(Alaska_Employment08, (AREA.CODE %in% c("000185", "000188")))
Alaska_Employment08 <- dplyr::tbl_df(Alaska_Employment08)
Alaska_Employment08 <- dplyr::select(Alaska_Employment08, AREANAME, NAICS.DESCRIPTION, NA., NA..1, NA..2, AVERAGE.EMPLOYMENT)
Alaska_Employment08 <- dplyr::filter(Alaska_Employment08, AVERAGE.EMPLOYMENT > 0)
Alaska_Employment08 <- dplyr::filter(Alaska_Employment08, (NA. %in% c("NATURAL RESOURCES & MINING", "MANUFACTURING", "TRADE, TRANS. & UTILITIES", "LEISURE & HOSPITALITY" )))
Alaska_Employment08$Year <- "2008"
Alaska_Employment08

# 2007 --------------------------------------------------------------------
Alaska_Employment07 = read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 10, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_Employment07<-data.frame(Alaska_Employment07)
Alaska_Employment07 = dplyr::filter(Alaska_Employment07, (AREA.CODE %in% c("000185", "000188")))
Alaska_Employment07 <- dplyr::tbl_df(Alaska_Employment07)
Alaska_Employment07 <- dplyr::select(Alaska_Employment07, AREANAME, NAICS.DESCRIPTION, NA., NA..1, NA..2, AVERAGE.EMPLOYMENT)
Alaska_Employment07 <- dplyr::filter(Alaska_Employment07, AVERAGE.EMPLOYMENT > 0)
Alaska_Employment07 <- dplyr::filter(Alaska_Employment07, (NA. %in% c("NATURAL RESOURCES & MINING", "MANUFACTURING", "TRADE, TRANS. & UTILITIES", "LEISURE & HOSPITALITY" )))
Alaska_Employment07$Year <- "2007"
Alaska_Employment07

# 2006 --------------------------------------------------------------------
Alaska_Employment06 = read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 11, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_Employment06<-data.frame(Alaska_Employment06)
Alaska_Employment06 = dplyr::filter(Alaska_Employment06, (AREA.CODE %in% c("000185", "000188")))
Alaska_Employment06 <- dplyr::tbl_df(Alaska_Employment06)
Alaska_Employment06 <- dplyr::select(Alaska_Employment06, AREANAME, NAICS.DESCRIPTION, NA., NA..1, NA..2, AVERAGE.EMPLOYMENT)
Alaska_Employment06 <- dplyr::filter(Alaska_Employment06, AVERAGE.EMPLOYMENT > 0)
Alaska_Employment06 <- dplyr::filter(Alaska_Employment06, (NA. %in% c("NATURAL RESOURCES & MINING", "MANUFACTURING", "TRADE, TRANS. & UTILITIES", "LEISURE & HOSPITALITY" )))
Alaska_Employment06$Year <- "2006"
Alaska_Employment06

# 2005 --------------------------------------------------------------------
Alaska_Employment05 = read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 12, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_Employment05<-data.frame(Alaska_Employment05)
Alaska_Employment05 = dplyr::filter(Alaska_Employment05, (AREA.CODE %in% c("000185", "000188")))
Alaska_Employment05 <- dplyr::tbl_df(Alaska_Employment05)
Alaska_Employment05 <- dplyr::select(Alaska_Employment05, AREANAME, NAICS.DESCRIPTION, NA., NA..1, NA..2, AVERAGE.EMPLOYMENT)
Alaska_Employment05 <- dplyr::filter(Alaska_Employment05, AVERAGE.EMPLOYMENT > 0)
Alaska_Employment05 <- dplyr::filter(Alaska_Employment05, (NA. %in% c("NATURAL RESOURCE & MINING", "MANUFACTURING", "TRADE, TRANS. & UTILITIES", "LEISURE & HOSPITALITY" )))
Alaska_Employment05$Year <- "2005"
Alaska_Employment05
##Slight name change in Resource rather than Resources

# 2004 --------------------------------------------------------------------
Alaska_Employment04 = read_excel("Livelihoods/Employment_Figures/USA/Alaska_Employment_2002_2014.xlsx", sheet = 13, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Alaska_Employment04<-data.frame(Alaska_Employment04)
Alaska_Employment04 = dplyr::filter(Alaska_Employment04, (AREA.CODE %in% c("000185", "000188")))
Alaska_Employment04 <- dplyr::tbl_df(Alaska_Employment04)
Alaska_Employment04 <- dplyr::select(Alaska_Employment04, AREANAME, NAICS.DESCRIPTION, NA., NA..1, NA..2, AVERAGE.EMPLOYMENT)
Alaska_Employment04 <- dplyr::filter(Alaska_Employment04, AVERAGE.EMPLOYMENT > 0)
Alaska_Employment04 <- dplyr::filter(Alaska_Employment04, (NA. %in% c("NATURAL RESOURCE & MINING", "MANUFACTURING", "TRADE, TRANS. & UTILITIES", "LEISURE & HOSPITALITY" )))
Alaska_Employment04$Year <- "2004"
Alaska_Employment04

# Alaska join and sort ----------------------------------------------------
Alaska_Group = rbind(Alaska_Employment04, Alaska_Employment05, Alaska_Employment06, Alaska_Employment07, Alaska_Employment08, Alaska_Employment09, Alaska_Employment10, Alaska_Employment11, Alaska_Employment12, Alaska_Employment13, Alaska_Employment14)
Alaska_Group<- select(Alaska_Group, Year, AREANAME, NA., AVERAGE.EMPLOYMENT)
Alaska_Group<-filter(Alaska_Group, !(NA. %in% c('NATURAL RESOURCE & MINING', 'NATURAL RESOURCES & MINING', 'NATURAL RESOURCES AND MINING')))
##Dropped natural resource and mining as detailed check of data shows that it is all related to oil/gas/mining - not fishing or trapping
Alaska_Group<-select(Alaska_Group, Year, AVERAGE.EMPLOYMENT, NA.)
Alaska_Group<-plyr::rename(Alaska_Group, c("AVERAGE.EMPLOYMENT"="value", "NA."="sector"))
Alaska_Jobs=Alaska_Group
Alaska_Jobs$rgn_id<- "Alaska"
Alaska_Jobs<-Alaska_Jobs[c(4,3,1,2)]
Alaska_Jobs
Alaska_Jobs[Alaska_Jobs=="TRADE, TRANS. & UTILITIES"]<- "transport"
Alaska_Jobs[Alaska_Jobs=="MANUFACTURING"]<-"manufacturing"
Alaska_Jobs[Alaska_Jobs=="LEISURE & HOSPITALITY"]<-"hospitality"

# Russia ------------------------------------------------------------------


# 2014 --------------------------------------------------------------------

Russia_Employment14 = read_excel("Livelihoods/Employment_Figures/Russia/Russia_Employment_2010_2014.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Russia_Employment14<-data.frame(Russia_Employment14)
Russia_Employment14 <- dplyr::tbl_df(Russia_Employment14)
Russia_Employment14 <- dplyr::filter(Russia_Employment14, !(District %in% c("North-western Federal District", "Including Nenets Autonomous Okrug", "Without Nenets", "Ural Federal District", "Siberian Federal District", "Far Eastern Federal District", "Republic of Kaerlia")))
Russia_Employment14<- mutate(Russia_Employment14, "Transport" = Transport.and...communication-of.them...communication)
Russia_Employment14<- dplyr::select(Russia_Employment14, District, Agriculture..hunting.and.forestry..fishing..fish.farming, Hotels.and...restaurants, Transport)
Russia_Employment14$Year <- "2014"
Russia_Employment14$Transport<-Russia_Employment14$Transport*1000
Russia_Employment14$Agriculture..hunting.and.forestry..fishing..fish.farming<-Russia_Employment14$Agriculture..hunting.and.forestry..fishing..fish.farming*1000
Russia_Employment14$Hotels.and...restaurants<-Russia_Employment14$Hotels.and...restaurants*1000



# #2013 -------------------------------------------------------------------

Russia_Employment13 = read_excel("Livelihoods/Employment_Figures/Russia/Russia_Employment_2010_2014.xlsx", sheet = 2, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Russia_Employment13<-data.frame(Russia_Employment13)
Russia_Employment13 <- dplyr::tbl_df(Russia_Employment13)
Russia_Employment13 <- dplyr::filter(Russia_Employment13, !(District %in% c("North-western Federal District", "Including Nenets Autonomous Okrug", "Without Nenets", "Ural Federal District", "Siberian Federal District", "Far Eastern Federal District", "Republic of Kaerlia")))
Russia_Employment13<- mutate(Russia_Employment13, "Transport" = Transport.and...communication-of.them...communication)
Russia_Employment13<- dplyr::select(Russia_Employment13, District, Agriculture..hunting.and.forestry..fishing..fish.farming, Hotels.and...restaurants, Transport)
Russia_Employment13$Year <- "2013"
Russia_Employment13$Transport<-Russia_Employment13$Transport*1000
Russia_Employment13$Agriculture..hunting.and.forestry..fishing..fish.farming<-Russia_Employment13$Agriculture..hunting.and.forestry..fishing..fish.farming*1000
Russia_Employment13$Hotels.and...restaurants<-Russia_Employment13$Hotels.and...restaurants*1000



# #2012 -------------------------------------------------------------------
Russia_Employment12 = read_excel("Livelihoods/Employment_Figures/Russia/Russia_Employment_2010_2014.xlsx", sheet = 3, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Russia_Employment12<-data.frame(Russia_Employment12)
Russia_Employment12 <- dplyr::tbl_df(Russia_Employment12)
Russia_Employment12 <- dplyr::filter(Russia_Employment12, !(District %in% c("North-western Federal District", "Including Nenets Autonomous Okrug", "Without Nenets", "Ural Federal District", "Siberian Federal District", "Far Eastern Federal District", "Republic of Kaerlia")))
Russia_Employment12<- mutate(Russia_Employment12, "Transport" = Transport.and...communication-of.them...communication)
Russia_Employment12<- dplyr::select(Russia_Employment12, District, Agriculture..hunting.and.forestry..fishing..fish.farming, Hotels.and...restaurants, Transport)
Russia_Employment12$Year <- "2012"
Russia_Employment12$Transport<-Russia_Employment12$Transport*1000
Russia_Employment12$Agriculture..hunting.and.forestry..fishing..fish.farming<-Russia_Employment12$Agriculture..hunting.and.forestry..fishing..fish.farming*1000
Russia_Employment12$Hotels.and...restaurants<-Russia_Employment12$Hotels.and...restaurants*1000



# #2011 -------------------------------------------------------------------
Russia_Employment11 = read_excel("Livelihoods/Employment_Figures/Russia/Russia_Employment_2010_2014.xlsx", sheet = 4, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Russia_Employment11<-data.frame(Russia_Employment11)
Russia_Employment11 <- dplyr::tbl_df(Russia_Employment11)
Russia_Employment11 <- dplyr::filter(Russia_Employment11, !(District %in% c("North-western Federal District", "Including Nenets Autonomous Okrug", "Without Nenets", "Ural Federal District", "Siberian Federal District", "Far Eastern Federal District", "Republic of Kaerlia")))
Russia_Employment11<- mutate(Russia_Employment11, "Transport" = Transport.and...communication-of.them...communication)
Russia_Employment11<- dplyr::select(Russia_Employment11, District, Agriculture..hunting.and.forestry..fishing..fish.farming, Hotels.and...restaurants, Transport)
Russia_Employment11$Year <- "2011"
Russia_Employment11$Transport<-Russia_Employment11$Transport*1000
Russia_Employment11$Agriculture..hunting.and.forestry..fishing..fish.farming<-Russia_Employment11$Agriculture..hunting.and.forestry..fishing..fish.farming*1000
Russia_Employment11$Hotels.and...restaurants<-Russia_Employment11$Hotels.and...restaurants*1000


# 2010 --------------------------------------------------------------------
Russia_Employment10 = read_excel("Livelihoods/Employment_Figures/Russia/Russia_Employment_2010_2014.xlsx", sheet = 5, col_names = TRUE, col_types = NULL, na = "", skip = 0)
Russia_Employment10<-data.frame(Russia_Employment10)
Russia_Employment10 <- dplyr::tbl_df(Russia_Employment10)
Russia_Employment10 <- dplyr::filter(Russia_Employment10, !(District %in% c("North-western Federal District", "Including Nenets Autonomous Okrug", "Without Nenets", "Ural Federal District", "Siberian Federal District", "Far Eastern Federal District", "Republic of Kaerlia")))
Russia_Employment10<- mutate(Russia_Employment10, "Transport" = Transport.and...communication-of.them...communication)
Russia_Employment10<- dplyr::select(Russia_Employment10, District, Agriculture..hunting.and.forestry..fishing..fish.farming, Hotels.and...restaurants, Transport)
Russia_Employment10$Year <- "2010"
Russia_Employment10$Transport<-Russia_Employment10$Transport*1000
Russia_Employment10$Agriculture..hunting.and.forestry..fishing..fish.farming<-Russia_Employment10$Agriculture..hunting.and.forestry..fishing..fish.farming*1000
Russia_Employment10$Hotels.and...restaurants<-Russia_Employment10$Hotels.and...restaurants*1000



# # bind ------------------------------------------------------------------

Russia_Group = rbind(Russia_Employment10, Russia_Employment11, Russia_Employment12, Russia_Employment13, Russia_Employment14)
Russia_Group<-plyr::rename(Russia_Group, c("Agriculture..hunting.and.forestry..fishing..fish.farming"="fishing", "Hotels.and...restaurants"="hospitality", "Transport"="transport"))
Russia_Group<- gather(Russia_Group, sector, value, 2:4)
Russia_Group$value<-as.numeric(Russia_Group$value)
Russia_Group$value[Russia_Group$District =="Chukotka Autonomous Okrug"] <-Russia_Group$value[Russia_Group$District =="Chukotka Autonomous Okrug"]/2.2758
Russia_Group$value[Russia_Group$District =="Krasnoyarsk region"] <-Russia_Group$value[Russia_Group$District =="Krasnoyarsk region"]/83.33333
Russia_Group$value[Russia_Group$District =="The Republic of Sakha (Yakutia)"] <-Russia_Group$value[Russia_Group$District =="The Republic of Sakha (Yakutia)"]/33.7838
Russia_Group$value<- round(Russia_Group$value, digits=0)
##Work out % employment in each region so can amend jobs - Krasnoyarsk region is huge.

##Taymyrsky Dolgano-Nenetsky District in Krasnoyarsk krai = population 34,432. KK population = 2,828,187. 1.2% of the data below
## Chukotsky, Iulintsky, Chaunsky and Bilibinksy districts in Chukotka = poppulation = 22,201. Chukotka pop = 50,526. 43.94%
##Sakha coastal districts = population 28,325. sakha pop = 958, 528. 2.96%

Russia_Jobs=Russia_Group%>%group_by(Year, sector)%>%
  dplyr::summarize(value = sum(value, na.rm=T))%>%
  ungroup()
Russia_Jobs$rgn_id <- "Russia"
Russia_Jobs<-Russia_Jobs[c(4,2,1,3)]
Russia_Jobs

le_jobs_sector_year= rbind(Russia_Jobs, Alaska_Jobs, canada_jobs, norway_jobs, Greenland_jobs_final)
le_jobs_sector_year
write.csv(le_jobs_sector_year, "le_jobs_sector_year.csv")
