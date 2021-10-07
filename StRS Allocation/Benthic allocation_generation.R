### Allocation calculations for reef fish surveys

# June 24, 2020
# Questions: Kaylyn.McCoy@noaa.gov

#__________________________________
# This pulls in the last 3 rounds (or so) of data and calculates variance at the trophic group level (but this pooling level can be changed). Proportional Variance for each trophic group is calculated at the strata level and averaged for each strata. Proportional area is calculated from the sectors file at the island level, and is multiplied by the average variance at each strata. This becomes the proportional weight and should be used in conjunction with the proprotional area to allocate survey effort.

# !!!!! FILES NEEDED: First run data pooling scripts 01 and 02 from GitHub to get site level averages: fish-paste/munge_REA/01_gitFishREACleanData and 02_gitFishREACalcWD. Summary data files you need are 1) TMPwsd.Rdata, and 2) TMPsectors.Rdata. You will also need the cruise itinerary and the number of sites that can be surveyed in one day.

# If we aren't visiting all sectors during the field mission, remove that sector from this calculation - i.e. if we aren't going to all marine protected areas in Guam or we aren't sampling backreef in the NWHI, remove those areas in the beginning.
#__________________________________

# set up  -----------------------------------------------------------------------------------------
rm(list=ls())      # clean working environment

# Load libraries
library(tidyverse) # for summarizing/orgainzing data
library(dplyr)     # for summarizing/orgainzing data- LOAD LAST TO PREVENT MASKING

# Load the clean trophic level site data and sectors data
# set wd to relevant folder
setwd("T:/Benthic/Data/StRS Allocation")
# load sector data
sectors<-read.csv("BenthicSectorsforAllocation.csv")
# view data
head(sectors)
# load demographic site data
wsd<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_GENUS.csv")
# view data
head(wsd)

# NWHI -------------------------------------------------------------------------

# filter past data for region and nSPC surveys and the last few years/rounds
nwhi<-wsd %>% filter(REGION == "NWHI", METHOD == "nSPC", OBS_YEAR>"2010") %>% 
  group_by(ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN) %>% # group by strata
  summarize(SD_PISC = sd(PISCIVORE),SD_PLANK = sd(PLANKTIVORE),SD_PRI = sd(PRIMARY),SD_SEC = sd(SECONDARY),SD_TOT = sd(TotFish)) %>% # calculate variance (sd) for each trophic level
  drop_na() # drop strata with no data

# # !!!!!! filter for reef zones/islands NOT being surveyed here !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# # for example, take out Lagoon and backreef
# nwhi<-nwhi %>% filter(REEF_ZONE != "Lagoon",REEF_ZONE != "Backreef")
# nwhi<-droplevels(nwhi)

# get total vairance per island in order to get proportion of each torhpic group's variance
a<-nwhi %>% group_by(ISLAND) %>%
  summarise(PISC=sum(SD_PISC),PLANK=sum(SD_PLANK),PRI=sum(SD_PRI),SEC=sum(SD_SEC),TOT=sum(SD_TOT)) 

# join totals back to original dataframe to calculate proportion
b<-full_join(nwhi,a,by="ISLAND") %>%
  mutate(PROP_PISC = (SD_PISC/PISC),PROP_PLANK = (SD_PLANK/PLANK),PROP_PRI = (SD_PRI/PRI),PROP_SEC = (SD_SEC/SEC),PROP_TOT = (SD_TOT/TOT))

# calculate average variance for each strata - first isloate proportion of variance, get rid of sd columns
names(b)
c<-b[,c("ISLAND","SEC_NAME","REEF_ZONE" , "DEPTH_BIN","PROP_PISC" , "PROP_PLANK", "PROP_PRI","PROP_SEC","PROP_TOT" )]
d<-gather(c, troph, prop, -c(ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN)) %>% # melt data with 'gather' function to get all data into columns rather than rows
  group_by(ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN) %>%
  summarise(mean_v=mean(prop)) # get the mean at the strata level

# get area values for each sector from the 'sectors' dataframe
head(sectors)
sec<-sectors %>% select(ISLAND, SEC_NAME, REEF_ZONE, DEPTH_BIN, AREA_HA)%>%
  right_join(d, by=c("ISLAND", "SEC_NAME", "REEF_ZONE", "DEPTH_BIN")) # right joining to the previous dataframe selects values for just the NWHI

e<-sec %>% group_by(ISLAND) %>%
  summarize(area=sum(AREA_HA)) # summarize area by island

f<-full_join(sec,e,by="ISLAND") %>% 
  mutate(AREA_PCT=AREA_HA/area) %>% 
  select(ISLAND, SEC_NAME, REEF_ZONE, DEPTH_BIN,mean_v,AREA_PCT) # calculate proportional area for each strata

g<-gather(f, var, value, -c(ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN)) %>% # melt data with 'gather' function to get all data into columns rather than rows
  group_by(ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN) %>% 
  summarise(mean_v=prod(value)) # multiply the mean variance times the proportion of area of each strata

h<-g %>% group_by(ISLAND) %>% 
  summarize(sum=sum(mean_v))
i<-full_join(g,h,by="ISLAND") %>% 
  mutate(AREA_VAR_PCT=mean_v/sum) %>% # calculate the proportional weight for each strata
  select(ISLAND, SEC_NAME, REEF_ZONE, DEPTH_BIN,AREA_VAR_PCT) # clean up dataframe

# JUST A CHECK: does total variance for each island add up to 1?
test<-i %>% 
  group_by(ISLAND) %>% 
  summarize(sum=sum(AREA_VAR_PCT))
summary(test)

# add proportion of each strata by area along with the proportional weight 
j<-i %>% full_join(f,by=c("ISLAND", "SEC_NAME", "REEF_ZONE", "DEPTH_BIN"))
k<-select(j,-mean_v)

# to calculate allocation based on weights:
# get list of islands
unique(k$ISLAND)
# Value (n) is number of sites we can survey in 1 day (this comes from team lead, based on small boats and number of divers)
n<-6
# Plug in number of days we have at each island here (using each island's 3-letter code), and multiply based on number of sites we can survey in 1 day (n)

ffs<-3*n
kur<-3*n
lay<-3*n
lis<-3*n
mid<-3*n
phr<-3*n

#create a field for sites and give a dummy variable = 1
k$TOTSITES<-1
# for each island, plug in total sites we can survey during this visit

k[k$ISLAND %in% "French Frigate",]$TOTSITES<-ffs
k[k$ISLAND %in% "Kure",]$TOTSITES<-kur
k[k$ISLAND %in% "Laysan",]$TOTSITES<-lay
k[k$ISLAND %in% "Lisianski",]$TOTSITES<-lis
k[k$ISLAND %in% "Midway",]$TOTSITES<-mid
k[k$ISLAND %in% "Pearl & Hermes",]$TOTSITES<-phr

# to get the allocation, multiply the number of total sites we can survey at each island by the 2 different weighted values (area and variance, and just area)
k$WEIGHTED_ALLOCATION<-round(k$TOTSITES*k$AREA_VAR_PCT)
k$AREA_ALLOCATION<-round(k$TOTSITES*k$AREA_PCT)

# save file
write.csv(k,file="NWHI_allocation.csv")


# Southern Marianas -------------------------------------------------------------------------

# filter past data for region and nSPC surveys and the last few years/rounds
smari<-wsd %>% filter(REGION == "S.MARIAN", METHOD == "nSPC",OBS_YEAR>"2010") %>% 
  group_by(ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN) %>% # group by strata
  summarize(SD_PISC = sd(PISCIVORE),SD_PLANK = sd(PLANKTIVORE),SD_PRI = sd(PRIMARY),SD_SEC = sd(SECONDARY),SD_TOT = sd(TotFish),n=n()) %>% # calculate variance (sd) for each trophic level 
  drop_na() # drop strata with no data

# # !!!!!! filter for reef zones/islands NOT being surveyed here - i.e. any Guam MP's not being surveyed !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# smari<-smari %>% filter(SEC_NAME != "GUA_TUMON")
# smari<-droplevels(smari)

# get total vairance per island in order to get proportion of each torhpic group's variance
a<-smari %>% group_by(ISLAND) %>% 
  summarise(PISC=sum(SD_PISC),PLANK=sum(SD_PLANK),PRI=sum(SD_PRI),SEC=sum(SD_SEC),TOT=sum(SD_TOT)) 

# join totals back to original dataframe to calculate proportion
b<-full_join(smari,a,by="ISLAND") %>% 
  mutate(PROP_PISC = (SD_PISC/PISC),PROP_PLANK = (SD_PLANK/PLANK),PROP_PRI = (SD_PRI/PRI),PROP_SEC = (SD_SEC/SEC),PROP_TOT = (SD_TOT/TOT))

# calculate average variance for each strata - first isloate proportion of variance, get rid of sd columns
names(b)
c<-b[,c("ISLAND","SEC_NAME","REEF_ZONE" , "DEPTH_BIN","PROP_PISC" , "PROP_PLANK", "PROP_PRI","PROP_SEC","PROP_TOT" )]
d<-gather(c, troph, prop, -c(ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN)) %>% # melt data with 'gather' function to get all data into columns rather than rows
  group_by(ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN) %>% 
  summarise(mean_v=mean(prop)) # get the mean at the strata level

# get area values for each sector from the 'sectors' dataframe
colnames(sectors)
sec<-sectors %>% select(ISLAND, SEC_NAME, REEF_ZONE, DEPTH_BIN, AREA_HA)%>% 
  right_join(d, by=c("ISLAND", "SEC_NAME", "REEF_ZONE", "DEPTH_BIN")) # right joining to the previous dataframe selects values for just this region

e<-sec %>% group_by(ISLAND) %>% 
  summarize(area=sum(AREA_HA)) # summarize area by island

f<-full_join(sec,e,by="ISLAND") %>% 
  mutate(AREA_PCT=AREA_HA/area) %>% 
  select(ISLAND, SEC_NAME, REEF_ZONE, DEPTH_BIN,mean_v,AREA_PCT) # calculate proportional area for each strata

g<-gather(f, var, value, -c(ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN)) %>% # melt data with 'gather' function to get all data into columns rather than rows
  group_by(ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN) %>% 
  summarise(mean_v=prod(value)) # multiply the mean variance times the proportion of area of each strata

h<-g %>% group_by(ISLAND) %>% 
  summarize(sum=sum(mean_v))
i<-full_join(g,h,by="ISLAND") %>% 
  mutate(AREA_VAR_PCT=mean_v/sum) %>% # calculate the proportional weight for each strata
  select(ISLAND, SEC_NAME, REEF_ZONE, DEPTH_BIN,AREA_VAR_PCT) # clean up dataframe

# JUST A CHECK: does total variance for each island add up to 1?
test<-i %>% 
  group_by(ISLAND) %>% 
  summarize(sum=sum(AREA_VAR_PCT))
summary(test)

# add proportion of each strata by area along with the proportional weight 
j<-i %>% full_join(f,by=c("ISLAND", "SEC_NAME", "REEF_ZONE", "DEPTH_BIN"))
k<-select(j,-mean_v)

# to calculate allocation based on weights:
# get list of islands
unique(k$ISLAND)
# Value (n) is number of sites we can survey in 1 day (this comes from team lead, based on small boats and number of divers)
n<-6
# Plug in number of days we have at each island here (using each island's 3-letter code), and multiply based on number of sites we can survey in 1 day (n)
agu<-3*n 
gua<-11.6*n
rot<-6*n
sai<-11*n
tin<-5*n

#create a field for sites and give a dummy variable = 1
k$TOTSITES<-1
# for each island, plug in total sites we can survey during this visit
k[k$ISLAND %in% "Aguijan",]$TOTSITES<-agu
k[k$ISLAND %in% "Guam",]$TOTSITES<-gua
k[k$ISLAND %in% "Rota",]$TOTSITES<-rot
k[k$ISLAND %in% "Saipan",]$TOTSITES<-sai
k[k$ISLAND %in% "Tinian",]$TOTSITES<-tin

# to get the allocation, multiply the number of total sites we can survey at each island by the 2 different weighted values (area and variance, and just area)
k$WEIGHTED_ALLOCATION<-round(k$TOTSITES*k$AREA_VAR_PCT)
k$AREA_ALLOCATION<-round(k$TOTSITES*k$AREA_PCT)

# save file
write.csv(k,file="S_MARIANA_allocation.csv")

# Northern Marianas -------------------------------------------------------------------------

# filter past data for region and nSPC surveys and the last few years/rounds
nmari<-wsd %>% filter(REGION == "N.MARIAN", METHOD == "nSPC",OBS_YEAR>"2010") %>% 
  group_by(ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN) %>% # group by strata
  summarize(SD_PISC = sd(PISCIVORE),SD_PLANK = sd(PLANKTIVORE),SD_PRI = sd(PRIMARY),SD_SEC = sd(SECONDARY),SD_TOT = sd(TotFish),n=n()) %>% # calculate variance (sd) for each trophic level 
  drop_na() # drop strata with no data

# # !!!!!! filter for reef zones/islands NOT being surveyed here  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# nmari<-nmari %>% filter(ISLAND != "Guguan")
# nmari<-droplevels(nmari)

# get total vairance per island in order to get proportion of each torhpic group's variance
a<-nmari %>% group_by(ISLAND) %>% 
  summarise(PISC=sum(SD_PISC),PLANK=sum(SD_PLANK),PRI=sum(SD_PRI),SEC=sum(SD_SEC),TOT=sum(SD_TOT)) 

# join totals back to original dataframe to calculate proportion
b<-full_join(nmari,a,by="ISLAND") %>% 
  mutate(PROP_PISC = (SD_PISC/PISC),PROP_PLANK = (SD_PLANK/PLANK),PROP_PRI = (SD_PRI/PRI),PROP_SEC = (SD_SEC/SEC),PROP_TOT = (SD_TOT/TOT))

# calculate average variance for each strata - first isloate proportion of variance, get rid of sd columns
names(b)
c<-b[,c("ISLAND","SEC_NAME","REEF_ZONE" , "DEPTH_BIN","PROP_PISC" , "PROP_PLANK", "PROP_PRI","PROP_SEC","PROP_TOT" )]
d<-gather(c, troph, prop, -c(ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN)) %>% # melt data with 'gather' function to get all data into columns rather than rows
  group_by(ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN) %>% 
  summarise(mean_v=mean(prop)) # get the mean at the strata level

# get area values for each sector from the 'sectors' dataframe
head(sectors)
sec<-sectors %>% select(ISLAND, SEC_NAME, REEF_ZONE, DEPTH_BIN, AREA_HA)%>% 
  right_join(d, by=c("ISLAND", "SEC_NAME", "REEF_ZONE", "DEPTH_BIN")) # right joining to the previous dataframe selects values for just this region

e<-sec %>% group_by(ISLAND) %>% 
  summarize(area=sum(AREA_HA)) # summarize area by island

f<-full_join(sec,e,by="ISLAND") %>% 
  mutate(AREA_PCT=AREA_HA/area) %>% 
  select(ISLAND, SEC_NAME, REEF_ZONE, DEPTH_BIN,mean_v,AREA_PCT) # calculate proportional area for each strata

g<-gather(f, var, value, -c(ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN)) %>% # melt data with 'gather' function to get all data into columns rather than rows
  group_by(ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN) %>% 
  summarise(mean_v=prod(value)) # multiply the mean variance times the proportion of area of each strata

h<-g %>% group_by(ISLAND) %>% 
  summarize(sum=sum(mean_v))
i<-full_join(g,h,by="ISLAND") %>% 
  mutate(AREA_VAR_PCT=mean_v/sum) %>% # calculate the proportional weight for each strata
  select(ISLAND, SEC_NAME, REEF_ZONE, DEPTH_BIN,AREA_VAR_PCT) # clean up dataframe

# JUST A CHECK: does total variance for each island add up to 1?
test<-i %>% 
  group_by(ISLAND) %>% 
  summarize(sum=sum(AREA_VAR_PCT))
summary(test)

# add proportion of each strata by area along with the proportional weight 
j<-i %>% full_join(f,by=c("ISLAND", "SEC_NAME", "REEF_ZONE", "DEPTH_BIN"))
k<-select(j,-mean_v)

# to calculate allocation based on weights:
# get list of islands
unique(k$ISLAND)
# Value (n) is number of sites we can survey in 1 day (this comes from team lead, based on small boats and number of divers)
n<-6
# Plug in number of days we have at each island here (using each island's 3-letter code), and multiply based on number of sites we can survey in 1 day (n)
agr<-3*n 
ala<-11.6*n
asu<-6*n
fdp<-11*n
gug<-5*n
mau<-10*n
pag<-9*n
sar<-1*n

#create a field for sites and give a dummy variable = 1
k$TOTSITES<-1
# for each island, plug in total sites we can survey during this visit
k[k$ISLAND %in% "Agrihan",]$TOTSITES<-agr
k[k$ISLAND %in% "Alamagan",]$TOTSITES<-ala
k[k$ISLAND %in% "Asuncion",]$TOTSITES<-asu
k[k$ISLAND %in% "Farallon de Pajaros",]$TOTSITES<-fdp
k[k$ISLAND %in% "Guguan",]$TOTSITES<-gug
k[k$ISLAND %in% "Maug",]$TOTSITES<-mau
k[k$ISLAND %in% "Pagan",]$TOTSITES<-pag
k[k$ISLAND %in% "Sarigan",]$TOTSITES<-sar

# to get the allocation, multiply the number of total sites we can survey at each island by the 2 different weighted values (area and variance, and just area)
k$WEIGHTED_ALLOCATION<-round(k$TOTSITES*k$AREA_VAR_PCT)
k$AREA_ALLOCATION<-round(k$TOTSITES*k$AREA_PCT)

# save file
write.csv(k,file="N_MARIANA_allocation.csv")

# American Samoa -------------------------------------------------------------------------
#identify which target taxa you would like to use for allocation
taxasum<-subset(wsd,GENUS_CODE!="SSSS") %>% filter(REGION == "SAMOA", OBS_YEAR>"2014") %>% 
  group_by(ISLAND,GENUS_CODE) %>% # group by strata
  summarize(adults = mean(AdColDen),n=n()) %>% # calculate variance (sd) for each trophic level 
  drop_na()
View(taxasum)

#Filter taxa you would like to include
wsd<-wsd %>% filter(GENUS_CODE %in% c("MOSP","POCS","ACSP","FASP"))
wsd<-droplevels(wsd)


# filter past data for region and the last few years/rounds
sam<-wsd %>% filter(REGION == "SAMOA", OBS_YEAR>"2014") %>% 
  group_by(ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN,GENUS_CODE) %>% # group by strata
  summarize(SD_AdColDen = sd(AdColDen),n=n()) %>% # calculate variance (sd) for each genus
  drop_na() # drop strata with no data
View(sam)

# # # !!!!!! filter for reef zones/islands NOT being surveyed here !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
unique(sam$SEC_NAME)
sam<-sam %>% filter(SEC_NAME %in% sectors$SEC_NAME)
sam<-droplevels(sam)

sam<-sam %>% pivot_wider(names_from=GENUS_CODE, values_from=SD_AdColDen,values_fill=NA)
head(sam)
colnames(sam)[6:9] <- paste("SD", colnames(sam[,c(6:9)]), sep = "_") #add "SD" prefix


# get total variance per island in order to get proportion of each genus's variance
a<-sam %>% group_by(ISLAND) %>% 
  summarise(MOSP=sum(SD_MOSP),POCS=sum(SD_POCS),ACSP=sum(SD_ACSP),FASP=sum(SD_FASP)) 

# join totals back to original dataframe to calculate proportion
b<-full_join(sam,a,by="ISLAND") %>% 
  mutate(PROP_MOSP = (SD_MOSP/MOSP),PROP_POCS = (SD_POCS/POCS),PROP_ACSP = (SD_ACSP/ACSP),PROP_FASP = (SD_FASP/FASP))

# calculate average variance for each strata - first isloate proportion of variance, get rid of sd columns
names(b)
c<-b[,c("ISLAND","SEC_NAME","REEF_ZONE" , "DEPTH_BIN","PROP_MOSP" , "PROP_POCS", "PROP_ACSP","PROP_FASP")]
d<-gather(c, genus, prop, -c(ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN)) %>% # melt data with 'gather' function to get all data into columns rather than rows
  group_by(ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN) %>% 
  summarise(mean_v=mean(prop)) # get the mean at the strata level

# get area values for each sector from the 'sectors' dataframe
#head(sectors)
sec<-sectors %>% select(ISLAND, SEC_NAME, REEF_ZONE, DEPTH_BIN, AREA_HA)%>% 
  right_join(d, by=c("ISLAND", "SEC_NAME", "REEF_ZONE", "DEPTH_BIN")) # right joining to the previous dataframe selects values for just this region

e<-sec %>% group_by(ISLAND) %>% 
  summarize(area=sum(AREA_HA)) # summarize area by island

f<-full_join(sec,e,by="ISLAND") %>% 
  mutate(AREA_PCT=AREA_HA/area) %>% 
  select(ISLAND, SEC_NAME, REEF_ZONE, DEPTH_BIN,mean_v,AREA_PCT) # calculate proportional area for each strata

g<-gather(f, var, value, -c(ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN)) %>% # melt data with 'gather' function to get all data into columns rather than rows
  group_by(ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN) %>% 
  summarise(mean_v=prod(value)) # multiply the mean variance times the proportion of area of each strata

h<-g %>% group_by(ISLAND) %>% 
  summarize(sum=sum(mean_v))
i<-full_join(g,h,by="ISLAND") %>% 
  mutate(AREA_VAR_PCT=mean_v/sum) %>% # calculate the proportional weight for each strata
  select(ISLAND, SEC_NAME, REEF_ZONE, DEPTH_BIN,AREA_VAR_PCT) # clean up dataframe

# JUST A CHECK: does total variance for each island add up to 1?
test<-i %>% 
  group_by(ISLAND) %>% 
  summarize(sum=sum(AREA_VAR_PCT))
summary(test)

# add proportion of each strata by area along with the proportional weight 
j<-i %>% full_join(f,by=c("ISLAND", "SEC_NAME", "REEF_ZONE", "DEPTH_BIN"))
k<-select(j,-mean_v)

# to calculate allocation based on weights:
# get list of islands
unique(k$ISLAND)
# Value (n) is number of sites we can survey in 1 day (this comes from team lead, based on small boats and number of divers)
n<-7
# Plug in number of days we have at each island here (using each island's 3-letter code), and multiply based on number of sites we can survey in 1 day (n)

ofu<-5*n 
ros<-4*n
swa<-4*n
tau<-4*n
tut<-15*n

#create a field for sites and give a dummy variable = 1
k$TOTSITES<-1
# for each island, plug in total sites we can survey during this visit
k[k$ISLAND %in% "Ofu & Olosega",]$TOTSITES<-ofu
k[k$ISLAND %in% "Rose",]$TOTSITES<-ros
k[k$ISLAND %in% "Swains",]$TOTSITES<-swa
k[k$ISLAND %in% "Tau",]$TOTSITES<-tau
k[k$ISLAND %in% "Tutuila",]$TOTSITES<-tut

# to get the allocation, multiply the number of total sites we can survey at each island by the 2 different weighted values (area and variance, and just area)
k$WEIGHTED_ALLOCATION<-round(k$TOTSITES*k$AREA_VAR_PCT)
k$AREA_ALLOCATION<-round(k$TOTSITES*k$AREA_PCT)

# save file
write.csv(k,file="SAMOA_2022allocation_bystratum.csv")

# PRIAs -------------------------------------------------------------------------
taxasum<-subset(wsd,GENUS_CODE!="SSSS") %>% filter(REGION == "PRIAs", OBS_YEAR>="2015") %>% 
  group_by(ISLAND,GENUS_CODE) %>% # group by strata
  summarize(adults = mean(AdColDen),n=n()) %>% # calculate variance (sd) for each trophic level 
  drop_na()
View(taxasum)

#Filter taxa you would like to include
wsd<-wsd %>% filter(GENUS_CODE %in% c("POSP","POCS","ACSP","FASP"))
wsd<-droplevels(wsd)


# filter past data for region and the last few years/rounds
sam<-wsd %>% filter(REGION == "PRIAs", OBS_YEAR>="2015") %>% 
  group_by(ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN,GENUS_CODE) %>% # group by strata
  summarize(SD_AdColDen = sd(AdColDen),n=n()) %>% # calculate variance (sd) for each genus
  drop_na() # drop strata with no data
View(sam)

# # # !!!!!! filter for reef zones/islands NOT being surveyed here !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
unique(sam$SEC_NAME)
sam<-sam %>% filter(SEC_NAME %in% sectors$SEC_NAME)
sam<-droplevels(sam)

sam<-sam %>% pivot_wider(names_from=GENUS_CODE, values_from=SD_AdColDen,values_fill=NA)
head(sam)
colnames(sam)[6:9] <- paste("SD", colnames(sam[,c(6:9)]), sep = "_") #add "SD" prefix


# get total variance per island in order to get proportion of each genus's variance
a<-sam %>% group_by(ISLAND) %>% 
  summarise(POSP=sum(SD_POSP),POCS=sum(SD_POCS),ACSP=sum(SD_ACSP),FASP=sum(SD_FASP)) 

# join totals back to original dataframe to calculate proportion
b<-full_join(sam,a,by="ISLAND") %>% 
  mutate(PROP_POSP = (SD_POSP/POSP),PROP_POCS = (SD_POCS/POCS),PROP_ACSP = (SD_ACSP/ACSP),PROP_FASP = (SD_FASP/FASP))

# calculate average variance for each strata - first isloate proportion of variance, get rid of sd columns
names(b)
c<-b[,c("ISLAND","SEC_NAME","REEF_ZONE" , "DEPTH_BIN","PROP_POSP" , "PROP_POCS", "PROP_ACSP","PROP_FASP")]
d<-gather(c, genus, prop, -c(ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN)) %>% # melt data with 'gather' function to get all data into columns rather than rows
  group_by(ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN) %>% 
  summarise(mean_v=mean(prop)) # get the mean at the strata level

# get area values for each sector from the 'sectors' dataframe
#head(sectors)
sec<-sectors %>% select(ISLAND, SEC_NAME, REEF_ZONE, DEPTH_BIN, AREA_HA)%>% 
  right_join(d, by=c("ISLAND", "SEC_NAME", "REEF_ZONE", "DEPTH_BIN")) # right joining to the previous dataframe selects values for just this region

e<-sec %>% group_by(ISLAND) %>% 
  summarize(area=sum(AREA_HA)) # summarize area by island

f<-full_join(sec,e,by="ISLAND") %>% 
  mutate(AREA_PCT=AREA_HA/area) %>% 
  select(ISLAND, SEC_NAME, REEF_ZONE, DEPTH_BIN,mean_v,AREA_PCT) # calculate proportional area for each strata

g<-gather(f, var, value, -c(ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN)) %>% # melt data with 'gather' function to get all data into columns rather than rows
  group_by(ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN) %>% 
  summarise(mean_v=prod(value)) # multiply the mean variance times the proportion of area of each strata

h<-g %>% group_by(ISLAND) %>% 
  summarize(sum=sum(mean_v))
i<-full_join(g,h,by="ISLAND") %>% 
  mutate(AREA_VAR_PCT=mean_v/sum) %>% # calculate the proportional weight for each strata
  select(ISLAND, SEC_NAME, REEF_ZONE, DEPTH_BIN,AREA_VAR_PCT) # clean up dataframe

# JUST A CHECK: does total variance for each island add up to 1?
test<-i %>% 
  group_by(ISLAND) %>% 
  summarize(sum=sum(AREA_VAR_PCT))
summary(test)

# add proportion of each strata by area along with the proportional weight 
j<-i %>% full_join(f,by=c("ISLAND", "SEC_NAME", "REEF_ZONE", "DEPTH_BIN"))
k<-select(j,-mean_v)

# to calculate allocation based on weights:
# get list of islands
unique(k$ISLAND)
# Value (n) is number of sites we can survey in 1 day (this comes from team lead, based on small boats and number of divers)
n<-6
# Plug in number of days we have at each island here (using each island's 3-letter code), and multiply based on number of sites we can survey in 1 day (n)

jar<-5*n 
pal<-8*n
kin<-5*n
how<-4*n
bak<-4*n

#create a field for sites and give a dummy variable = 1
k$TOTSITES<-1
# for each island, plug in total sites we can survey during this visit

k[k$ISLAND %in% "Jarvis",]$TOTSITES<-jar
k[k$ISLAND %in% "Palmyra",]$TOTSITES<-pal
k[k$ISLAND %in% "Kingman",]$TOTSITES<-kin
k[k$ISLAND %in% "Howland",]$TOTSITES<-how
k[k$ISLAND %in% "Baker",]$TOTSITES<-bak

# to get the allocation, multiply the number of total sites we can survey at each island by the 2 different weighted values (area and variance, and just area)
k$WEIGHTED_ALLOCATION<-round(k$TOTSITES*k$AREA_VAR_PCT)
k$AREA_ALLOCATION<-round(k$TOTSITES*k$AREA_PCT)

# save file
write.csv(k,file="PRIAs_2022allocation_bystratum2015-2018.csv")

# MHIs -------------------------------------------------------------------------

# filter past data for region and nSPC surveys and the last few years/rounds
mhia<-wsd %>% filter(REGION == "MHI", METHOD == "nSPC",OBS_YEAR>"2008") %>% 
  group_by(ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN) %>% # group by strata
  summarize(SD_PISC = sd(PISCIVORE),SD_PLANK = sd(PLANKTIVORE),SD_PRI = sd(PRIMARY),SD_SEC = sd(SECONDARY),SD_TOT = sd(TotFish),n=n()) %>% # calculate variance (sd) for each trophic level 
  drop_na() # drop strata with no data

# # # !!!!!! filter for reef zones/islands NOT being surveyed here !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# mhi<-mhia %>% filter(SEC_NAME != "HAW_SE",SEC_NAME != "KAU_NAPALI", SEC_NAME != "LAN_NORTH", SEC_NAME != "MAI_HANA", SEC_NAME != "MAI_NW",SEC_NAME != "MOL_PALI",SEC_NAME != "MOL_WEST", SEC_NAME !="OAH_EAST", SEC_NAME !="MAI_SE",SEC_NAME !="MAI_MOLOKINI")
# mhi<-droplevels(mhi)

# get total vairance per island in order to get proportion of each torhpic group's variance
a<-mhi %>% group_by(ISLAND) %>% 
  summarise(PISC=sum(SD_PISC),PLANK=sum(SD_PLANK),PRI=sum(SD_PRI),SEC=sum(SD_SEC),TOT=sum(SD_TOT)) 

# join totals back to original dataframe to calculate proportion
b<-full_join(mhi,a,by="ISLAND") %>% 
  mutate(PROP_PISC = (SD_PISC/PISC),PROP_PLANK = (SD_PLANK/PLANK),PROP_PRI = (SD_PRI/PRI),PROP_SEC = (SD_SEC/SEC),PROP_TOT = (SD_TOT/TOT))

# calculate average variance for each strata - first isloate proportion of variance, get rid of sd columns
names(b)
c<-b[,c("ISLAND","SEC_NAME","REEF_ZONE" , "DEPTH_BIN","PROP_PISC" , "PROP_PLANK", "PROP_PRI","PROP_SEC","PROP_TOT" )]
d<-gather(c, troph, prop, -c(ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN)) %>% # melt data with 'gather' function to get all data into columns rather than rows
  group_by(ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN) %>% 
  summarise(mean_v=mean(prop)) # get the mean at the strata level

# get area values for each sector from the 'sectors' dataframe
#head(sectors)
sec<-sectors %>% select(ISLAND, SEC_NAME, REEF_ZONE, DEPTH_BIN, AREA_HA)%>% 
  right_join(d, by=c("ISLAND", "SEC_NAME", "REEF_ZONE", "DEPTH_BIN")) # right joining to the previous dataframe selects values for just the selected region


# 2015 add  MOL_NW, NII_LEHUA, MAI_KAHULUI to get area
s<-sectors %>% select(ISLAND, SEC_NAME, REEF_ZONE, DEPTH_BIN, AREA_HA)%>%
  filter(SEC_NAME == "MOL_NW"|SEC_NAME =="MAI_KAHULUI"|SEC_NAME =="NII_LEHUA") %>% 
  mutate(mean_v=0)
t<-sectors %>% select(ISLAND, SEC_NAME, REEF_ZONE, DEPTH_BIN, AREA_HA)%>%
  filter(SEC_NAME == "MAI_LAHAINA" & DEPTH_BIN == "Deep") %>% 
  mutate(mean_v=0)
sec<-rbind(sec,s,t)

e<-sec %>% group_by(ISLAND) %>% 
  summarize(area=sum(AREA_HA)) # summarize area by island

f<-full_join(sec,e,by="ISLAND") %>% 
  mutate(AREA_PCT=AREA_HA/area) %>% 
  select(ISLAND, SEC_NAME, REEF_ZONE, DEPTH_BIN,mean_v,AREA_PCT) # calculate proportional area for each strata

g<-gather(f, var, value, -c(ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN)) %>% # melt data with 'gather' function to get all data into columns rather than rows
  group_by(ISLAND,SEC_NAME,REEF_ZONE,DEPTH_BIN) %>% 
  summarise(mean_v=prod(value)) # multiply the mean variance times the proportion of area of each strata

h<-g %>% group_by(ISLAND) %>% 
  summarize(sum=sum(mean_v))
i<-full_join(g,h,by="ISLAND") %>% 
  mutate(AREA_VAR_PCT=mean_v/sum) %>% # calculate the proportional weight for each strata
  select(ISLAND, SEC_NAME, REEF_ZONE, DEPTH_BIN,AREA_VAR_PCT) # clean up dataframe

# JUST A CHECK: does total variance for each island add up to 1?
test<-i %>% 
  group_by(ISLAND) %>% 
  summarize(sum=sum(AREA_VAR_PCT))
summary(test)

# add proportion of each strata by area along with the proportional weight 
j<-i %>% full_join(f,by=c("ISLAND", "SEC_NAME", "REEF_ZONE", "DEPTH_BIN"))
k<-select(j,-mean_v)

# to calculate allocation based on weights:
# get list of islands
unique(k$ISLAND)
# Value (n) is number of sites we can survey in 1 day (this comes from team lead, based on small boats and number of divers)
n<-6
# Plug in number of days we have at each island here (using each island's 3-letter code), and multiply based on number of sites we can survey in 1 day (n)
haw<-3*n 
kau<-11.6*n
lan<-6*n
mai<-11*n
mol<-5*n
nii<-5*n
oah<-4*n

#create a field for sites and give a dummy variable = 1
k$TOTSITES<-1
# for each island, plug in total sites we can survey during this visit

k[k$ISLAND %in% "Hawaii",]$TOTSITES<-haw
k[k$ISLAND %in% "Kauai",]$TOTSITES<-kau
k[k$ISLAND %in% "Lanai",]$TOTSITES<-lan
k[k$ISLAND %in% "Maui",]$TOTSITES<-mai
k[k$ISLAND %in% "Molokai",]$TOTSITES<-mol
k[k$ISLAND %in% "Niihau",]$TOTSITES<-nii
k[k$ISLAND %in% "Oahu",]$TOTSITES<-oah

# to get the allocation, multiply the number of total sites we can survey at each island by the 2 different weighted values (area and variance, and just area)
k$WEIGHTED_ALLOCATION<-round(k$TOTSITES*k$AREA_VAR_PCT)
k$AREA_ALLOCATION<-round(k$TOTSITES*k$AREA_PCT)


# save file
write.csv(k,file="D:/CRED/cruise/allocation/cruise_allocation/MHI_allocation.csv")
