
rm(list=ls())

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_BSR.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")

#Original SITEVISIT table from Oracle
tv<-read.csv(file="C:/Users/Courtney.S.Couch/Documents/GitHub/tmpALLsite-sitevisit.csv")
head(tv)

#Streamlined Sitevisit table 
load("~/GitHub/fish-paste/data/SURVEY METDATA.RData")
head(sv)

sv[sv$OBS_YEAR=="2015" & sv$ISLAND=="Jarvis",]

#Site visit table with manually entered sector and analysis pooling information 
mod<-read.csv(file="C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/MOD SURVEY MASTER.csv")
head(mod)
