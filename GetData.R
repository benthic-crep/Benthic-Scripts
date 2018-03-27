#### In order to use this script you will need to contact ITS (ask for Tim Lee if he is available) to set up the ODBC drivers on your computer. You will not have adminstrative rights
# You will also need to set up an oracle account if you haven't already
##Once everything it set up you can download the individual datasets. If you experience errors telling you the datasets can't be found double check your oracle account to make sure you have access
#if you don't have access you will need to ask someone in Data Management to have you access to whatever views you are interested in.


rm(list=ls())
library(RODBC)            # to connect to oracle
setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Benthic REA")

Sys.setenv(ODBCINI = "/library/ODBC/odbc.ini")

##*******## jump down to Benthic REA WORKINGS if already saved as a .rfile
ch <- odbcConnect("CRED_Oracle", uid = "ccouch", pwd = "XXXXXXXXX")
##
## #list available tables
tv<-sqlTables(ch, tableType = "VIEW")
a<-as.vector(tv$TABLE_NAME[grep("V0_", as.character(tv$TABLE_NAME))])
b<-as.vector(tv$TABLE_NAME[grep("V_BIA_PERC_COVER_PHOTO_STR", as.character(tv$TABLE_NAME))])
c<-as.vector(tv$TABLE_NAME[grep("V_BIA", as.character(tv$TABLE_NAME))])
d<-as.vector(tv$TABLE_NAME[grep("MV_BIA", as.character(tv$TABLE_NAME))])

##
rawtables<-c(a,b,c,d)
rawtables
##
df <- sqlQuery(ch, paste("SELECT * FROM GISDAT.V0_CORAL_OBS_E")); head(df)
save(df, file="ALL_REA_ADULTCORAL_RAW.rdata")

df <- sqlQuery(ch, paste("SELECT * FROM GISDAT.V0_CORAL_OBS_F")); head(df)
save(df, file="ALL_REA_JUVCORAL_RAW.rdata")

#BENTHIC Photoquad REA
#photoquad data from CPCe
bia <- sqlQuery(ch, paste("SELECT * FROM GISDAT.V_BIA_PERC_COVER_PHOTO_STR_")); head(bia)
save(bia, file="ALL_BIA_STR_RAW_NEW.rdata")

cli <- sqlQuery(ch, paste("SELECT * FROM GISDAT.V_BIA_PERC_COVER_PHOTO_CLI_")); head(cli)
save(cli, file="ALL_BIA_CLIMATE_PERM.rdata")


# Coral Net Photoquad Data
cnet <- sqlQuery(ch, paste("SELECT * FROM GISDAT.MV_BIA_CNET_ANALYSIS_DATA")); head(cnet)
save(cnet, file="C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/BIA/ALL_BIA_STR_CNET.rdata")

#LPI data 2002-2012
lpi<-sqlQuery(ch, paste("SELECT * FROM GISDAT.V0_BENT_LPI")); head(lpi)
#save(lpi, file="ESD_BENTHIC_LPI.rdata")
write.csv(lpi,"ESD_BENTHIC_LPI.csv")

#Towed Diver Survey data
tds<-sqlQuery(ch, paste("SELECT * FROM GISDAT.V0_BENT_TDS")); head(tds)
write.csv(tds,"ESD_BENTHIC_TDS.csv")

