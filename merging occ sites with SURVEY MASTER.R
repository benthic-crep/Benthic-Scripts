#Read in SURVEY MASTER file
sm<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SURVEY MASTER.csv")
sm$SITE<-SiteNumLeadingZeros(sm$SITE) #add 000 to site to prevent MAR-22 from being changed to March 22

#Read in OCC site table to merge missing OCC_SITEID names
occ<-read.csv("T:/Benthic/Data/Lookup Tables/OCC_LIST_OF_SITES_instrumentsummary_22OCT2020.csv")
names(occ)[names(occ) == "REA_SITE"] <- "SITE" #Change column name to match SM
occ$SITE<-SiteNumLeadingZeros(occ$SITE)

#There are multiple OCC_SITEIDs with the same REA_SITE name in the occ df. They are located across different depth strata, some are active and inactive and some have differences in instrumentation
#The following section of code subsets and adds a depth bin column to help merge with the SURVEY MASTER file. This took a lot of fiddling!!!

occ<-subset(occ,STATUS=="Active") #remove inactive sites

#Manually drop the following OCC sites from OCC site list- they weren't surveyed for photoquads and are causing merging issues
occ<-occ[!(occ$OCC_SITEID %in% c("OCC-GUA-016","OCC-PAG-003")),];head(occ)

#add depth bin column to occ
# occ$DEPTH_BIN<-NULL
# for (i in c(1:nrow(occ))){ #opening brace
#   if(occ$SITE_DEPTH_M[i] >0 & occ$SITE_DEPTH_M[i]<=6){ #c&p
#     occ$DEPTH_BIN[i] = "Shallow" #c&p
#   } #c&p
#   if(occ$SITE_DEPTH_M[i] >6 & occ$SITE_DEPTH_M[i]<=18){ #c&p
#     occ$DEPTH_BIN[i]= "Mid" #c&p
#   } #c&p
#   if(occ$SITE_DEPTH_M[i] >18 ){ #c&p
#     occ$DEPTH_BIN[i]= "Deep" #c&p
#   } #c&p
# }

head(occ)


nrow(sm)
sm.new<-left_join(sm,occ[,c("SITE","OCC_SITEID")]);nrow(sm.test) #Join dfs together and make sure sitevisitIDS weren't droped
tmp<-sm.new[duplicated(sm.new$SITEVISITID),];head(tmp);nrow(tmp) #double check there aren't duplicate SITEVISITIDs



nwhi<-subset(cnet,REGION=="NWHI"&OBS_YEAR=="2019")
tmp2<-ddply(nwhi,.(SITEVISITID,SITE,OCC_SITEID),summarize,tmp=length(REPLICATE));tmp2
names(tmp2)[names(tmp2) == "OCC_SITEID"] <- "OCC_SITEID2"

sm.new2<-left_join(sm.new,tmp2[,c("SITEVISITID","OCC_SITEID2")]);nrow(sm.new2) #Join dfs together and make sure sitevisitIDS weren't droped
View(sm.new2)
write.csv(sm.new2,file="C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/data/SURVEY MASTER_new.csv",row.names = F)
