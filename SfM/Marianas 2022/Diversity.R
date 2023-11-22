ad_sfm<-read.csv("T:/Benthic/Data/SfM/Analysis Ready/ASRAMP23_SfM_Adult_CLEANED.csv")
ad_sfm2<-read.csv("T:/Benthic/Data/SfM/Analysis Ready/MARAMP22_SfM_Adult_CLEANED.csv")
ad_sfm <- bind_rows(ad_sfm, ad_sfm2)


ad_sfm$SITE_SEG<-paste(ad_sfm$SITE,ad_sfm$SEGMENT,sep ="_")
length(unique(ad_sfm$SITE_SEG));length(unique(ad_sfm$SITE)) #should be 389 ss and 104 sites

ad_sfm <- subset(ad_sfm, TRANSECT == "A")

# PREP VISUAL DIVER DATA ---------------------------------------------
## LOAD benthic data
awd_<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_Adults_raw_CLEANED.csv")

awd<-subset(awd_,OBS_YEAR=="2023"|OBS_YEAR=="2022")


#Changing Analyst names to match SfM
awd$DIVER<-ifelse(awd$DIVER=="J_E","JE",as.character(awd$DIVER))
awd$DIVER<-ifelse(awd$DIVER=="J_C","JC",as.character(awd$DIVER))


#Colony fragments and scleractinans are subseted in the functions 
#Add a column for adult fragments so we can remove them from the dataset later (-1 indicates fragment)
colnames(awd)[colnames(awd)=="Fragment"]<-"FRAGMENT"
awd$FRAGMENT[is.na(awd$FRAGMENT)] <- 0

#Simplify Bleaching Severity categories: in 2019 the team decided to simplify the bleaching severity from 1-5 to 1-3 to improve consistency in severity values
#This code converts the severity data collected prior to 2019 to a 1-3 scale
awd$DATE_ <- as.Date(awd$DATE_, format = "%Y-%m-%d")

colnames(awd)[which(colnames(awd) == 'DIVER')] <- "ANALYST"
awd$EX_BOUND<-0;awd$EX_BOUND<-as.numeric(awd$EX_BOUND) #add ex_bound columns to match SfM



#Calculate segment area
awd$SEGAREA<-awd$SEGLENGTH*awd$SEGWIDTH

#Only include sites and segments surveyed by divers during HARAMP 2019
awd$SITE_SEG<-paste(awd$SITE,awd$SEGMENT,sep="_")

#remove columns that aren't needed & 
awd<-dplyr::select(awd,-c(TRANSECTAREA,bANALYSIS_SCHEME,ANALYSIS_YEAR,REGION_NAME,NO_SURVEY_YN,DATE_,ISLANDCODE))
awd<-dplyr::filter(awd, SITE_SEG %in% c(ad_sfm$SITE_SEG));head(awd) 


##
length(unique(awd$SITE))


ad_sfm_sub<-dplyr::filter(ad_sfm, SITE_SEG %in% c(awd$SITE_SEG));nrow(ad_sfm);nrow(ad_sfm_sub)

awd_sub<-dplyr::filter(awd, SITE_SEG %in% c(ad_sfm_sub$SITE_SEG))

length(unique(ad_sfm_sub$SITE));length(unique(ad_sfm_sub$SITE_SEG))

# Add Method column to diver data
awd$METHOD<-"Diver";awd$METHOD<-as.factor(awd$METHOD)


#Merge diver and SfM data
sort(colnames(awd))
sort(colnames(ad_sfm_sub))

sapply(awd,class)
sapply(ad_sfm,class)

awd.all<-rbind(ad_sfm_sub[,-13],awd[,-17])
awd.all$SEGWIDTH[is.na(awd.all$SEGWIDTH)] <- 1
awd.all$SEGAREA<-awd.all$SEGLENGTH*awd.all$SEGWIDTH
awd.all$TRANSECT <- recode_factor(awd.all$TRANSECT, "1" = "A")
awd.all <- subset(awd.all, awd.all$TRANSECT == "A")

awd.all$TRANSECTAREA<-TransectareaMETHOD(awd.all)


SURVEY_COL<-c("METHOD","SITEVISITID", "OBS_YEAR", "REGION", "ISLAND","SEC_NAME", "SITE", "REEF_ZONE",
              "DEPTH_BIN", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M","TRANSECT","SEGMENT","COLONYID","GENUS_CODE","TAXONCODE","SPCODE","COLONYLENGTH")
survey_colony<-unique(awd.all[,SURVEY_COL])

SURVEY_SITE<-c("METHOD","SITEVISITID", "OBS_YEAR", "REGION", "ISLAND","SEC_NAME", "SITE", "REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M")
survey_site<-unique(awd.all[,SURVEY_SITE])
visits <- survey_site$SITEVISITID[survey_site$SEC_NAME == ""]
for (i in 1:length(visits)) {
survey_site$SEC_NAME[survey_site$SITEVISITID == visits[i] & survey_site$METHOD == "SfM"] <- survey_site$SEC_NAME[survey_site$SITEVISITID == visits[i] & survey_site$METHOD == "Diver"]
}


SURVEY<-c("METHOD","SITEVISITID", "OBS_YEAR", "REGION", "ISLAND","SEC_NAME", "SITE", "REEF_ZONE",
          "DEPTH_BIN", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M","METHOD","TRANSECT","SEGMENT")
survey_segment<-unique(awd.all[,SURVEY])

nrow(survey_site)
sectors<-read.csv("C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Sectors-Strata-Areas.csv", stringsAsFactors=FALSE)
meta<-left_join(survey_site,sectors)
meta[which(is.na(meta$AREA_HA)),]
nrow(survey_site)
nrow(meta)



#TEMPORARY WORK AROUND-ASK MICHAEL TO FIX
#survey_site$REEF_ZONE<-ifelse(survey_site$SITE=="HAW-04285","Forereef",as.character(survey_site$REEF_ZONE))
#survey_segment$REEF_ZONE<-ifelse(survey_segment$SITE=="HAW-04285","Forereef",as.character(survey_segment$REEF_ZONE))
colnames(awd.all)[colnames(awd.all)=="FRAGMENT"]<-"Fragment"
ex_b<-subset(awd.all,EX_BOUND==0)

taxa <- unique(awd.all$GENUS_CODE)
acd.div <- acd.div_G
acd.div_G$Removed <- "NONE"

for (i in 1:length(taxa)) {
acd.div_G<-Calc_Diversity_Transect(subset(awd.all, awd.all$GENUS_CODE != taxa[i]),"GENUS_CODE");colnames(acd.div_G)[4:7] <- paste("Adult", colnames(acd.div_G[,c(4:7)]), sep = "_")
acd.div_G$Removed <- taxa[i]
acd.div <- bind_rows(acd.div, acd.div_G)
}

remove_5 <-  c("AAAA", doms.order$GENUS_CODE[doms.order$rank < (nrow(doms.order)-5)])
remove_10 <- c("AAAA", doms.order$GENUS_CODE[doms.order$rank < (nrow(doms.order)-10)])
remove_15 <- c("AAAA", doms.order$GENUS_CODE[doms.order$rank < (nrow(doms.order)-15)])
remove_25 <- c("AAAA", doms.order$GENUS_CODE[doms.order$rank < (nrow(doms.order)-25)])
remove_top <- c("AAAA", doms.order$GENUS_CODE[doms.order$rank < 4])

acd.div.as<-Calc_Diversity_Transect(subset(awd.all, awd.all$ANALYST != "SAMOA"),"GENUS_CODE");colnames(acd.div.as)[4:7] <- paste("Adult", colnames(acd.div.as[,c(4:7)]), sep = "_")
acd.div.marian<-Calc_Diversity_Transect(subset(awd.all, awd.all$REGION == "MARIAN"),"GENUS_CODE");colnames(acd.div.marian)[4:7] <- paste("Adult", colnames(acd.div.marian[,c(4:7)]), sep = "_")
acd.div.5$count <- "5"
acd.div.10<-Calc_Diversity_Transect(subset(awd.all, awd.all$GENUS_CODE %in% remove_10),"GENUS_CODE");colnames(acd.div.10)[4:7] <- paste("Adult", colnames(acd.div.10[,c(4:7)]), sep = "_")
acd.div.10$count <- "10"
acd.div.15<-Calc_Diversity_Transect(subset(awd.all, awd.all$GENUS_CODE %in% remove_15),"GENUS_CODE");colnames(acd.div.15)[4:7] <- paste("Adult", colnames(acd.div.15[,c(4:7)]), sep = "_")
acd.div.15$count <- "15"
acd.div.30<-Calc_Diversity_Transect(subset(awd.all, awd.all$GENUS_CODE %in% remove_25),"GENUS_CODE");colnames(acd.div.30)[4:7] <- paste("Adult", colnames(acd.div.30[,c(4:7)]), sep = "_")
acd.div.30$count <- "30"
acd.div.top<-Calc_Diversity_Transect(subset(awd.all, awd.all$GENUS_CODE %in% remove_top),"GENUS_CODE");colnames(acd.div.top)[4:7] <- paste("Adult", colnames(acd.div.top[,c(4:7)]), sep = "_")
acd.div.top$count <- "top"
acd.div<-Calc_Diversity_Transect(awd.all,"TAXONCODE");colnames(acd.div)[4:7] <- paste("Adult", colnames(acd.div[,c(4:7)]), sep = "_")
acd.div$count <- "all"

acd.div.diver<- Calc_Diversity_Transect(subset(awd.all, awd.all$METHOD == "Diver"),"GENUS_CODE");colnames(acd.div.diver)[4:7] <- paste("Adult", colnames(acd.div.diver[,c(4:7)]), sep = "_")
acd.div.JC<-Calc_Diversity_Transect(subset(awd.all, awd.all$ANALYST == "JC" & awd.all$METHOD == "SfM"),"GENUS_CODE");colnames(acd.div.JC)[4:7] <- paste("Adult", colnames(acd.div.JC[,c(4:7)]), sep = "_")
acd.div.CA<-Calc_Diversity_Transect(subset(awd.all, awd.all$ANALYST == "CA" & awd.all$METHOD == "SfM"),"GENUS_CODE");colnames(acd.div.CA)[4:7] <- paste("Adult", colnames(acd.div.CA[,c(4:7)]), sep = "_")
acd.div.MSL<-Calc_Diversity_Transect(subset(awd.all, awd.all$ANALYST == "MSL"& awd.all$METHOD == "SfM"),"GENUS_CODE");colnames(acd.div.MSL)[4:7] <- paste("Adult", colnames(acd.div.MSL[,c(4:7)]), sep = "_")
acd.div.IGB<-Calc_Diversity_Transect(subset(awd.all, awd.all$ANALYST == "IGB"& awd.all$METHOD == "SfM"),"GENUS_CODE");colnames(acd.div.IGB)[4:7] <- paste("Adult", colnames(acd.div.IGB[,c(4:7)]), sep = "_")

acd.div.JC<-bind_rows(acd.div.JC, subset(acd.div.diver, acd.div.diver$SITE %in% unique(acd.div.JC$SITE)))
acd.div.CA<-bind_rows(acd.div.CA, subset(acd.div.diver, acd.div.diver$SITE %in% unique(acd.div.CA$SITE)))
acd.div.MSL<-bind_rows(acd.div.MSL, subset(acd.div.diver, acd.div.diver$SITE %in% unique(acd.div.MSL$SITE)))
acd.div.IGB<-bind_rows(acd.div.IGB, subset(acd.div.diver, acd.div.diver$SITE %in% unique(acd.div.IGB$SITE)))

acd.div.JC$count <- "JC" 
acd.div.CA$count <- "CA"
acd.div.MSL$count <- "MSL"
acd.div.IGB$count <- "IGB"
all.annot <- bind_rows(acd.div.JC, acd.div.CA, acd.div.MSL, acd.div.IGB)

all.div <- bind_rows(acd.div, acd.div.5, acd.div.10, acd.div.15, acd.div.30, acd.div.top)

all.diversity<-left_join(acd.div,meta)


all.diversity$METHOD[all.diversity$METHOD == "Diver"] <- "DIVER"

sfm_div<-subset(all.diversity,METHOD=="SfM")
diver_div<-subset(all.diversity,METHOD=="DIVER")

# #Set up in wide format
colnames(sfm_div)[c(4:7)] <- paste("SfM_", colnames(sfm_div[,c(4:7)]), sep = "");sfm_div<-dplyr::select(sfm_div,-c(METHOD))

colnames(diver_div)[c(4:7)] <- paste("Diver_", colnames(diver_div[,c(4:7)]), sep = "");diver_div<-dplyr::select(diver_div,-c(METHOD))

div.wide<-left_join(sfm_div,diver_div[,2:7])
div.marian <- div.wide

div <- all.diversity


hist(div$Adult_Shannon)
m<-lmer(Adult_Shannon~METHOD + (1|SEC_NAME),data=div)
DPlots(m,div)
leveneTest(Adult_Shannon~METHOD, data = div)

hist(div$Adult_Richness)
m<-lmer(Adult_Richness~METHOD + (1|SEC_NAME),data=div)
DPlots(m,div)
leveneTest(Adult_Richness~METHOD, data = div)

hist(div$Adult_Hills)
m<-lmer(logGENSPratio_Adult~METHOD + (1|SEC_NAME),data=div)
DPlots(m,div)
leveneTest(GENSPratio_Adult~METHOD, data = div)

##Shannon
full.form <- formula(Adult_Shannon ~ METHOD*MAX_DEPTH_M, data=div)

#fit lme using "ML"= maximum likelihood - so can use step fxn
full.lme <-lme(full.form, data=div, random=~1|SEC_NAME, method="ML",na.action = na.omit) #use if needed

#drop 3-way interaction term & depth x habitat (not interested in these hypotheses)
FULL.MOD <- update(full.lme, .~. -METHOD:MAX_DEPTH_M) 
anova(full.lme, FULL.MOD) #from output, drop the 2-way interaction term with the larger p-value first

RED.MOD1 <- update(FULL.MOD, .~. -METHOD) #drop 2-way interaction term
anova(FULL.MOD, RED.MOD1) #LRT --> move forward w/ whichever model keeps/removes term

anova(full.lme)


##hills
full.form <- formula(Adult_Hills ~ METHOD*MAX_DEPTH_M, data=div)

#fit lme using "ML"= maximum likelihood - so can use step fxn
full.lme <-nlme::lme(full.form, data=div, random=~1|SEC_NAME, method="ML",na.action = na.omit) #use if needed

#drop 3-way interaction term & depth x habitat (not interested in these hypotheses)
FULL.MOD <- update(full.lme, .~. -METHOD:MAX_DEPTH_M) 
anova(full.lme, FULL.MOD) #from output, drop the 2-way interaction term with the larger p-value first

RED.MOD1 <- update(FULL.MOD, .~. -METHOD) #drop 2-way interaction term
anova(FULL.MOD, RED.MOD1) #LRT --> move forward w/ whichever model keeps/removes term

anova(full.lme)

##hills
full.form <- formula(Adult_Richness ~ METHOD*MAX_DEPTH_M, data=div)

#fit lme using "ML"= maximum likelihood - so can use step fxn
full.lme <-nlme::lme(full.form, data=div, random=~1|SEC_NAME, method="ML",na.action = na.omit) #use if needed

#drop 3-way interaction term & depth x habitat (not interested in these hypotheses)
FULL.MOD <- update(full.lme, .~. -METHOD:MAX_DEPTH_M) 
anova(full.lme, FULL.MOD) #from output, drop the 2-way interaction term with the larger p-value first

RED.MOD1 <- update(FULL.MOD, .~. -METHOD) #drop 2-way interaction term
anova(FULL.MOD, RED.MOD1) #LRT --> move forward w/ whichever model keeps/removes term

anova(full.lme)

##hills
full.form <- formula(Adult_Shannon ~ METHOD*MAX_DEPTH_M, data=div)

#fit lme using "ML"= maximum likelihood - so can use step fxn
full.lme <-nlme::lme(full.form, data=div, random=~1|SEC_NAME, method="ML",na.action = na.omit) #use if needed

#drop 3-way interaction term & depth x habitat (not interested in these hypotheses)
FULL.MOD <- update(full.lme, .~. -METHOD:MAX_DEPTH_M) 
anova(full.lme, FULL.MOD) #from output, drop the 2-way interaction term with the larger p-value first

RED.MOD1 <- update(FULL.MOD, .~. -METHOD) #drop 2-way interaction term
anova(FULL.MOD, RED.MOD1) #LRT --> move forward w/ whichever model keeps/removes term

anova(full.lme)

##richness
full.form <- formula(Adult_Richness ~ METHOD*MAX_DEPTH_M, data=div[div$count == "5",])

#fit lme using "ML"= maximum likelihood - so can use step fxn
full.lme <-lme(full.form, data=div[div$count == "5",], random=~1|SEC_NAME, method="ML",na.action = na.omit) #use if needed

#drop 3-way interaction term & depth x habitat (not interested in these hypotheses)
FULL.MOD <- update(full.lme, .~. -METHOD:MAX_DEPTH_M) 
anova(full.lme, FULL.MOD) #from output, drop the 2-way interaction term with the larger p-value first

RED.MOD1 <- update(FULL.MOD, .~. -METHOD) #drop 2-way interaction term
anova(FULL.MOD, RED.MOD1) #LRT --> move forward w/ whichever model keeps/removes term

anova(full.lme)



mod1<-lm(Adult_Shannon~METHOD, data = div);summary(mod1)
mod1<-lm(Adult_Richness~METHOD, data = div);summary(mod1)
mod1<-lm(Adult_Hills~METHOD, data = div);summary(mod1)
mod1<-lm(logGENSPratio_Adult~METHOD, data = div);summary(mod1)


Plot1to1_new<-function(d,response_variable,predictor_variable,r_name,p_name,x,y){
  #sub<-d[d$taxon,]
  d$Y<-d[,response_variable]
  d$X<-d[,predictor_variable]
  mx_val<-max(d$Y, d$X, na.rm = TRUE)
  
  lm_fit <- lm(Y ~ X, data=d)
  predicted_df <- data.frame(Y_pred = predict(lm_fit, d), X=d$X)
  
  corr<-cor.test(d$X, d$Y, method="pearson")
  rmse<-rmse(d$Y, d$X,na.rm=TRUE)
  r_text<-paste("RMSE = ", round(rmse,digits = 2),"\n r = ", round((corr$estimate),2), sep="")
  
  p1<-ggplot(d, aes(x=X, y=Y)) + 
    geom_point(size=1) + 	
    geom_abline(slope=1, intercept=0) +
    facet_wrap(~count)+
    #geom_jitter(width=.25,height=0,color="black",alpha=0.5)+
    #geom_smooth(method="lm", color="red", linetype="dashed", se=F) +
    geom_line(linetype = "dashed", color='red',data = predicted_df, aes(x=X, y=Y_pred))+
    geom_label(label=r_text,x=x,y=y, nudge_y=-0.1, nudge_x=1,label.size=0.35, color="black", fill="white") +
    theme_bw()+
    theme(panel.grid.major = element_blank()
          ,panel.grid.minor = element_blank())+
    scale_x_continuous(limits=c(0,mx_val)) +
    #scale_y_continuous(limits=c(0,mx_val)) +
    ylab(r_name) +  xlab(p_name)     
  return(p1)
} # 


p1 <- Plot1to1_new(div.wide,"SfM_Adult_Shannon","Diver_Adult_Shannon","SfM Adult Shannon Diversity","Diver Adult Shannon Diversity",0.75,2)+
  geom_text(x = 0, y = 2.7, label = "A", color = "black")
p2 <- Plot1to1_new(div.wide,"SfM_Adult_Hills","Diver_Adult_Hills","SfM Adult Diversity (Hills)","Diver Adult Diversity (Hills)",3.5,12)+
  geom_text(x = 0, y = 14.75, label = "C", color = "black")
 p3<- Plot1to1_new(div.wide,"SfM_Adult_Richness","Diver_Adult_Richness","SfM Adult Genus Richness","Diver Adult Genus Richness",6,20)+
  geom_text(x = 0, y = 28, label = "E", color = "black")

grid.arrange(p1,p2,p3,nrow=1,ncol=3)


doms <- awd.all[awd.all$GENUS_CODE!="AAAA",] %>% 
  dplyr::filter(ANALYST %in% c("CA", "JC", "MSL")) %>% 
  dplyr::group_by(GENUS_CODE, METHOD) %>% 
  dplyr::summarise(counts = n()) %>% 
  dplyr::arrange(desc(counts))

obs.sfm <- awd.all[awd.all$GENUS_CODE!="AAAA",] %>% 
  dplyr::filter(ANALYST %in% c("CA", "JC", "MSL")) %>% 
  dplyr::group_by(METHOD) %>% 
  dplyr::summarise(totals = n())
doms <- left_join(doms, obs.sfm)
doms$prop <- doms$counts*100/doms$totals

doms.order <- awd.all[awd.all$GENUS_CODE!="AAAA",] %>% 
  dplyr::group_by(GENUS_CODE) %>% 
  dplyr::summarise(counts = as.numeric(n())) %>% 
  dplyr::arrange(desc(counts))

doms.order$rank <- c(1:nrow(doms.order))


doms <- left_join(doms, doms.order[,-2])
doms <- arrange(doms, rank)

overall.rankabun <- ggplot()+
  geom_line(data = doms, aes(x = rank, y = prop, color = METHOD))+
  geom_text(data = doms.order, aes(x = rank, label = GENUS_CODE, y = 0), size = 3)+
  theme_classic()

ggsave(overall.rankabun, file= "C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Output/PLOTS/overall.rankabun.png", height = 10, width = 12)

obs <- awd.all[awd.all$GENUS_CODE!="AAAA",] %>% 
  dplyr::filter(ANALYST %in% c("CA", "JC", "MSL")) %>% 
  dplyr::group_by(GENUS_CODE, ANALYST) %>% 
  dplyr::summarise(counts = n()) %>% 
  dplyr::arrange(desc(counts))

obs.an <- awd.all[awd.all$GENUS_CODE!="AAAA",] %>% 
  dplyr::filter(ANALYST %in% c("CA", "JC", "MSL")) %>% 
  dplyr::group_by(ANALYST) %>% 
  dplyr::summarise(totals = n())
obs <- left_join(obs, obs.an)
obs$prop <- obs$counts*100/obs$totals



obs <- left_join(obs, doms.order[,-2])
obs <- arrange(obs, rank)

analyst.rankabun <- ggplot()+
  geom_line(data = obs, aes(x = rank, y = prop, color = ANALYST))+
  geom_text(data = doms.order, aes(x = rank, label = GENUS_CODE, y = 0), size = 3)+
  theme_classic()

ggsave(analyst.rankabun, file= "C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Output/PLOTS/analyst.rankabun.png", height = 10, width = 12)

doms.obs <- awd.all[awd.all$GENUS_CODE!="AAAA",] %>% 
  dplyr::group_by(GENUS_CODE, METHOD, ANALYST) %>% 
  dplyr::summarise(counts = n()) %>% 
  dplyr::arrange(desc(counts))
doms.obs <- left_join(doms.obs, doms.order[,-2])
doms.obs <- arrange(doms.obs, rank)
obs <- awd.all[awd.all$GENUS_CODE!="AAAA",] %>% 
  dplyr::group_by(METHOD, ANALYST) %>% 
  dplyr::summarise(totals = n())
doms.obs <- left_join(doms.obs, obs)
doms.obs$prop <- doms.obs$counts*100/doms.obs$totals

obs.rankabun <- 
  ggplot()+
  geom_line(data = doms.obs[doms.obs$ANALYST %in% c("CA", "JC", "MSL"),], aes(x = rank, y = prop, color = METHOD))+
  geom_line(data = doms, aes(x = rank, y = prop, color = METHOD), lwd = 3, alpha = .1)+
  facet_wrap("ANALYST", ncol = 2)+
  #geom_text(data = doms.order, aes(x = rank, label = GENUS_CODE, y = (prop/2)), size = 3)+
  theme_classic()

ggsave(obs.rankabun, file= "C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Output/PLOTS/observer.rankabun.png", height = 10, width = 12)


site<-read.csv("C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Output/MARAMP22_GENUS_SITE_Juvs.csv")
site_as <- read.csv("C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Output/ASRAMP23_GENUS_SITE_Juvs.csv")
site <- bind_rows(site, site_as)
site <- subset(site, select = -c(HABITAT_CODE, NOTES))
#site <- na.omit(site)
site$METHOD[site$METHOD == "Diver"] <- "DIVER"


method <- site %>%
  filter(GENUS_CODE != "SSSS") %>% 
  group_by(METHOD, GENUS_CODE) %>% 
  summarise(Mean.Density = mean(AdColDen), SE.Density = sd(AdColDen)/sqrt(length(unique(site$SITE))), Mean.Size = mean(Ave.size, na.rm = T), SE.Size = sd(Ave.size, na.rm = T)/sqrt(length(unique(site$SITE))), count = sum(AdColCount))

region <- site %>%
  filter(GENUS_CODE != "SSSS") %>% 
  group_by(METHOD, GENUS_CODE, REGION) %>% 
  summarise(Mean.Density = mean(AdColDen), Var.Density = sd(AdColDen), Mean.Size = mean(Ave.size, na.rm = T), Var.Size = sd(Ave.size, na.rm = T), count = sum(AdColCount))

lump <- ggplot(method[method$GENUS_CODE != "ASTS" & method$Mean.Density > 0.5,], aes(y = Mean.Size, x = Mean.Density, fill = METHOD, label = GENUS_CODE))+
  theme_classic()+
  geom_line(aes(group = GENUS_CODE), size = 1, color = "gray75")+
  geom_errorbar(aes(ymin = Mean.Size - SE.Size, ymax = Mean.Size + SE.Size), size = 1)+
  geom_errorbarh(aes(xmin = Mean.Density - SE.Density, xmax = Mean.Density + SE.Density), size = 1)+
  geom_label()+
  geom_text(inherit.aes = F, x = 0.5, y = 25, label = "Lump", size = 10)+
  geom_text(inherit.aes = F, x = 0.55, y = 24, label = "Fewer, larger colonies")+
  geom_text(inherit.aes = F, x = 1.7, y = 10, label = "Split", size = 10)+
  geom_text(inherit.aes = F, x = 1.75, y = 9, label = "More, smaller colonies")+
  guides(size = "none")+labs(x = "Mean Colony Density", y = "Mean Colony Size", fill = "Method")

ggsave(lump, height = 8, width = 10, file="C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Output/PLOTS/Lump_Split.png")
  


ggplot(region[region$GENUS_CODE %in% doms.order$GENUS_CODE[doms.order$rank<16],], aes(y = Mean.Size, x = Mean.Density, fill = METHOD, label = GENUS_CODE, size = count))+
  theme_classic()+
  geom_line(aes(group = GENUS_CODE), size = 1)+
  geom_label()#+
  #facet_wrap(~REGION)


fish <- read.csv("C:/Users/Jonathan.Charendoff/Downloads/Cruise Report Table_ Fish REA Site Visits.csv")[,1:7]
SfM <- read.csv("C:/Users/Jonathan.Charendoff/Downloads/SFM process tracking.csv")[,8]
fish$SfM <- ifelse(fish$Site %in% SfM, -1, 0)

write.csv(fish[fish$SfM == -1,],"C:/Users/Jonathan.Charendoff/Downloads/extra_sfm.csv")



j_sfm<-read.csv("T:/Benthic/Data/SfM/Analysis Ready/ASRAMP23_SfM_Juv_CLEANED.csv")
j_sfm2<-read.csv("T:/Benthic/Data/SfM/Analysis Ready/MARAMP22_SfM_Juv_CLEANED.csv")
j_sfm <- bind_rows(j_sfm, j_sfm2)


j_sfm$SITE_SEG<-paste(j_sfm$SITE,j_sfm$SEGMENT,sep ="_")
length(unique(j_sfm$SITE_SEG));length(unique(j_sfm$SITE)) #should be 389 ss and 104 sites

j_sfm <- subset(j_sfm, TRANSECT == "A")

# PREP VISUAL DIVER DATA ---------------------------------------------
## LOAD benthic data
jwd_<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Analysis Ready Raw data/CoralBelt_Juveniles_raw_CLEANED.csv")

jwd<-subset(jwd_,OBS_YEAR=="2023"|OBS_YEAR=="2022")


#Changing Analyst names to match SfM
jwd$DIVER<-ifelse(jwd$DIVER=="J_E","JE",as.character(jwd$DIVER))
jwd$DIVER<-ifelse(jwd$DIVER=="J_C","JC",as.character(jwd$DIVER))



#Simplify Bleaching Severity categories: in 2019 the team decided to simplify the bleaching severity from 1-5 to 1-3 to improve consistency in severity values
#This code converts the severity data collected prior to 2019 to a 1-3 scale
jwd$DATE_ <- as.Date(jwd$DATE_, format = "%Y-%m-%d")

colnames(jwd)[which(colnames(jwd) == 'DIVER')] <- "ANALYST"
jwd$EX_BOUND<-0;jwd$EX_BOUND<-as.numeric(jwd$EX_BOUND) #add ex_bound columns to match SfM



#Calculate segment area
jwd$SEGAREA<-jwd$SEGLENGTH*jwd$SEGWIDTH

#Only include sites and segments surveyed by divers during HARAMP 2019
jwd$SITE_SEG<-paste(jwd$SITE,jwd$SEGMENT,sep="_")

#remove columns that aren't needed & 
jwd<-dplyr::select(jwd,-c(TRANSECTAREA,bANALYSIS_SCHEME,ANALYSIS_YEAR,REGION_NAME,NO_SURVEY_YN,DATE_,ISLANDCODE))
jwd<-dplyr::filter(jwd, SITE_SEG %in% c(j_sfm$SITE_SEG));head(jwd) 


##
length(unique(jwd$SITE))


j_sfm_sub<-dplyr::filter(j_sfm, SITE_SEG %in% c(jwd$SITE_SEG));nrow(j_sfm);nrow(j_sfm_sub)

jwd_sub<-dplyr::filter(jwd, SITE_SEG %in% c(j_sfm_sub$SITE_SEG))

length(unique(j_sfm_sub$SITE));length(unique(j_sfm_sub$SITE_SEG))

# Add Method column to diver data
jwd$METHOD<-"Diver";jwd$METHOD<-as.factor(jwd$METHOD)

j_sfm_sub<-dplyr::select(j_sfm_sub,-c(FRAGMENT,HABITAT_CODE))
jwd<-dplyr::select(jwd,-c(MISSIONID))

#Merge diver and SfM data
sort(colnames(jwd))
sort(colnames(j_sfm_sub))

sapply(jwd,class)
sapply(j_sfm,class)

jwd.all<-rbind(j_sfm_sub,jwd)
jwd.all$SEGWIDTH[is.na(jwd.all$SEGWIDTH)] <- 1
jwd.all$SEGAREA<-jwd.all$SEGLENGTH*jwd.all$SEGWIDTH
jwd.all$TRANSECT <- recode_factor(jwd.all$TRANSECT, "1" = "A")
jwd.all <- subset(jwd.all, jwd.all$TRANSECT == "A")
jwd.all$MISSIONID <- "COMPARISON"

jwd.all$TRANSECTAREA<-TransectareaMETHOD(jwd.all)


SURVEY_COL<-c("METHOD","SITEVISITID", "OBS_YEAR", "REGION", "ISLAND","SEC_NAME", "SITE", "REEF_ZONE",
              "DEPTH_BIN", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M","TRANSECT","SEGMENT","COLONYID","GENUS_CODE","TAXONCODE","SPCODE","COLONYLENGTH")
survey_colony<-unique(jwd.all[,SURVEY_COL])

SURVEY_SITE<-c("METHOD","SITEVISITID", "OBS_YEAR", "REGION", "ISLAND","SEC_NAME", "SITE", "REEF_ZONE",
               "DEPTH_BIN", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M")
survey_site<-unique(jwd.all[,SURVEY_SITE])
visits <- survey_site$SITEVISITID[survey_site$SEC_NAME == ""]
for (i in 1:length(visits)) {
  survey_site$SEC_NAME[survey_site$SITEVISITID == visits[i] & survey_site$METHOD == "SfM"] <- survey_site$SEC_NAME[survey_site$SITEVISITID == visits[i] & survey_site$METHOD == "Diver"]
}


SURVEY<-c("METHOD","SITEVISITID", "OBS_YEAR", "REGION", "ISLAND","SEC_NAME", "SITE", "REEF_ZONE",
          "DEPTH_BIN", "LATITUDE", "LONGITUDE","MIN_DEPTH_M","MAX_DEPTH_M","METHOD","TRANSECT","SEGMENT")
survey_segment<-unique(jwd.all[,SURVEY])

nrow(survey_site)
sectors<-read.csv("C:/Users/Jonathan.Charendoff/Documents/GitHub/Benthic-Scripts/SfM/Marianas 2022/Sectors-Strata-Areas.csv", stringsAsFactors=FALSE)
meta<-left_join(survey_site,sectors)
meta[which(is.na(meta$AREA_HA)),]
nrow(survey_site)
nrow(meta)
jwd.all$Fragment <- 0

ggplot()+
  geom_histogram(data = na.omit(jwd.all[jwd.all$COLONYLENGTH < 5 & jwd.all$COLONYLENGTH > 0.5,]), aes(x = COLONYLENGTH), bins = 10)+
  facet_wrap(~METHOD)+
  theme_classic()

jcd.gen<-Calc_ColDen_Transect(jwd.all[jwd.all$COLONYLENGTH < 5 & jwd.all$COLONYLENGTH > 0.5,],"GENUS_CODE"); colnames(jcd.gen)[colnames(jcd.gen)=="ColCount"]<-"JuvColCount";colnames(jcd.gen)[colnames(jcd.gen)=="ColDen"]<-"JuvColDen";colnames(jcd.gen)[colnames(jcd.gen)=="TRANSECTAREA"]<-"TRANSECTAREA_j"
jcd.gen.475<-Calc_ColDen_Transect(jwd.all[jwd.all$COLONYLENGTH < 4.75 & jwd.all$COLONYLENGTH > 0.5,],"GENUS_CODE"); colnames(jcd.gen)[colnames(jcd.gen)=="ColCount"]<-"JuvColCount";colnames(jcd.gen)[colnames(jcd.gen)=="ColDen"]<-"JuvColDen";colnames(jcd.gen)[colnames(jcd.gen)=="TRANSECTAREA"]<-"TRANSECTAREA_j"
jcd.gen.450<-Calc_ColDen_Transect(jwd.all[jwd.all$COLONYLENGTH < 4.5 & jwd.all$COLONYLENGTH > 0.5,],"GENUS_CODE"); colnames(jcd.gen)[colnames(jcd.gen)=="ColCount"]<-"JuvColCount";colnames(jcd.gen)[colnames(jcd.gen)=="ColDen"]<-"JuvColDen";colnames(jcd.gen)[colnames(jcd.gen)=="TRANSECTAREA"]<-"TRANSECTAREA_j"

jcd.gen %>% 
  group_by(METHOD) %>% 
  summarise(total = sum(JuvColCount))
jcd.gen.475 %>% 
  group_by(METHOD) %>% 
  summarise(total = sum(ColCount))
jcd.gen.450 %>% 
  group_by(METHOD) %>% 
  summarise(total = sum(ColCount))

#TEMPORARY WORK AROUND-ASK MICHAEL TO FIX
#survey_site$REEF_ZONE<-ifelse(survey_site$SITE=="HAW-04285","Forereef",as.character(survey_site$REEF_ZONE))
#survey_segment$REEF_ZONE<-ifelse(survey_segment$SITE=="HAW-04285","Forereef",as.character(survey_segment$REEF_ZONE))

