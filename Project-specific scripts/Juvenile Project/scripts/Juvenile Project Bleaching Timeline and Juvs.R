
hs.all<-read.csv("T:/Benthic/Projects/Juvenile Project/Juvenile_HSTimeline.csv")

#Change Regions to correspond to juvenile data
Convert_Region<-function(data){
  data$REGION<-ifelse(data$ISLAND %in% c("Farallon de Pajaros", "Maug", "Asuncion", "Alamagan", "Pagan", "Agrihan", "Guguan", "Sarigan","Farallon_de_Pajaros")
                      ,"NMI", as.character(data$REGION))
  data$REGION<-ifelse(data$ISLAND %in% c("Saipan", "Tinian", "Aguijan", "Rota", "Guam")
                      ,"SMI", as.character(data$REGION))
  data$REGION<-ifelse(data$ISLAND %in% c("Howland","Baker")
                      ,"PHOENIX", as.character(data$REGION))
  data$REGION<-ifelse(data$ISLAND =="Wake"
                      ,"WAKE", as.character(data$REGION))
  data$REGION<-ifelse(data$ISLAND %in% c("Kingman","Palmyra","Jarvis")
                      ,"LINE", as.character(data$REGION))
  return(data$REGION)
}

hs.all$REGION<-Convert_Region(hs.all)


hs.all <- mutate_if(hs.all, 
                      is.character, 
                      str_replace_all, pattern = " ", replacement = "_")


df<-read.csv("T:/Benthic/Projects/Juvenile Project/JuvDen_Pred_SITE_AllYears.csv")#Combined juvenile delta density and all predictors
df<-filter(df,ISLAND !="Guguan") #only 1 year

class(df$DATE_)
df$DATE_<-ymd(df$DATE_)
juvR.date <- df %>%
  group_by(REGION,OBS_YEAR) %>%
  summarise(DATE_=median(DATE_))

juvR.date$ANALYSIS_YEAR<-juvR.date$OBS_YEAR

# Temporal Trends in Juveniles --------------------------------------------
data.temporal <-read.csv("T:/Benthic/Projects/Juvenile Project/JuvDen_Temporal.csv")
#Use survey package to calculate mean SE and conduct statistical analyses
data.temporal$ANALYSIS_YEAR<-as.factor(data.temporal$ANALYSIS_YEAR)
des<-svydesign(id=~1, strata=~ANALYSIS_YEAR+REGION+ISLAND+SEC_NAME+DB_RZ, weights=~sw,data=data.temporal)

#Calculate regional mean and SE
temp_Rmean<-svyby(~JuvColDen,~ANALYSIS_YEAR+REGION,des,svymean)

#Test fixed effects of region and year
modR<-svyglm(JuvColCount ~ REGION*ANALYSIS_YEAR, design=des,offset= TRANSECTAREA_j, family="poisson")

svystdres(modR,stvar="DB_RZ",doplot=TRUE)
anova(modR)

#Run separate post hoc tests for each region to test for differences between years- I don't care about comparing all possible combinations of year and region
library(multcomp)

mhi.des<-svydesign(id=~1, strata=~ANALYSIS_YEAR+ISLAND+SEC_NAME+DB_RZ, weights=~sw,data=subset(data.temporal,REGION=="MHI"))
mhi<-svyglm(JuvColCount ~ ANALYSIS_YEAR, design=mhi.des,offset= TRANSECTAREA_j, family="poisson")
summary(glht(mhi, mcp(ANALYSIS_YEAR="Tukey"))) 

wake.des<-svydesign(id=~1, strata=~ANALYSIS_YEAR+ISLAND+SEC_NAME+DB_RZ, weights=~sw,data=subset(data.temporal,REGION=="WAKE"))
wake<-svyglm(JuvColCount ~ ANALYSIS_YEAR, design=wake.des,offset= TRANSECTAREA_j, family="poisson")
summary(glht(wake, mcp(ANALYSIS_YEAR="Tukey"))) 

ph.des<-svydesign(id=~1, strata=~ANALYSIS_YEAR+ISLAND+SEC_NAME+DB_RZ, weights=~sw,data=subset(data.temporal,REGION=="PHOENIX"))
ph<-svyglm(JuvColCount ~ ANALYSIS_YEAR, design=ph.des,offset= TRANSECTAREA_j, family="poisson")
summary(glht(ph, mcp(ANALYSIS_YEAR="Tukey"))) 

line.des<-svydesign(id=~1, strata=~ANALYSIS_YEAR+ISLAND+SEC_NAME+DB_RZ, weights=~sw,data=subset(data.temporal,REGION=="LINE"))
l<-svyglm(JuvColCount ~ ANALYSIS_YEAR, design=line.des,offset= TRANSECTAREA_j, family="poisson")
summary(glht(l, mcp(ANALYSIS_YEAR="Tukey"))) 

sm.des<-svydesign(id=~1, strata=~ANALYSIS_YEAR+ISLAND+SEC_NAME+DB_RZ, weights=~sw,data=subset(data.temporal,REGION=="SMI"))
sm<-svyglm(JuvColCount ~ ANALYSIS_YEAR, design=sm.des,offset= TRANSECTAREA_j, family="poisson")
summary(glht(sm, mcp(ANALYSIS_YEAR="Tukey"))) 

nm.des<-svydesign(id=~1, strata=~ANALYSIS_YEAR+ISLAND+SEC_NAME+DB_RZ, weights=~sw,data=subset(data.temporal,REGION=="NMI"))
nm<-svyglm(JuvColCount ~ ANALYSIS_YEAR, design=nm.des,offset= TRANSECTAREA_j, family="poisson")
summary(glht(nm, mcp(ANALYSIS_YEAR="Tukey")))

sam.des<-svydesign(id=~1, strata=~ANALYSIS_YEAR+ISLAND+SEC_NAME+DB_RZ, weights=~sw,data=subset(data.temporal,REGION=="SAMOA"))
sam<-svyglm(JuvColCount ~ ANALYSIS_YEAR, design=sam.des,offset= TRANSECTAREA_j, family="poisson")
summary(glht(sam, mcp(ANALYSIS_YEAR="Tukey"))) 


#Calculate adjusted pvalues for multiple test corrections
pvals<-c(0.54424,0.03636,0.00345,0.348,0.299,0.000001,0.263,0.000295,0.391)
round(p.adjust(pvals, "BH"), 3) #0.568 0.082 0.010 0.447 0.447 0.000 0.447 0.001 0.568


# PLOTTING ----------------------------------------------------------------

#Merge regional mean and median dates
temp_Rmean$ANALYSIS_YEAR<-as.factor(temp_Rmean$ANALYSIS_YEAR)
juvR.date$ANALYSIS_YEAR<-as.factor(juvR.date$ANALYSIS_YEAR)

temp_Rmean<-left_join(temp_Rmean,juvR.date)
head(temp_Rmean)

#bar plot of juv by region by year with post hoc tests 
temp_Rmean$REGION <- factor(temp_Rmean$REGION, levels = c("MHI","WAKE","PHOENIX","LINE","SMI","NMI","SAMOA"))
temp_Rmean$ANALYSIS_YEAR<-as.factor(temp_Rmean$ANALYSIS_YEAR)
#Add Posthoc groupings from glms
temp_Rmean<- temp_Rmean[order(temp_Rmean$REGION),];temp_Rmean
temp_Rmean$sig<-c("ab","b","a","","","","","a","b","","","a","b","","")


p8 <- ggplot(temp_Rmean, aes(x=ANALYSIS_YEAR, y=JuvColDen,color=REGION)) +
  geom_point()+
  #geom_bar(stat = "identity", position = position_dodge2(preserve='single'), width = 1, color="black") +
  geom_errorbar(aes(y=JuvColDen, x=ANALYSIS_YEAR,ymin=JuvColDen-se, ymax=JuvColDen+se), width=.2)+
  facet_grid(~REGION, scales = "free_x", space = "free") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(0, "lines"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 12),
        legend.position = "none",
        axis.line = element_line(color = "black"),
        text = element_text(size = 12),
        axis.text.y = element_text(colour="black"),
        axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values = c("#D55E00","#E69F00","#F0E442","#009E73","#56B4E9","#0072B2","#999999")) +
  xlab("Year") +
  ylab("Mean Juvenile Colonies/m^2") +
  scale_y_continuous(expand = c(0,0), limits = c(0,16)) +
  geom_text(aes(x=ANALYSIS_YEAR,y=JuvColDen+se,label=sig, group = REGION),
            position = position_dodge(),
            vjust = -0.5) 
p8

timeseries %>%
  ggplot() +
  facet_grid(facet ~ ., scales = "free") +
  geom_point(data = subset(fake_data_long, facet == "var2"), 
             aes(x = time, y = y),
             size = 1) +
  geom_line(data = subset(fake_data_long, facet == "var2"), 
            aes(x = time, y = y)) +
  geom_boxplot(data = subset(fake_data_long, facet == "var1"), 
               aes(x = time, y = y, group = time))