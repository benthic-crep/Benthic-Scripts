sp<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_SPCODE.csv")

pal<-subset(sp,ISLAND=="Palmyra" & SPCODE=="PVAU")

pal$OBS_YEAR<-as.factor(pal$OBS_YEAR)
p<-ggplot(data=pal,aes(x=JuvColDen,y=AdColDen,color=OBS_YEAR)) + 
  geom_point() + 
  geom_smooth(method="lm")+
  theme_bw() + 
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.title.x = element_text( vjust = -.0001))

load("T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/ALL_REA_JUVCORAL_RAW_2013-2020.rdata") #from oracle
j<-df
p_j<-subset(j,ISLAND=="Palmyra"& TAXONCODE=="PVAU")

p_j$OBS_YEAR<-as.factor(p_j$OBS_YEAR)
ggplot(p_j) + 
  geom_density(aes(x = COLONYLENGTH, fill = OBS_YEAR), alpha = 0.2)+
  ggtitle("Juvenile PVAU Size Frequency")

load("T:/Benthic/Data/REA Coral Demography & Cover/Raw from Oracle/ALL_REA_ADULTCORAL_RAW_2013-2020.rdata") #from oracle
a<-df
p_ad<-subset(a,ISLAND=="Palmyra"& TAXONCODE=="PVAU")

p_ad$OBS_YEAR<-as.factor(p_ad$OBS_YEAR)
ggplot(p_j) + 
  geom_density(aes(x = COLONYLENGTH, fill = OBS_YEAR), alpha = 0.2)+
  ggtitle("Adult PVAU Size Frequency")


all_col<-rbind(p_ad[,c("SITE","OBS_YEAR","COLONYLENGTH")],p_j[,c("SITE","OBS_YEAR","COLONYLENGTH")])

all_col$OBS_YEAR<-as.factor(all_col$OBS_YEAR)
ggplot(all_col) + 
  geom_density(aes(x = COLONYLENGTH, fill = OBS_YEAR), alpha = 0.2)+
  geom_vline(xintercept=5, color = "black")+
  ggtitle("PVAU Size Frequency")


#POSP
p_j<-subset(j,ISLAND=="Palmyra"& TAXONCODE=="POSP")
p_ad<-subset(a,ISLAND=="Palmyra"& TAXONCODE=="POSP")

all_col<-rbind(p_ad[,c("SITE","OBS_YEAR","COLONYLENGTH")],p_j[,c("SITE","OBS_YEAR","COLONYLENGTH")])

all_col$OBS_YEAR<-as.factor(all_col$OBS_YEAR)
ggplot(all_col) + 
  geom_density(aes(x = COLONYLENGTH, fill = OBS_YEAR), alpha = 0.2)+
  geom_vline(xintercept=5, color = "black")+
  ggtitle("POSP (as SPCODE) Size Frequency")

p_j<-subset(j,ISLAND=="Palmyra"& GENUS_CODE=="POSP")
p_ad<-subset(a,ISLAND=="Palmyra"& GENUS_CODE=="POSP")

all_col<-rbind(p_ad[,c("SITE","OBS_YEAR","COLONYLENGTH")],p_j[,c("SITE","OBS_YEAR","COLONYLENGTH")])

all_col$OBS_YEAR<-as.factor(all_col$OBS_YEAR)
ggplot(all_col) + 
  geom_density(aes(x = COLONYLENGTH, fill = OBS_YEAR), alpha = 0.2)+
  geom_vline(xintercept=5, color = "black")+
  ggtitle("POSP (at GENUS) Size Frequency")

#PLOB
p_j<-subset(j,ISLAND=="Palmyra"& TAXONCODE=="PLOB")
p_ad<-subset(a,ISLAND=="Palmyra"& TAXONCODE=="PLOB")

all_col<-rbind(p_ad[,c("SITE","OBS_YEAR","COLONYLENGTH")],p_j[,c("SITE","OBS_YEAR","COLONYLENGTH")])

all_col$OBS_YEAR<-as.factor(all_col$OBS_YEAR)
ggplot(all_col) + 
  geom_density(aes(x = COLONYLENGTH, fill = OBS_YEAR), alpha = 0.2)+
  geom_vline(xintercept=5, color = "black")+
  ggtitle("PLOB Size Frequency")

ggplot(all_col) + 
  geom_histogram(aes(x = COLONYLENGTH, fill = OBS_YEAR))+
  geom_vline(xintercept=5, color = "black")+
  ggtitle("PLOB Size Frequency")

#PLUT
p_j<-subset(j,ISLAND=="Palmyra"& TAXONCODE=="PLUT")
p_ad<-subset(a,ISLAND=="Palmyra"& TAXONCODE=="PLUT")

all_col<-rbind(p_ad[,c("SITE","OBS_YEAR","COLONYLENGTH")],p_j[,c("SITE","OBS_YEAR","COLONYLENGTH")])

all_col$OBS_YEAR<-as.factor(all_col$OBS_YEAR)
ggplot(all_col) + 
  geom_density(aes(x = COLONYLENGTH, fill = OBS_YEAR), alpha = 0.2)+
  geom_vline(xintercept=5, color = "black")+
  ggtitle("PLUT Size Frequency")

ggplot(all_col) + 
  geom_histogram(aes(x = COLONYLENGTH, fill = OBS_YEAR))+
  geom_vline(xintercept=5, color = "black")+
  ggtitle("PLUT Size Frequency")
