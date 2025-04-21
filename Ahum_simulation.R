library(tidyverse)
library(vegan)
library(ggrepel)

ahumG=read.csv("C:/Users/Thomas.Oliver/WORK/Projects/zMisc. Projects/ESA/AhumulisGroup_Characters_WallaceMTQ2012.csv")
DataNames=names(ahumG)[2:15]
ContNames=names(ahumG)[2:7]
FactorNames=names(ahumG)[8:15]#["Tapering":"Coen_Between_Radial_Shape"]
ahumG$Axial_Wall_Thickness=ahumG$Axial_Outer_Diameter-ahumG$Axial_Inner_Diameter
#drop
ContNames=ContNames[!ContNames%in%c("Axial_Primary_Septa","Radial_Primary_Septa_R","Axial_Inner_Diameter")]
ContNames=c(ContNames,"Axial_Wall_Thickness")
FactorNames=FactorNames[!FactorNames%in%c("Radial_Synaptical_Rings","Axial_Synaptical_Rings","Coen_Between_Radial_Shape")]

ahumG=ahumG[,c("Taxon",ContNames,FactorNames)]
ahumG$Tapering=factor(ahumG$Tapering,levels=c("Reverse Tapering","Terete","Tapering","Strongly Tapering"))
ahumG$Radial_Crowding=factor(ahumG$Radial_Crowding,levels=c("Not Touching","Some Touching","Most Touching"))
ahumG$Radial_Size=factor(ahumG$Radial_Size,levels=c("Medium","Large"))
ahumG$Radial_Size_Classes=factor(ahumG$Radial_Size_Classes,levels=c("One or Graded","Two Sizes"))
ahumG$Radial_Openings=factor(ahumG$Radial_Openings,levels=c("Oval-Rounded","Dimidate"))
#ahumG$Coen_Between_Radial_Shape=factor(ahumG$Coen_Between_Radial_Shape,levels=c("Reticulate","Reticulo-Costate"))
#ahumG$Axial_Synaptical_Rings=factor(ahumG$Axial_Synaptical_Rings,levels=c("only3","gt3"))
#ahumG$Radial_Synaptical_Rings=factor(ahumG$Radial_Synaptical_Rings,levels=c("only3","gt3"))

Taxa=substr(ahumG$Taxon,1,4)

dummyAH=ahumG
for(i in 1:length(FactorNames)){
  dummyAH[,FactorNames[i]]=as.numeric(ahumG[,FactorNames[i]])
}

N=16
RepTax=NULL
for(i in 1:length(Taxa)){
  thistaxa=dummyAH[which(substr(ahumG$Taxon,1,4)==Taxa[i]),c(ContNames,FactorNames)]
  repN=thistaxa[rep(1:2,N/2),]
  for(j in 1:ncol(repN)){
    if(diff(range(repN[,j])))
    repN[,j]=sample(seq(min(repN[,j]),max(repN[,j]),length.out=100),N)
  }
  row.names(repN)=paste0(rep(Taxa[i],nrow(repN)),1:N)
  RepTax=rbind(RepTax,repN)
}






mod=cca(RepTax)
smod=summary(mod)
sp=as.data.frame(smod$species)
sp$param=row.names(sp)
st=as.data.frame(smod$sites)
st$sample=row.names(st)
st$taxon=substr(st$sample,1,4)
sc=10

Crit=c(ContNames,FactorNames)
Centroid=st %>% group_by(taxon) %>% summarize(across(CA1:CA6,mean))
ad=function(x){return(abs(diff(x)))}


Centroid %>%
  filter(taxon%in%c("AGLO","AHUM")) %>%
  summarize(across(CA1:CA6,ad)) %>%
  pivot_longer(CA1:CA6) %>% 
  filter(value==max(value)) %>% 
  pull(name)

Centroid %>%
  filter(taxon%in%c("AGLO","ASAM")) %>%
  summarize(across(CA1:CA6,ad)) %>%
  pivot_longer(CA1:CA6) %>% 
  filter(value==max(value)) %>% 
  pull(name)


ggplot()+
  geom_point(aes(CA1,CA4,fill=taxon),size=5,alpha=.75,shape=21,color="black",data=st)+
  geom_segment(aes(x = 0,y=0,xend=sc*CA1,yend=sc*CA4),arrow=grid::arrow(),data=sp)+
  geom_text_repel(aes(x=sc*CA1,y=sc*CA4,label=param),data=sp)+
  theme_bw()

#mod <- cca(dune ~ A1 + Moisture + Management, dune.env)
## better control -- remember to set scaling etc identically
plot(mod, type="n", scaling="sites")
text(mod, dis="cn", scaling="sites")
points(mod, pch=21, col="red", bg="yellow", cex=1.2, scaling="sites")

plot(AHUMGcca,type="points",cex=.1)
text(mod, "species", col="blue", cex=0.8, scaling="sites")









mm=mm %>% arrange(Weighed.Sums.Percentile) %>% na.omit()
head(mm)

Metrics=mm[,names(mm)[grep(pattern = ".Percentile",x = names(mm))][1:4]]
mPCA=prcomp(Metrics)
mPCAw=as.data.frame(cbind(mPCA$x,WSP=mm$Weighed.Sums.Percentile))
mPCA_loads=as.data.frame(apply(mPCA$rotation,MARGIN = 2,as.numeric))
mPCA_loads$Name=as.character(row.names(mPCA$rotation))
mPCA_loads$PlotName=c("Abund","GeoB","GeoBVar","Bleach")#,"LBSP","Fishing","OA","MPA","Gov")
library(ggrepel)
ggplot()+
  geom_point(aes(x = PC1,y = PC2),size=3,data=mPCAw)+
  geom_point(aes(x = PC1,y = PC2,size=100*(WSP),color=(((100*WSP)-95)/5)),data=mPCAw)+
  geom_segment(aes(xend = PC1,yend = PC2),x=0,y=0,arrow=grid::arrow(),data=mPCA_loads)+
  geom_label_repel(aes(x = PC1,y = PC2,label=PlotName),data=mPCA_loads)+
  scale_size(limits=c(95,100),range=c(.1,7))+
  scale_color_gradient(limits=c(0,5))
  
  

site.data.gen2<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Site/BenthicREA_sitedata_TAXONCODE_2023.csv")
sdg=site.data.gen2 %>% filter(AdColCount  >0)
table(sdg$SITE,sdg$TAXONCODE)
sdg %>% group_by(SEC_NAME,TAXONCODE) %>% summarize(N=sum(AdColCount))
