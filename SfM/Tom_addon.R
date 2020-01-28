rm(list=ls())

#source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/SfMvDiver Plotting Functions.R") 
source("C:/Users/Corinne.Amir/Documents/GitHub/Benthic-Scripts/Functions/SfMvDiver Plotting Functions.R") 
#Plot1to1; PlotBioAlt; PlotPair

#data.gen<-read.csv("T:/Benthic/Data/SfM/Summarized Data/HARAMP_repeats_GENUS_Summarized Data.csv")
data.gen<-read.csv("T:/Benthic/Data/SfM/Summarized Data/HARAMP_repeats_GENUS_Summarized Data-CALIBRATION.csv")

#data.gen.rm<-subset(data.gen, TRANSECT %in% c(3, 4, 5, 6, "NA"));data.gen[data.gen$TRANSECT] #for calibration?
library(plyr)
data.gen$SST=paste0(data.gen$SS,"_",data.gen$GENUS_CODE)
seg4list=ddply(data.gen,.(SST,SS),summarize,NBox=length(unique(MethodRep)))
all4seglist=subset(seg4list,NBox>=4)
length(unique(all4seglist[,"SS"]))

data.sm=subset(data.gen,SST%in%all4seglist$SST&GENUS_CODE=="SSSS")
dim(data.sm)
length(unique(data.sm$SS))

dscol=c("AdColDen","JuvColDen","Ave.cl","Ave.od", "Ave.rd","DZGN_G_prev","BLE_prev","CHRO_prev")
#dscol=c("AdColDen","JuvColDen","Ave.cl","Ave.od","BLE_prev")
library(reshape2)
mm=melt(data.sm,measure.vars = dscol)
mm=subset(mm,select=-c(DZGN_G_den,BLE_den,CHRO_den,JuvColCount,AdColCount))
head(mm)

mm.add=ddply(mm,.(MethodRep,variable),summarise,
             value_mn=mean(value,na.rm=T),
             value_se=sd(value,na.rm=T)/sqrt(length(value)),
             value_cv=value_se/value_mn)

library(ggplot2)
ggplot(mm,aes(x=MethodRep,y=value))+geom_violin()+
  facet_wrap("variable",scales ="free_y") +
  geom_jitter(height=0,width=.1, col="black")+
  geom_errorbar(data=mm.add, aes(y=value_mn,ymin=value_mn-value_cv,ymax=value_mn-value_cv,width=.75), col="gray")+
  geom_errorbar(data=mm.add, aes(y=value_mn,ymin=value_mn+value_cv,ymax=value_mn+value_cv,width=.75), col="gray")+
  geom_errorbar(data=mm.add, aes(y=value_mn,ymin=value_mn,ymax=value_mn,width=.75), col="red")+
  scale_y_sqrt()+theme_bw()
  


#Add another dataframe for the above figure

#mean value
a <- aggregate(data.sm$AdColDen, list(data.sm$MethodRep), mean) 
a$Group <- rep("AdColDen",times=4)
b <- aggregate(data.sm$JuvColDen, list(data.sm$MethodRep), mean)
b$Group <- rep("JuvColDen",times=4)
c <- aggregate(data.sm$Ave.cl, list(data.sm$MethodRep), mean)
c$Group <- rep("Ave.cl",times=4)
d <- aggregate(data.sm$Ave.od, list(data.sm$MethodRep), mean)
d$Group <- rep("Ave.od",times=4)
# g <- aggregate(data.sm$Ave.rd, list(data.sm$MethodRep), mean)
# g$Group <- rep("Ave.rd",times=4)
# f <- aggregate(data.sm$DZGN_G_prev, list(data.sm$MethodRep), mean)
# f$Group <- rep("DZGN_G_prev",times=4)
e <- aggregate(data.sm$BLE_prev, list(data.sm$MethodRep), mean)
e$Group <- rep("BLE_prev",times=4)
# h <- aggregate(data.sm$CHRO_prev, list(data.sm$MethodRep), mean)
# h$Group <- rep("CHRO_prev",times=4)
mm.mean <- rbind(a,b,c,d,e)
colnames(mm.mean) <- c("MethodRep", "Mean", "Group")

#SE
StandErr <- function(data,variable,group) {
  as.numeric(variable)
  se<-sd(variable)/sqrt(nrow(data))
  a<-aggregate(variable, list(group), se)
  colnames(a) <- c("MethodRep", "SE")
  return(a)
}
a<- StandErr(data.sm,data.sm$AdColDen,data.sm$MethodRep)
a$Group <- rep("AdColDen",times=4)
b<- StandErr(data.sm,data.sm$JuvColDen,data.sm$MethodRep)
b$Group <- rep("JuvColDen",times=4)
c<- StandErr(data.sm,data.sm$Ave.cl,data.sm$MethodRep)
c$Group <- rep("Ave.cl",times=4)
d<- StandErr(data.sm,data.sm$Ave.od,data.sm$MethodRep)
d$Group <- rep("Ave.od",times=4)
e<- StandErr(data.sm,data.sm$BLE_prev,data.sm$MethodRep)
e$Group <- rep("BLE_prev",times=4)
mm.se <- rbind(a,b,c,d,e)

#Coefficient of Variance
CVariation <- function(variable,group){
  as.numeric(variable)
  cv<-sd(variable)/mean(variable)
  a<-aggregate(variable, list(group), se)
  colnames(a) <- c("MethodRep", "CV")
  return(a)
}
a<- CVariation(data.sm$AdColDen, data.sm$MethodRep)
a$Group <- rep("AdColDen",times=4)
b<-CVariation(data.sm$AdColDen, data.sm$MethodRep)
b$Group <- rep("JuvColDen",times=4)
c<-CVariation(data.sm$Ave.cl, data.sm$MethodRep)
c$Group <- rep("Ave.cl",times=4)
d<-CVariation(data.sm$Ave.od, data.sm$MethodRep)
d$Group <- rep("Ave.od",times=4)
e<-CVariation(data.sm$BLE_prev, data.sm$MethodRep)
e$Group <- rep("BLE_prev",times=4)
mm.cv <- rbind(a,b,c,d,e)

mm.add <- join_all(list(mm.mean,mm.se,mm.cv), by=c("MethodRep","Group"))
colnames(mm.add) <- c("MethodRep", "value", "variable", "SE", "CV")



#ADD THIS TO THE DATAPREP RSCRIPT
data.sm$Ave.od[is.na(data.sm$Ave.od)] <- 0
data.sm$Ave.rd[is.na(data.sm$Ave.rd)] <- 0
data.sm$Ave.cl[is.na(data.sm$Ave.cl)] <- 0
data.sm$DZGN_G_prev[is.na(data.sm$DZGN_G_prev)] <- 0
data.sm$BLE_prev[is.na(data.sm$BLE_prev)] <- 0
data.sm$CHRO_prev[is.na(data.sm$CHRO_prev)] <- 0