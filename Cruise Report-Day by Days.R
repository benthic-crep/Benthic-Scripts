library(tidyverse)
library(lubridate)
CA=read.csv("C:/Users/Thomas.Oliver/Downloads/Cruise Activities_02_SE2406.csv")
head(CA)
CA$ISLAND=substr(CA$Site,1,3)
CA$Date=dmy(CA$Date)
CA$SITEIDNUM=as.numeric(substr(CA$Site,5,99))
#Fish.Rea.Survey.Yn Photoquad.Yn Sfm.Yn Ds.Suite.Activity.Yn Cb.Activity.Yn Ctd.Activity.Yn H2o.Activity.Yn Str.Dep.Activity.Yn Str.Rec.Activity.Yn
#Caus.Deploy.Yn Caus.Retrieve.Yn Bmus.Deploy.Yn Bmus.Retrieve.Yn
SumYes=function(x){return(length(which(x=="YES")))}
DayByDay=CA %>% group_by(Date) %>% summarize(
  ISLAND=paste(unique(ISLAND),collapse="-"),
  FishSPC=SumYes(Fish.Rea.Survey.Yn),
  PQ.Random=length(which(Type!="Oceanography"&Photoquad.Yn=="YES")),
  PQ.Fixed=length(which(Type=="Oceanography"&Photoquad.Yn=="YES")),
  SfM.Random=length(which(Type!="Oceanography"&Sfm.Yn=="YES")),
  SfM.Fixed=length(which(Type=="Oceanography"&Sfm.Yn=="YES")),
  H2O.Random=length(which(Type!="Oceanography"&H2o.Activity.Yn=="YES")),
  H2O.Fixed=length(which(Type=="Oceanography"&H2o.Activity.Yn=="YES"&SITEIDNUM<9000)),
  H2O.Off=length(which(Type=="Oceanography"&H2o.Activity.Yn=="YES"&SITEIDNUM>9000)),
  STR.Dep=SumYes(Str.Dep.Activity.Yn),
  STR.Rec=SumYes(Str.Rec.Activity.Yn),
  CAU.Dep=SumYes(Caus.Deploy.Yn),
  CAU.Rec=SumYes(Caus.Retrieve.Yn),
  BMU.Dep=SumYes(Bmus.Deploy.Yn),
  BMU.Rec=SumYes(Bmus.Retrieve.Yn),
  Carb.Budget=SumYes(Cb.Activity.Yn),
  DielSuite=SumYes(Ds.Suite.Activity.Yn)
) %>% arrange(Date)

write.csv(DayByDay,"C:/Users/Thomas.Oliver/Downloads/Cruise Activities_DayByDay_SE2406.csv")
