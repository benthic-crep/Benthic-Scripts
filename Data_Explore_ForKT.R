library(lme4)
library(lmerTest)
library(tidyverse)
library(emmeans)
library(performance)
library(patchwork)
library(multcomp)
library(multcompView)
r2.mar=function(mod){r2(mod)[2]}
r2.con=function(mod){r2(mod)[1]}

cb=read.csv("C:/Users/Thomas.Oliver/Downloads/summarized_data (2).csv")
cb$ED50_CUT=cut(cb$ED50,breaks = 5)
cb$Paling_CUT=cut(cb$slope,breaks = 5)
############################################################
Ychoice="ED50"
PURGEHH=FALSE
purgelimit=3
eval(parse(text=paste0("cb$YY=cb$",Ychoice)))
orgcb=cb

kcol=names(cb)[grep(pattern = "K_",x = names(cb))]
modlist=vector(mode = "list", length = length(kcol))
names(modlist)=kcol
emobjlist=vector(mode = "list", length = length(kcol))
emlist=vector(mode = "list", length = length(kcol))
conlist=vector(mode = "list", length = length(kcol))
for(ki in 1:length(kcol)){
  cb=orgcb
  cb$HH=as.factor(paste0(cb$Species,"_",cb[,kcol[ki]]))
  if(PURGEHH){
    tHH=table(cb$HH)
    purgelist=names(tHH[which(tHH<purgelimit)])
    if(length(purgelist)>0){cb=cb %>% filter(!HH%in%purgelist)}
    cb$HH=as.factor(paste0(cb$Species,"_",cb[,kcol[ki]]))
  }
  cb$KK=as.factor(cb[,kcol[ki]])
  cb=cb %>% filter(!is.na(KK))#,Species!="ICRA")
  KKlevels=cb %>% group_by(KK) %>%
    summarize(N=length(YY),mnYY=median(YY)) %>%
    arrange(desc(mnYY)) %>% na.omit()
  cb$KK=factor(cb$KK,levels=KKlevels$KK)
  
  mm=lmer(YY~KK+(1|Site)+(1|Species),data=cb)
  modlist[[ki]]=mm
  kk_posthoc <- emmeans(mm, pairwise ~ KK, adjust = "tukey")
  emobjlist[[ki]]=kk_posthoc
  emlist[[ki]]=as.data.frame(kk_posthoc$emmeans)
  conlist[[ki]]=as.data.frame(kk_posthoc$contrasts)
  print(ki)
}
moddf=data.frame(K=kcol,
                 AIC=unlist(lapply(modlist,AIC)),
                 BIC=unlist(lapply(modlist,BIC)),
                 r2.mar=unlist(lapply(modlist,r2.mar)),
                 r2.con=unlist(lapply(modlist,r2.con)))
EFplot=ggplot(moddf,aes(AIC,r2.mar))+
  geom_point()+
  geom_quantile(quantiles = 0.75)+
  geom_label(aes(label=K))+
  theme_bw()+ggtitle(Ychoice)
EFplot

#############################
ki=which(kcol=="K_3")
##############################
cb$KK=as.factor(cb[,kcol[ki]])
cb=cb %>% filter(!is.na(KK))#,Species!="ICRA")
KKlevels=cb %>% group_by(KK) %>%
  summarize(N=length(YY),mnYY=median(YY)) %>%
  arrange(desc(mnYY)) %>% na.omit()
KKlevels$KK=factor(KKlevels$KK,levels=KKlevels$KK)
cb$KK=factor(cb$KK,levels=KKlevels$KK)
Kplot=ggplot(cb,aes(x=KK,y=YY))+
  #geom_boxplot()+
  geom_text(aes(x=KK,label=N),y=quantile(cb$YY,0),data=KKlevels)+
  geom_point(aes(x=KK,y=emmean),data=emlist[[ki]])+
  geom_errorbar(aes(x=KK,y=emmean,ymin=emmean-SE,ymax=emmean+SE),data=emlist[[ki]])+
  geom_jitter(aes(shape=factor(Site),color=Species))+
  scale_x_discrete()+
  scale_shape_manual(values = 1:8)+
  theme_bw()+ggtitle(Ychoice)
EFplot/Kplot

cb=orgcb
cb$HH=as.factor(paste0(cb$Species,"_",cb[,kcol[ki]]))
if(PURGEHH){
  tHH=table(cb$HH)
  purgelist=names(tHH[which(tHH<purgelimit)])
  if(length(purgelist)>0){cb=cb %>% filter(!HH%in%purgelist)}
  cb$HH=as.factor(paste0(cb$Species,"_",cb[,kcol[ki]]))
}
HHlevels=cb %>% group_by(HH) %>%
  summarize(N=length(YY),mnYY=median(YY)) %>%
  arrange(desc(mnYY)) %>% na.omit()
HHlevels$HH=factor(HHlevels$HH,levels=HHlevels$HH)
cb$HH=factor(cb$HH,levels=HHlevels$HH)
emdf=emlist[[ki]]
# Example: emmeans object from a model
model=modlist[[ki]]
em <- emmeans(model, ~ HH)
pairs_result=pairs(em)
pvals <- summary(pairs_result)$p.value
names(pvals) <- gsub(" - ", "-", summary(pairs_result)$contrast)
letters_out <- multcompLetters(pvals)$Letters
emdf$group <- letters_out[match(emdf$HH, names(letters_out))]

Hplot=ggplot(cb,aes(x=HH,y=YY))+
  #geom_boxplot()+
  geom_text(aes(x=HH,label=N),y=quantile(cb$YY,0),data=HHlevels)+
  geom_text(aes(x=HH,label=group),y=quantile(cb$YY,1),data=emdf)+
  geom_point(aes(x=HH,y=emmean),data=emdf)+
  geom_errorbar(aes(x=HH,y=emmean,ymin=emmean-SE,ymax=emmean+SE),data=emdf)+
  geom_jitter(aes(shape=factor(Site),color=Species))+
  scale_x_discrete()+
  scale_shape_manual(values = 1:8)+
  theme_bw()+ggtitle(paste0(Ychoice,": ",kcol[ki]))

EFplot/Hplot

conlist[[ki]] %>%
  arrange((p.value)) %>%
  filter(p.value<0.05)

######################################

ggplot(cb,aes(x=ED50_CUT,y=ED50,color=Species,shape=factor(Site)))+
  geom_jitter()+
  scale_shape_manual(values = 1:8)+
  theme_bw()
ggplot(cb,aes(x=Paling_CUT,y=slope,color=Species,shape=factor(Site)))+
  geom_jitter()+
  scale_shape_manual(values = 1:8)+
  theme_bw()

PalingSpecies_Turnover=cb %>% group_by(Species,Paling_CUT) %>% summarize(N=length(slope)) %>% 
  ggplot(aes(x=Paling_CUT,y=N,fill=Species))+
  geom_col(position="fill")
ED50Species_Turnover=cb %>% group_by(Species,ED50_CUT) %>% summarize(N=length(slope)) %>% 
  ggplot(aes(x=ED50_CUT,y=N,fill=Species))+
  geom_col(position="fill")