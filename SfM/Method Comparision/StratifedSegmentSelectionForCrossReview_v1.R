library(dplyr)
library(pwr)

# What follows is a script to assign a specific subset of segments from the NCRMP Benthic dataset to be cross reviewed by other annotators
#(A) Assign adult segments
# (1) Load the dataset, and report basic stats
# (2) Set parameters, most importantly, the proportion of segments to review and the identity of the review team
# (3) Build a segment-level dataset for ANALYST-stratified subsampling, select
# (4) Assign each segment to a different annotator (equally)
# (5) Build a sub-sampled geo-databse file for each annotator's cross-review


# (1) Load Geodatabase, Report  -------------------------------------------
workdir="C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/"
#make output directory if necessary
if(!dir.exists(paste0(workdir,"GeoDatabase_Review_Assignments"))){dir.create(paste0(workdir,"GeoDatabase_Review_Assignments"))}

GeoD_A=read.csv(paste0(workdir,"HARAMP2019_QCdsfm_ADULT.csv"))
GeoD_J=read.csv(paste0(workdir,"HARAMP2019_QCdsfm_JUV.csv"))

#Make changes to juvenile data
GeoD_J<-subset(GeoD_J,select=-c(FRAGMENT.1))
GeoD_J$site_seg<-paste(GeoD_J$SITE,GeoD_J$SEGMENT,sep=" ")
head(GeoD_J)

GeoD<-dplyr::bind_rows(GeoD_A, GeoD_J) #Combine dataframes
head(GeoD)


### First Assign Adults

print("##############################")
print("### Adult Colonies First   ###")
print("##############################")
print(paste(dim(GeoD_A)[1],"Adult Colonies in GeoDatabase"))
print(paste(length(unique(GeoD_A$site_seg)),"Segments in Adult GeoDatabase"))
print(paste(length(unique(GeoD_A$SITE)),"Sites in Adult GeoDatabase"))

# (2) Set Parameters ----------------------------------------------------------
ReviewTeam=c("MA","MSL","RS","CA","CSC")
PowerTarget=0.025
MaxLoops=10000
targetcvtt=0.075

#Get Adult Col/Seg 
ColPSeg_A=table(GeoD_A$site_seg)
md_ColPSeg_A=10^median(log10(ColPSeg_A))

#N colonies needed to have 80% power to tell if the error rate is above 1% (if as little as ",PowerTarget*100,"%)
#Power Test
BPT=pwr.p.test(h=ES.h(PowerTarget,.01),
           sig.level = 0.05,
           power = 0.80,
           alternative = "greater")
print(paste("Need to review",ceiling(BPT$n),"colonies, or",ceiling(BPT$n/md_ColPSeg_A),"for sufficent statistical power to tell ",PowerTarget*100,"% error from baseline of 1%."))

#ProportionToReview=0.10
NtoSample_A=ceiling(BPT$n/md_ColPSeg_A)#ceiling(length(unique(GeoD_A$site_seg))*ProportionToReview)

AnnotationTeam=unique(GeoD_A$ANALYST)
Nanalysts=length(AnnotationTeam)
NperAnalyst_A=ceiling(NtoSample_A/Nanalysts)

#Report Out
print(paste("Preparing to sample",NperAnalyst_A*Nanalysts,"segments total,",NperAnalyst_A,"from each analyst."))

# (3) Build a segment-level dataset for ANALYST-stratified subsa --------
Xrev=unique(GeoD_A[,c("ANALYST","site_seg")])

#Stratified Random Selection of Segments Across Analysts
SegToReview = Xrev %>%
  group_by(ANALYST) %>%
  sample_n(NperAnalyst_A)

#Quick Evenness Check
print(paste("Quick check to make sure we've sampled evenly across analysts:"))
print(table(SegToReview$ANALYST))


# (4) Assign each segment to a different annotator (with equal probability) --------------

#Ensure reasonable fairness in final colony count (not segment count) by imbeding this code in a while loop
NLoops=0
cvtt=1
print(paste("Starting sampling loop, with iterations to ensure assignments that are 'fair enough' across annotators:"))
GeoD4Rev_A_keep=NULL
AssignedSeg_A_keep=NULL
while(cvtt>targetcvtt&NLoops<MaxLoops){
  AssignedSeg_A=NULL
  #For each original annotator's segments, randomly assign them to another annotator
  for(i_ass in AnnotationTeam){
    others=setdiff(ReviewTeam,i_ass)
    thisset=subset(SegToReview,ANALYST==i_ass)
    thisset$REVIEW_ANALYST=sample(others,nrow(thisset),replace=T)
    AssignedSeg_A=rbind(AssignedSeg_A,thisset)
  }
  
  #subsample GeoD_A to only flagged segments
  GeoD4Rev_A=subset(GeoD_A,site_seg%in%AssignedSeg_A$site_seg)
  
  #Build lookup table and assign REVIEW_ANALYST to geodatabase entries
  RA_LU=AssignedSeg_A$REVIEW_ANALYST
  names(RA_LU)=AssignedSeg_A$site_seg
  GeoD4Rev_A$REVIEW_ANALYST=RA_LU[as.vector(GeoD4Rev_A$site_seg)]
  
  #Check Fairness by Colony Count
  table(GeoD4Rev_A$REVIEW_ANALYST)
  tt=table(GeoD4Rev_A$REVIEW_ANALYST)
  mtt=mean(tt)
  sdtt=sd(tt)
  cvtt_new=sdtt/mtt
  if(cvtt_new<cvtt){
    AssignedSeg_A_keep=AssignedSeg_A
    GeoD4Rev_A_keep=GeoD4Rev_A
    cvtt=cvtt_new
  }
  NLoops=NLoops+1
}
GeoD4Rev_A=GeoD4Rev_A_keep
AssignedSeg_A=AssignedSeg_A_keep
print(" ")
print("########################")
print(" ")

if(NLoops>=MaxLoops){
  print("Function exited without hitting the target fairness metric. You should re-run or lower the target.")
}else{
  print(paste("After",NLoops,"iterations, the function exited with a fair assignment."))
}

print(paste("The tables below should show:"))
print(paste("(1) that no annotator has been assigned her/his own segment"))
print(table(AssignedSeg_A[,c("ANALYST","REVIEW_ANALYST")]))
print(paste("(2) that an equal number of segments per annotator are being reviewed"))
print(table(AssignedSeg_A[,c("ANALYST")]))
print(paste("(3) a roughly equal number of segments are flagged for review per annotator (but randomness in small samples means not exactly)"))
print(table(AssignedSeg_A[,c("REVIEW_ANALYST")]))
print(paste("(4) a roughly equal number of colonies are flagged for review per annotator (original and review)"))
print(table(GeoD4Rev_A[,c("ANALYST")]))
print(table(GeoD4Rev_A[,c("REVIEW_ANALYST")]))
print(paste("Coeff. of Variation between annotator colony counts:",round(cvtt,4)))


# (5) Assess power for juveniles using sector assignments

print("###############################")
print("### Juvenile Colonies Now   ###")
print("###############################")
NCol_J=dim(GeoD_J)[1]
NSeg_J=length(unique(GeoD_J$site_seg))
print(paste(NCol_J,"Juvenile Colonies in GeoDatabase"))
print(paste(NSeg_J,"Segments in Juvenile GeoDatabase"))
print(paste(length(unique(GeoD_J$SITE)),"Sites in Juvenile GeoDatabase"))
#subsample GeoD_J to only flagged segments
AssignedSeg_J=AssignedSeg_A
GeoD4Rev_J=subset(GeoD_J,site_seg%in%AssignedSeg_J$site_seg)
GeoD_Jout=subset(GeoD_J,!site_seg%in%AssignedSeg_J$site_seg)
NCol_J=nrow(GeoD4Rev_J)
print("###############################")
NCol_J_Ass=dim(GeoD4Rev_J)[1]
NSeg_J_Ass=length(unique(GeoD4Rev_J$site_seg))
print(paste(NCol_J_Ass,"Juvenile Colonies already in selected segments"))
print(paste(NSeg_J_Ass,"Segments already in selected segments"))
print(paste(length(unique(GeoD4Rev_J$SITE)),"Sites already in selected segments"))

BPT_J=pwr.p.test(h=ES.h(PowerTarget,.01),
                 sig.level = 0.05,
                 n = NCol_J_Ass,
                 alternative = "greater")

#N colonies needed to have 80% power to tell if the error rate is above 1% (if as little as ",PowerTarget*100,"%)
#Power Test
print(paste("Juveniles in assigned segments provide",floor(100*BPT_J$power),"% power to tell ",PowerTarget*100,"% error from baseline of 1%. (Target is 80%)"))

#Will now randomly add one segment per original analyst until our Power reaches 80%
NSeg_Left=length(unique(GeoD_Jout$site_seg))
MaxSegTot=ceiling(NSeg_J*.3)#set max review to 30% of total; 
MaxSegAdd=MaxSegTot-NSeg_J_Ass
MaxRounds=ceiling(MaxSegAdd/Nanalysts)
print(paste("Now adding one Juvenile segment per original annotator, until power is sufficent or 30% of all Juvenile segments are assigned."))
NRounds=0
while(NRounds<=MaxRounds&BPT_J$power<.8){
  #Stratified Random Selection of Segments Across Analysts
  Xrev_Jout=unique(GeoD_Jout[,c("ANALYST","site_seg")])

  SegToAdd_J = Xrev_Jout %>%
    group_by(ANALYST) %>%
    sample_n(1)
  SegToAdd_J$REVIEW_ANALYST=NA
  AssignedSeg_J=rbind(AssignedSeg_J,SegToAdd_J)
  GeoD4Rev_J=subset(GeoD_J,site_seg%in%AssignedSeg_J$site_seg)
  GeoD_Jout=subset(GeoD_J,!site_seg%in%AssignedSeg_J$site_seg)
  NCol_J_Ass_New=dim(GeoD4Rev_J)[1]
  Added_NCol=NCol_J_Ass_New-NCol_J_Ass
  NCol_J_Ass=NCol_J_Ass_New
  NRounds=NRounds+1
  BPT_J=pwr.p.test(h=ES.h(PowerTarget,.01),
                   sig.level = 0.05,
                   n = NCol_J_Ass,
                   alternative = "greater")
  print(paste("Added",Added_NCol,"colonies in the",NRounds,"-th round, for new power of",floor(100*BPT_J$power),"%"))
}
if(NRounds>=MaxRounds){print(paste("Exited addition reaching MaxRound limit. Power is",floor(100*BPT_J$power),"%"))
  }else{print(paste("Exited with power=",floor(100*BPT_J$power),"%"))}

GeoD4Rev_J$REVIEW_ANALYST=RA_LU[as.vector(GeoD4Rev_J$site_seg)]

#Ensure reasonable fairness in additional juvenile colony count (not segment count) by imbeding this code in a while loop
NLoops=0
cvtt=1
print(paste("Starting Juvenile sampling loop, with iterations to ensure additional juv. assignments that are 'fair enough' across annotators:"))
GeoD4Rev_J_keep=NULL
AssignedSeg_J_keep=NULL
while(cvtt>targetcvtt&NLoops<MaxLoops){
  #For each original annotator's segments, randomly assign them to another annotator
  for(i_ass in AnnotationTeam){
    others=setdiff(ReviewTeam,i_ass)
    thisann_i=which(AssignedSeg_J$ANALYST==i_ass&is.na(AssignedSeg_J$REVIEW_ANALYST))
    AssignedSeg_J$REVIEW_ANALYST[thisann_i]=sample(others,length(thisann_i),replace=T)
  }
  
  #subsample GeoD_J to only flagged segments
  GeoD4Rev_J=subset(GeoD_J,site_seg%in%AssignedSeg_J$site_seg)
  
  #Build lookup table and assign REVIEW_ANALYST to geodatabase entries
  RA_LU_J=AssignedSeg_J$REVIEW_ANALYST
  names(RA_LU_J)=AssignedSeg_J$site_seg
  GeoD4Rev_J$REVIEW_ANALYST=RA_LU_J[as.vector(GeoD4Rev_J$site_seg)]
  
  #Check Fairness by Colony Count
  tt=table(c(GeoD4Rev_A$REVIEW_ANALYST,GeoD4Rev_J$REVIEW_ANALYST))
  mtt=mean(tt)
  sdtt=sd(tt)
  cvtt_new=sdtt/mtt
  if(cvtt_new<cvtt){
    AssignedSeg_J_keep=AssignedSeg_J
    GeoD4Rev_J_keep=GeoD4Rev_J
    cvtt=cvtt_new
  }
  NLoops=NLoops+1
}
GeoD4Rev_J=GeoD4Rev_J_keep
AssignedSeg_J=AssignedSeg_J_keep
print(" ")
print("########################")
print(" ")
if(NLoops>=MaxLoops){
  print("Function exited without hitting the target fairness metric. You should re-run or lower the target.")
}else{
  print(paste("After",NLoops,"iterations, the function exited with a fair assignment."))
}

#Make Joint J and A out DF
GeoD4Rev=rbind(GeoD4Rev_A,GeoD4Rev_J)
GeoD4Rev[order(GeoD4Rev$site_seg),]


print(paste("The tables below should show:"))
print(paste("(1) that no annotator has been assigned her/his own segment"))
print(table(AssignedSeg_J[,c("ANALYST","REVIEW_ANALYST")]))

print(paste("(2) that an equal number of segments per annotator are being reviewed"))
print(table(AssignedSeg_J[,c("ANALYST")]))

print(paste("(3) a roughly equal number of segments are flagged for review per annotator (but randomness in small samples means not exactly)"))
print(table(AssignedSeg_J[,c("REVIEW_ANALYST")]))

print(paste("(4) a roughly equal number of colonies are flagged for review per annotator across A and J(original and review)"))
print(table(GeoD4Rev[,c("ANALYST")]))
print(table(GeoD4Rev[,c("REVIEW_ANALYST")]))
print(paste("Coeff. of Variation between annotator colony counts:",round(cvtt,4)))

print("")
print(paste("Current assignments amount to",100*round(length(unique(GeoD4Rev_A$site_seg))/length(unique(GeoD$site_seg)),3),"% of Adult Segments, and",100*round(length(unique(GeoD4Rev_J$site_seg))/length(unique(GeoD$site_seg)),3),"% of Juvenile Segments."))
print(paste("Current assignments amount to",100*round(nrow(GeoD4Rev_A)/nrow(GeoD),3),"% of Adult Colonies, and",100*round(nrow(GeoD4Rev_J)/nrow(GeoD),3),"% of Juvenile Colonies"))

#Add columns for QC review before writing file
d.cols<-c("Lump_Split_Error","SPCODE_CORRECT","MORPH_CORRECT","RDCAUSE_CORRECT","CONDITION_CORRECT","SEVERITY_CORRECT","SIZE_CORRECT","DELETE_COLONY")
GeoD4Rev[d.cols] <- NA


print("Writing out Assignment Files...")
#Write out review .csv for each REVIEW_ANALYST
for(i_rev in ReviewTeam){
  Assignment_AJ=subset(GeoD4Rev,REVIEW_ANALYST==i_rev)
  write.csv(x = Assignment_AJ,file = paste0(workdir,"GeoDatabase_Review_Assignments/Benthic_SfM_ReviewAssignments_",i_rev,"AdultAndJuveniles.csv"),row.names = F)
}

# Exploration -------------------------------------------------------------


#Explore Colonies Per Segment
#Colonies Per Segment Distribution (~25)
ColPSeg_A=table(GeoD_A$site_seg)
L10_ColPSeg=log10(ColPSeg_A)
L10_ColPSeg[is.infinite(L10_ColPSeg)]=NA

md_L10_ColPSeg=median(L10_ColPSeg,na.rm=T)
sd_L10_ColPSeg=sd(L10_ColPSeg,na.rm=T)
se_L10_ColPSeg=sd(L10_ColPSeg,na.rm=T)/sqrt(length(L10_ColPSeg))
CI95_L10_ColPSeg=1.96*sd(L10_ColPSeg,na.rm=T)/sqrt(length(L10_ColPSeg))


md=10^md_L10_ColPSeg
sdB=10^(md_L10_ColPSeg+c(-1,1)*sd_L10_ColPSeg)
seB=10^(md_L10_ColPSeg+c(-1,1)*se_L10_ColPSeg)
CI95B=10^(md_L10_ColPSeg+c(-1,1)*CI95_L10_ColPSeg)

par(mfrow=c(2,1))
hist((ColPSeg_A),seq(0,175,1)-.5,main=paste(md,"Adult Colonies per Segment \n (median, [",round(CI95B,4)[1],",",round(CI95B,4)[2],"ci95])"))
abline(v=md,col="red",lty=1)
abline(v=CI95B,col="green",lty=3)

ColPSeg_J=table(GeoD_J$site_seg)
L10_ColPSeg=log10(ColPSeg_J)
L10_ColPSeg[is.infinite(L10_ColPSeg)]=NA

md_L10_ColPSeg=median(L10_ColPSeg,na.rm=T)
sd_L10_ColPSeg=sd(L10_ColPSeg,na.rm=T)
se_L10_ColPSeg=sd(L10_ColPSeg,na.rm=T)/sqrt(length(L10_ColPSeg))
CI95_L10_ColPSeg=1.96*sd(L10_ColPSeg,na.rm=T)/sqrt(length(L10_ColPSeg))


md=10^md_L10_ColPSeg
sdB=10^(md_L10_ColPSeg+c(-1,1)*sd_L10_ColPSeg)
seB=10^(md_L10_ColPSeg+c(-1,1)*se_L10_ColPSeg)
CI95B=10^(md_L10_ColPSeg+c(-1,1)*CI95_L10_ColPSeg)

hist((ColPSeg_J),seq(0,175,1)-.5,main=paste(md,"Juvenile Colonies per Segment \n (median, [",round(CI95B,4)[1],",",round(CI95B,4)[2],"ci95])"))
abline(v=md,col="red",lty=1)
abline(v=CI95B,col="green",lty=3)
