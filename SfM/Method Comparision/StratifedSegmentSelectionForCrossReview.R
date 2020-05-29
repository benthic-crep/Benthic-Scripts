library(dplyr)

# What follows is a script to assign a specific subset of segments from the NCRMP Benthic dataset to be cross reviewed by other annotators
# (1) Load the dataset, and report basic stats
# (2) Set parameters, most importantly, the proportion of segments to review and the identity of the review team
# (3) Build a segment-level dataset for ANALYST-stratified subsampling, select
# (4) Assign each segment to a different annotator (equally)
# (5) Build a sub-sampled geo-databse file for each annotator's cross-review


# (1) Load Geodatabase, Report  -------------------------------------------
workdir="C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/"

GeoD=read.csv(paste0(workdir,"HARAMP2019_output_052220.csv"))
head(GeoD)
print(paste(dim(GeoD)[1],"Colonies in GeoDatabase"))
print(paste(length(unique(GeoD$site_seg)),"Segments in GeoDatabase"))
print(paste(length(unique(GeoD$SITE)),"Sites in GeoDatabase"))

# (2) Set Parameters ----------------------------------------------------------
ProportionToReview=0.10
ReviewTeam=c("MA","MSL","RS","CA")

NtoSample=ceiling(length(unique(GeoD$site_seg))*ProportionToReview)
NtoSample

AnnotationTeam=unique(GeoD$ANALYST)
Nanalysts=length(AnnotationTeam)
NperAnalyst=ceiling(NtoSample/Nanalysts)
print(paste("Preparing to sample",NperAnalyst*Nanalysts,"segments total,",NperAnalyst,"from each analyst."))

# (3) Build a segment-level dataset for ANALYST-stratified subsa --------
Xrev=unique(GeoD[,c("ANALYST","site_seg")])

#Stratified Randome Selection of Segments Across Analysts
set.seed(1)
SegToReview = Xrev %>%
  group_by(ANALYST) %>%
  sample_n(NperAnalyst)

#Quick Evenness Check
print(paste("Quick check to make sure we've sampled evenly across analysts:"))
table(SegToReview$ANALYST)


# (4) Assign each segment to a different annotator (with equal probability) --------------

#Ensure reasonable fairness in final colony count (not segment count) by imbeding this code in a while loop
MaxLoops=1000
NLoops=0
cvtt=1
targetcvtt=0.05
while(cvtt>targetcvtt&NLoops<MaxLoops){
  AssignedSeg=NULL
  #For each original annotator's segments, randomly assign them to another annotator
  for(i_ass in AnnotationTeam){
    others=setdiff(ReviewTeam,i_ass)
    thisset=subset(SegToReview,ANALYST==i_ass)
    thisset$REVIEW_ANALYST=sample(others,nrow(thisset),replace=T)
    AssignedSeg=rbind(AssignedSeg,thisset)
  }
  
  #subsample GeoD to only flagged segments
  GeoD4Rev=subset(GeoD,site_seg%in%AssignedSeg$site_seg)
  
  #Build lookup table and assign REVIEW_ANALYST to geodatabase entries
  RA_LU=AssignedSeg$REVIEW_ANALYST
  names(RA_LU)=AssignedSeg$site_seg
  GeoD4Rev$REVIEW_ANALYST=RA_LU[as.vector(GeoD4Rev$site_seg)]
  
  #Check Fairness by Colony Count
  table(GeoD4Rev$REVIEW_ANALYST)
  tt=table(GeoD4Rev$REVIEW_ANALYST)
  mtt=mean(tt)
  sdtt=sd(tt)
  cvtt=sdtt/mtt
  NLoops=NLoops+1
}
if(NLoops==MaxLoops){
  print("Function exited without hitting the target fairness metric. You should re-run or lower the target.")
}else{
  print(paste("After",NLoops,"iterations, the function exited with a fair assignment."))
}

print(paste("The tables below should show:"))
print(paste("(1) that no annotator has been assigned her/his own segment"))
table(AssignedSeg[,c("ANALYST","REVIEW_ANALYST")])
print(paste("(2) that an equal number of segments per annotator are being reviewed"))
table(AssignedSeg[,c("ANALYST")])
print(paste("(3) a roughly equal number of segments are flagged for review per annotator (but randomness in small samples means not exactly"))
table(AssignedSeg[,c("REVIEW_ANALYST")])
print(paste("(4) a roughly equal number of colonies are flagged for review per annotator (but randomness in small samples means not exactly"))
table(GeoD4Rev[,c("REVIEW_ANALYST")])
print(paste("Coeff. of Variation between annotator colony counts:",round(cvtt,4)))

# (5) Build a sub-sampled geo-databse file for each annotator's cr --------
#make output directory if necessary
if(!dir.exists(paste0(workdir,"GeoDatabase_Review_Assignments"))){dir.create(paste0(workdir,"GeoDatabase_Review_Assignments"))}

#Write out review .csv for each REVIEW_ANALYST
for(i_rev in ReviewTeam){
  Assignment=subset(GeoD4Rev,REVIEW_ANALYST==i_rev)
  write.csv(x = Assignment,file = paste0(workdir,"GeoDatabase_Review_Assignments/Benthic_SfM_Geodatabase_ReviewAssignments_",i_rev,".csv"),row.names = F)
}

# Exploration -------------------------------------------------------------


#Explore Colonies Per Segment
#Colonies Per Segment Distribution (~25)
ColPSeg=table(GeoD$site_seg)
L10_ColPSeg=log10(ColPSeg)

md_L10_ColPSeg=median(L10_ColPSeg)
sd_L10_ColPSeg=sd(L10_ColPSeg)
se_L10_ColPSeg=sd(L10_ColPSeg)/sqrt(length(L10_ColPSeg))
CI95_L10_ColPSeg=1.96*sd(L10_ColPSeg)/sqrt(length(L10_ColPSeg))


md=10^md_L10_ColPSeg
sdB=10^(md_L10_ColPSeg+c(-1,1)*sd_L10_ColPSeg)
seB=10^(md_L10_ColPSeg+c(-1,1)*se_L10_ColPSeg)
CI95B=10^(md_L10_ColPSeg+c(-1,1)*CI95_L10_ColPSeg)

hist((ColPSeg),seq(0,175,1)-.5,main=paste(md,"Colonies per Segment \n (median, [",round(CI95B,4)[1],",",round(CI95B,4)[2],"ci95])"))
abline(v=md,col="red",lty=1)
#abline(v=seB,col="blue",lty=2)
abline(v=CI95B,col="green",lty=3)
#abline(v=sdB,col="gray",lty=3)
