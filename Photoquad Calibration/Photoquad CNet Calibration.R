rm(list=ls())

setwd("T:/Benthic/Data/CoralNet Calibration")

library(gdata)             # needed for drop_levels()
library(reshape)           # reshape library inclues the cast() function used below
library(splitstackshape)
library(dplyr)
library(plyr)
library(tidyr)


cnet<-read.csv("ASRAMP2018_CalibrationTest_v2.csv")
lu<-read.csv("T:/Benthic/Data/SpGen_Reference/CNET_categories_lookup.csv")
cnet$SHORT_CODE<-cnet$Cnet_SHORT_CODE

cnet<-left_join(cnet,lu)
nrow(cnet)
cnet$POINTS<-1
cnet$METHOD<-"CNET"
cnet$TIER_1<-cnet$CATEGORY_CODE
cnet$TIER_2<-cnet$SUBCATEGORY_CODE
cnet$TIER_3<-cnet$GENERA_CODE
cnet$IMAGE<-cnet$Name
cnet$T3_DESC<-cnet$GENERA_NAME


ab<-cnet


### SOME CLEAN UP

#CREATING CLASS EMA "Encrusting Macroalgae
levels(ab$TIER_1)<-c(levels(ab$TIER_1), "EMA")
levels(ab$CATEGORY_NAME)<-c(levels(ab$CATEGORY_NAME), "Encrusting macroalga")
ab[ab$TIER_3 %in% c("LOBO","PESP", "EMA"),]$TIER_1<-"EMA"
ab[ab$TIER_3 %in% c("LOBO","PESP", "EMA"),]$TIER_2<-"EMA"

###Create a Halimeda class
ab$TIER_3<-as.character(ab$TIER_3)
ab$TIER_1<-as.character(ab$TIER_1)

for (i in 1:nrow(ab)){ #opening brace
  if(ab$TIER_3[i] =="HALI"){ #c&p
    ab$TIER_1[i]="HALI" #c&p
  } #c&p
} #closing curly brace for entire for loop

hal<-subset(ab,TIER_3=="HALI")
head(hal)

# ###Create a Peyssonnelia class
# ab$GENERA_NAME<-as.character(ab$GENERA_NAME)
# ab$TIER_1<-as.character(ab$TIER_1)
# 
# for (i in 1:nrow(ab)){ #opening brace
#   if(ab$GENERA_NAME[i] =="Peyssonnelia sp"){ #c&p
#     ab$TIER_1[i]="PESP" #c&p
#   } #c&p
# } #closing curly brace for entire for loop
# 
# pes<-subset(ab,TIER_2=="PESP")
# head(pes)

all.tab<-aggregate(ab$POINTS, by=ab[,c("Annotator","IMAGE")], FUN=length)

ab<-droplevels(ab)
table(ab$ISLAND, ab$OBS_YEAR)
summary(ab)



#### NB THERE ARE SEVERAL UNCLASSIFIED CATEGORIES  THAT WILL NEED TO BE REMOVED PRIOR TO CALCULATING % COVER
UNIDENTIFIED_T1<-c("TW", "MF", "UC")
UNIDENTIFIED_T2<-c("MOBF", "TAPE", "UNK", "WAND", "SHAD")

length(unique(ab$SITE))

#Generate a SITE table
SITE_FIELDS<-c("Annotator","IMAGE")
sites<-aggregate(ab[,"POINTS"],by=ab[,SITE_FIELDS], sum)
sites$x<-NULL
dim(sites)

### GENERATE DATA AT SITE LEVEL FOR TIER 1 CATEGORIES
#Sum up all tier 1 points by site - pool transects together. You need to use dcast to insert zero values where there was no coral at a site (for example)
photoT1<-cast(ab, Annotator +IMAGE ~ TIER_1, value="POINTS", fun.aggregate=sum, fill=0)
head(photoT1)

photoT3<-cast(ab, Annotator +IMAGE ~ TIER_3, value="POINTS", fun.aggregate=sum, fill=0)
head(photoT3)

r_levelsT1<-c(unique(as.character(ab$TIER_1)))
photoT1$N<-rowSums(photoT1[,r_levelsT1])
data.colsT1<-c(r_levelsT1)

r_levelsT3<-c(unique(as.character(ab$TIER_3)))
photoT3$N<-rowSums(photoT3[,r_levelsT3])
data.colsT3<-c(r_levelsT3)


#Substract mobile inverts and tape wand shallow and uclassified
photoT1$new.N<-photoT1$N-(photoT1$MF+photoT1$UC+photoT1$TW)

#Calculate proportion
photoT1[,data.colsT1]<-photoT1[,data.colsT1]/photoT1$new.N
head(photoT1)


#Substract mobile inverts and tape wand shallow and uclassified
photoT3$new.N<-photoT3$N-(photoT3$WAND+photoT3$UNK+photoT3$SHAD)

#Calculate proportion
photoT3[,data.colsT3]<-photoT3[,data.colsT3]/photoT3$new.N
photoT3<-subset(photoT3,select = -c(WAND,UNK,SHAD,N,new.N))
head(photoT3)

T3long<-gather(photoT3,TIER_3,cover,ACBR:ZO,factor_key = TRUE) #remove unidenitfied columns
#create a look up table to cover codes to full names
lookup<-ddply(ab,.(TIER_3,T3_DESC),
              summarize,
              count=length(TIER_3))

T3long<-merge(T3long,lookup,by="TIER_3",all.x=TRUE)
se<-function(e) {sd(e)/sqrt(length(e))}

#summary of mean  and se cover
T3sum<-ddply(T3long,.(Annotator,T3_DESC),
             summarize,
             mean=mean(cover*100),
             se=se(cover*100))
head(T3sum)

#Plot Data
p1<-ggplot(T3sum, aes(x=T3_DESC, y=mean, fill=Annotator)) + geom_bar(position=position_dodge(), stat="identity", color="black") + 
  facet_wrap(~T3_DESC,scales="free") +
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001) # adjust x axis to lower the same amount as the genus labels
    ,axis.text.x=element_blank()
    ,legend.position="bottom")+  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),width=.15, position=position_dodge(.9)) + 
  ylab("% Cover") + xlab("Category") 

print(p1)
ggsave("ASRAMP2018_Calibration_barplots_v2.pdf",width=12,height=8,p1)


# Confusion Matrix --------------------------------------------------------


## READ THE DATA FILE - AND PULL OUT BASIC COLUMNS
bdata<-read.csv("ASRAMP2018_CalibrationTest_v2.csv"); head(bdata)
bdata<-bdata[,c("Cnet_SHORT_CODE", "Annotator", "Unique_point")]
head(bdata)

## READ THE CNET CATEGORIES FILE - SORT IT AND ADD FIELD "order"
cats<-read.csv("T:/Benthic/Data/SpGen_Reference/CNETcategories.csv"); head(cats)
cats<-cats[with(cats, order(TIER1, TIER2, TIER3)),]
cats$order<-seq(1,dim(cats)[1])

#MERGE THE DATA FILE WITH THE CATEGORIES, AND add a "DUMMY" field for use in cast function below 
bd<-merge(bdata, cats, by="Cnet_SHORT_CODE", all.x=T)
dim(bd); dim(bdata); summary(bd)
bd$DUMMY<-"x"
head(bd)
## FUNCTION TO GENERATE CONFUSION MATRIX
GenerateCM<-function(x1, x2, comp_level="T3_DESC"){
  
  x1$DLEV<-x1[,comp_level]
  x2$DLEV<-x2[,comp_level]
  
  xx<-cast(rbind(x1, x2), DLEV ~ DUMMY, value="order", min)
  xx<-xx[with(xx, order(x)),]
  x1$DLEV<-factor(x1$DLEV, levels = as.character(xx$DLEV))
  x2$DLEV<-factor(x2$DLEV, levels = as.character(xx$DLEV))
  
  x<-data.frame(T1=x1$TIER1, T2=x1$TIER2, GOOD=x1$DLEV, TEST=x2$DLEV, filler=1)
  return(table(x$GOOD, x$TEST))
  
} #end GenerateCM 

GenerateCM<-function(data,gold, test, lev="TIER3"){
  x1<-data[data$Annotator==gold,]
  x2<-data[data$Annotator==test,]
  tmp<-merge(x1[c(2,3,8)],x2[c(2,3,8)],by="Unique_point")
  colnames(tmp)<-c("Unique_point","Gold","Gold_Tier","Test","Test_Tier")
  tmp$comb<-paste(tmp$Gold_Tier,tmp$Test_Tier,sep="_")
  tmp2<-ddply(tmp,.(comb),
              summarize,
              count=length(comb))
  
  spl <-cSplit(tmp2, 'comb', sep="_", type.convert=FALSE)
  colnames(spl)<-c("count","Gold","Test")
  spl<-as.data.frame(spl)
  ca<-dcast(spl, formula=Gold ~ Test, value.var="count",fill=0)
  return(ca)
  
} #end GenerateCM 


# GenerateCM<-function(x1,x2, lev){
#   test<-merge(x1[c(2,3,8)],x2[c(2,3,8)],by="Unique_point")
#   colnames(test)<-c("Unique_point","Gold","Gold_Tier","Test","Test_Tier")
#   test$comb<-paste(test$Gold_Tier,test$Test_Tier,sep="_")
#   test2<-ddply(test,.(comb),
#                summarize,
#                count=length(comb))
#   
#   spl <-cSplit(test2, 'comb', sep="_", type.convert=FALSE)
#   colnames(spl)<-c("count","Gold","Test")
#   spl<-as.data.frame(spl)
#   ca<-dcast(spl, formula=Gold ~ Test, value.var="count",fill=0)
#   return(ca)
#   
# } #end GenerateCM 



table(bd$Annotator)

#PLOT CONFUSION MATRIX
plotCM<-function(data,gold, test,test2, lev="TIER3"){
  
  # RUN the function - then everyhting below is just to add some additional information to make the outputs cleaner
  # a<-GenerateCM(data[data$Annotator==gold,], data[data$Annotator==test,], lev) #gold is rows, test is columns
  # a<-GenerateCM(bd[bd$Annotator=="cs389",], bd[bd$Annotator=="hatsueb",], lev="TIER3") #gold is rows, test is columns
  a<-GenerateCM(data,gold, test, lev="TIER3") #gold is rows, test is columns
  
  #Convert cells to % of points
  a$totpoints<-rowSums(a[2:(ncol(a)-1)])
  c<-cbind(a[1],a[, 2:(ncol(a)-1)]/a$totpoints*100) # selects every row and 2nd to last columns
  c[is.na(c)]<-0
  d<-a[1:(ncol(a)-1)] 
  
  m<-melt(c);head(m)
  p1<-ggplot(data = m, aes(x = variable, y = Gold)) +
    geom_tile(aes(fill = value),colour="grey")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    scale_fill_gradient('value', limits=c(0, 100), breaks = c(0, 25, 50,75,100),  low = "yellow", high = "red")+
    ylab("Answer Key")+xlab(test)+ggtitle("% of Points")
  
  m<-melt(d)
  p2<-ggplot(data = m, aes(x = variable, y = Gold)) +
    geom_tile(aes(fill = value),colour="white")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    scale_fill_gradient('value', limits=c(0, 275), breaks = c(0, 50, 100,150,275),  low = "lightblue", high = "darkblue")+
    ylab("Answer Key")+xlab(test2)+ggtitle("Total Points")
  
  
  
  ggsave(p1,file=paste(lev, "_", gold, "_", test, "percentpoints.pdf", sep=""),width=10,height=8)
  ggsave(p2,file=paste(lev, "_", gold, "_", test, "totalpoints.pdf", sep=""),width=10,height=8)
  
  return(p1)
  return(p2)
}

#Plot Data
#Answer Key vs. Hatsue
plotCM(bd,gold="cs389",test="hatsueb",test2="Hatsue",lev="TIER3") #create plots
plotCM(bd,gold="cs389",test="elooney12",test2="Erin",lev="TIER3") #create plots
plotCM(bd,gold="cs389",test="paulamisa",test2="Paula",lev="TIER3") #create plots
plotCM(bd,gold="cs389",test="cristirichards",test2="Cristi",lev="TIER3") #create plots
plotCM(bd,gold="cs389",test="mskye13",test2="Mia",lev="TIER3") #create plots


#Transpose Long to Wide to identify which points we have issues with
wide<-dcast(bd,formula=Unique_point~Annotator,value.var="TIER3",fill=NA)

wide$count <- apply(wide[,2:6], 1, function(x)length(unique(x))) # count number of unique categories for each row.
nrow(wide)
wide1<-subset(wide,count==1);nrow(wide1) #subset points that have >=4 classifications
wide2<-subset(wide,count<=2);nrow(wide2) #subset points that have >=4 classifications

wide4<-subset(wide,count>=4);nrow(wide4) #subset points that have >=4 classifications
head(wide4)

write.csv(wide4,"ASRAMPcalib_4.csv")
write.csv(wide2,"ASRAMPcalib_2.csv")
