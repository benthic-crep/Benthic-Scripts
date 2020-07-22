
rm(list=ls())

#LOAD LIBRARY FUNCTIONS ... 
source("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/Functions/Benthic_Functions_newApp_vTAOfork.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/core_functions.R")
source("C:/Users/Courtney.S.Couch/Documents/GitHub/fish-paste/lib/GIS_functions.R")

#Read in files
df<-read.csv("C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/Cross QC/Cross QC Concat.csv")

head(df);nrow(df)

#If you are adding a missing colony, make sure there is no information in the correct columns so you don't double count errors
cols <- c("SPCODE_CORRECT", "OLDDEAD_CORRECT", "MORPH_CORRECT","RDCAUSE_CORRECT","CONDITION_CORRECT","EXTENT_CORRECT","SEVERITY_CORRECT","SIZE_CORRECT")
df[is.na(df$FID), cols] <- NA
View(df)

#Identify spcodes that match
df$SPCODE<-as.character(df$SPCODE)
df$SPCODE_CORRECT<-as.character(df$SPCODE_CORRECT)
df[which(df$SPCODE == df$SPCODE_CORRECT),]

#Add MISSING colony column
df$MISSING_COLONY<-ifelse(is.na(df$FID),1,NA);View(df)

df[,38:49] <- lapply(df[,38:49], as.character)

head(df)

df$SIZE_CORRECT<-as.numeric(df$SIZE_CORRECT)

df$delsize<-df$SIZE_CORRECT-df$Shape_Leng

df[is.na(df)]<-"NONE"


df<-df[,c(1:6,35:50)]
df[df=="NONE"]<-0;head(df)

#change to errors to 1
df[,10:22] <- ifelse(df[,10:22]!=0, 1,0)

df.col<-subset(df,MISSING_COLONY!=-1)


gl.error<-ddply(df.col,.(SEGAREA),
                 summarize,
                 ls.error=sum(Lump_Split_Error)/length(Lump_Split_Error)*100,
                 delcol.error=sum(DELETE_COLONY)/length(DELETE_COLONY)*100,
                 od.error=sum(OLDDEAD_CORRECT)/length(OLDDEAD_CORRECT)*100,
                 sp.error=sum(SPCODE_CORRECT)/length(SPCODE_CORRECT)*100,
                 rdc.error=sum(RDCAUSE_CORRECT)/length(RDCAUSE_CORRECT)*100,
                 con.error=sum(CONDITION_CORRECT)/length(CONDITION_CORRECT)*100,
                 size.error=sum(SIZE_CORRECT)/length(SIZE_CORRECT)*100)


gl.error

missingcol<-ddply(df,.(SEGAREA),
                summarize,
                miss.error=sum(MISSING_COLONY)/length(MISSING_COLONY)*100)
missingcol

gl.error<-left_join(gl.error,missingcol)

df.longG<-gather(gl.error,ErrorType,value,ls.error:miss.error)
df.longG$ANALYST<-"AllAnnotators"
df.longG <- df.longG[, c(1,4,2,3)]



#By annotator
gl.error<-ddply(df.col,.(SEGAREA,ANALYST),
                summarize,
                ls.error=sum(Lump_Split_Error)/length(Lump_Split_Error)*100,
                delcol.error=sum(DELETE_COLONY)/length(DELETE_COLONY)*100,
                od.error=sum(OLDDEAD_CORRECT)/length(OLDDEAD_CORRECT)*100,
                sp.error=sum(SPCODE_CORRECT)/length(SPCODE_CORRECT)*100,
                rdc.error=sum(RDCAUSE_CORRECT)/length(RDCAUSE_CORRECT)*100,
                con.error=sum(CONDITION_CORRECT)/length(CONDITION_CORRECT)*100,
                size.error=sum(SIZE_CORRECT)/length(SIZE_CORRECT)*100)


gl.error

missingcol<-ddply(df,.(SEGAREA,ANALYST),
                  summarize,
                  miss.error=sum(MISSING_COLONY)/length(MISSING_COLONY)*100)
missingcol

gl.error<-left_join(gl.error,missingcol)

df.long<-gather(gl.error,ErrorType,value,ls.error:miss.error)

#merge with globalerror
df.long<-rbind(df.long,df.longG);View(df.long)

a<-subset(df.long,SEGAREA=="2.5")
library(treemap)
png(filename="C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/Cross QC/AdultCrossQC_treemap.png",width=800, height=600)
treemap(a, #Your data frame object
        index=c("ANALYST","ErrorType"),  #A list of your categorical variables
        vSize = "value",  #This is your quantitative variable
        type="index", #Type sets the organization and color scheme of your treemap
        palette = "Reds",  #Select your color palette from the RColorBrewer presets or make your own.
        title="Error Rates for Adult Colonies", #Customize your title
        fontsize.title = 14 #Change the font size of the title
)

#ggsave(plot=tree1,file="C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/Cross QC/AdultCrossQC_treemap.pdf",width=12,height=10)
dev.off()

p1<-ggplot(subset(df.long,SEGAREA=="2.5"), aes(x=ErrorType, y=value, fill=ErrorType)) + 
  geom_bar(stat = "identity", position = position_dodge2(preserve='single'),color="black",width = 1) +
  geom_hline(yintercept=10, color = "black",lty=2)+
  facet_wrap(~ANALYST, labeller=label_parsed) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90,size=12)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001) # adjust x axis to lower the same amount as the genus labels
    ,legend.position="none")+
  labs(x="Error Type",y="Error Rate(% of colonies)")+
  ggtitle("Adults")
p1
ggsave(plot=p1,file="C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/Cross QC/AdultCrossQC_barplot.pdf",width=10,height=8)


j<-subset(df.long,SEGAREA=="1")
library(treemap)
png(filename="C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/Cross QC/JuvenileCrossQC_treemap.png",width=800, height=600)
treemap(j, #Your data frame object
               index=c("ANALYST","ErrorType"),  #A list of your categorical variables
               vSize = "value",  #This is your quantitative variable
               type="index", #Type sets the organization and color scheme of your treemap
               palette = "Reds",  #Select your color palette from the RColorBrewer presets or make your own.
               title="Error Rates for Juvenile Colonies", #Customize your title
               fontsize.title = 14 #Change the font size of the title
)
dev.off()

p2<-ggplot(subset(df.long,SEGAREA=="1"), aes(x=ErrorType, y=value, fill=ErrorType)) + 
  geom_bar(stat = "identity", position = position_dodge2(preserve='single'),color="black",width = 1) +
  geom_hline(yintercept=10, color = "black",lty=2)+
  facet_wrap(~ANALYST, labeller=label_parsed) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90,size=12)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001) # adjust x axis to lower the same amount as the genus labels
    ,legend.position="none")+
  labs(x="Error Type",y="Error Rate(% of colonies)")+
  ggtitle("Juveniles")
p2

ggsave(plot=p2,file="C:/Users/Courtney.S.Couch/Documents/GitHub/Benthic-Scripts/SfM/Method Comparision/Cross QC/JuvenileCrossQC_barplot.pdf",width=10,height=8)



# Error at the segment level ----------------------------------------------


#Quantify the number of colonies that have at least 1 error- removed morph error check since this is super subjective
new.df<-df %>%
  mutate(GLOBAL_ERROR = ifelse(Lump_Split_Error!="NONE"|SPCODE_CORRECT!="NONE"|RDCAUSE_CORRECT!="NONE"|CONDITION_CORRECT!="NONE"|
                                 SIZE_CORRECT!="NONE"|DELETE_COLONY!="NONE"|OLD_DEAD_CORRECT!="NONE"|MISSING_COLONY!="NONE",1,0))


new.df<-new.df[,c(1:6,35:50)]
new.df[new.df=="NONE"]<-0;head(new.df)

#change to errors to 1
new.df[,10:22] <- ifelse(new.df[,10:22]!=0, 1,0)

  
g.error<-ddply(new.df,.(site_seg,SEGAREA,ANALYST),
               summarize,
               totalcol=length(GLOBAL_ERROR),
               globe.error=sum(GLOBAL_ERROR),
               perc.error=globe.error/totalcol*100)
g.error


g.error.sum<-ddply(g.error,.(ANALYST,SEGAREA),
                   summarize,
                   mean.error=mean(perc.error),
                   se.error=std.error(perc.error),
                   ncol=sum(totalcol))

g.error.sum

all.error<-ddply(new.df,.(site_seg,SEGAREA,ANALYST),
               summarize,
               glob.error=sum(GLOBAL_ERROR)/length(GLOBAL_ERROR)*100,
               ls.error=sum(Lump_Split_Error)/length(Lump_Split_Error)*100,
               misscol.error=sum(MISSING_COLONY)/length(MISSING_COLONY)*100,
               delcol.error=sum(DELETE_COLONY)/length(DELETE_COLONY)*100,
               od.error=sum(OLDDEAD_CORRECT)/length(OLDDEAD_CORRECT)*100,
               sp.error=sum(SPCODE_CORRECT)/length(SPCODE_CORRECT)*100,
               rdc.error=sum(RDCAUSE_CORRECT)/length(RDCAUSE_CORRECT)*100,
               con.error=sum(CONDITION_CORRECT)/length(CONDITION_CORRECT)*100,
               size.error=sum(SIZE_CORRECT)/length(SIZE_CORRECT)*100)
               
               
all.error
df.long<-gather(all.error,ErrorType,value,glob.error:size.error)

df.long$ErrorType <- factor(df.long$ErrorType, levels = c("glob.error","ls.error","misscol.error","delcol.error",
                                                          "size.error","sp.error","od.error","rdc.error","con.error"))

#bar plots of juv desnity by sector by year
p1<-ggplot(subset(df.long,SEGAREA=="2.5"), aes(x=ErrorType, y=value, fill=ErrorType)) + 
  geom_boxplot() + 
  geom_jitter(height=0,width=0.25)+
  geom_hline(yintercept=10, color = "black",lty=2)+
  facet_wrap(~ANALYST, labeller=label_parsed) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001) # adjust x axis to lower the same amount as the genus labels
    ,legend.position="none")+
  labs(x="Error Type",y="Error Rate(% of colonies)")+
  ylim(c(0,10))+
  ggtitle("Adults")
p1

j<-df.long[!(df.long$ErrorType %in% c("od.error","rdc.error","con.error")),]

p2<-ggplot(subset(j,SEGAREA=="1"), aes(x=ErrorType, y=value, fill=ErrorType)) + 
  geom_boxplot() + 
  geom_hline(yintercept=10, color = "black",lty=2)+
  facet_wrap(~ANALYST, labeller=label_parsed) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001) # adjust x axis to lower the same amount as the genus labels
    ,legend.position="none")+
  labs(x="Error Type",y="Error Rate(% of colonies)")+
  ggtitle("Juveniles")
p2



#mean and se for bar plots
all.error.mean<-ddply(all.error,.(SEGAREA,ANALYST),
                 summarize,
                 global=mean(glob.error),
                 lump_split=mean(ls.error),
                 misscol=mean(misscol.error),
                 delcol=mean(delcol.error),
                 olddead=mean(od.error),
                 spcode=mean(sp.error),
                 rdcause=mean(rdc.error),
                 condition=mean(con.error),
                 size=mean(size.error))

all.error.se<-ddply(all.error,.(SEGAREA,ANALYST),
                    summarize,
                    global=std.error(glob.error),
                    lump_split=std.error(ls.error),
                    misscol=std.error(misscol.error),
                    delcol=std.error(delcol.error),
                    olddead=std.error(od.error),
                    spcode=std.error(sp.error),
                    rdcause=std.error(rdc.error),
                    condition=std.error(con.error),
                    size=std.error(size.error))


View(all.error.mean)

df.longM<-gather(all.error.mean,ErrorType,Mean,global:size)
head(df.longM)

df.longSE<-gather(all.error.se,ErrorType,SE,global:size)
head(df.longSE)
sum.df<-left_join(df.longM,df.longSE);View(sum.df)

sum.df$ErrorType <- factor(sum.df$ErrorType, levels = c("global","lump_split","misscol","delcol",
                                                          "size","spcode","olddead","rdcause","condition"))

#bar plots of juv desnity by sector by year
p3<-ggplot(subset(sum.df,SEGAREA=="2.5"), aes(x=ErrorType, y=Mean, fill=ErrorType)) + 
  geom_bar(stat = "identity", position = position_dodge2(preserve='single'),color="black",width = 1) +
  geom_errorbar(aes(y=Mean, x=ErrorType,ymin=Mean-SE, ymax=Mean+SE), width=.2)+
  geom_hline(yintercept=10, color = "black",lty=2)+
  facet_wrap(~ANALYST, labeller=label_parsed) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001) # adjust x axis to lower the same amount as the genus labels
    ,legend.position="none")+
  labs(x="Error Type",y="Mean Error Rate(% of colonies)")+
  ggtitle("Adults")
p3

j<-sum.df[!(sum.df$ErrorType %in% c("od.error","rdc.error","con.error")),]

p4<-ggplot(subset(j,SEGAREA=="1"), aes(x=ErrorType, y=Mean, fill=ErrorType)) + 
  geom_bar(stat = "identity", position = position_dodge2(preserve='single'),color="black",width = 1) +
  geom_errorbar(aes(y=Mean, x=ErrorType,ymin=Mean-SE, ymax=Mean+SE), width=.2)+
  geom_hline(yintercept=10, color = "black",lty=2)+
  facet_wrap(~ANALYST, labeller=label_parsed) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001) # adjust x axis to lower the same amount as the genus labels
    ,legend.position="none")+
  labs(x="Error Type",y="Mean Error Rate(% of colonies)")+
  ggtitle("Juveniles")
p4






#look at error by reviewer
#Mollie had higher missing colony error as a reviewer than others because she reviewed a lot of Mia's segments who tended to miss colonies
review.error<-ddply(new.df,.(site_seg,SEGAREA,REVIEW_ANALYST),
                 summarize,
                 glob.error=sum(GLOBAL_ERROR)/length(GLOBAL_ERROR)*100,
                 ls.error=sum(Lump_Split_Error)/length(Lump_Split_Error)*100,
                 misscol.error=sum(MISSING_COLONY)/length(MISSING_COLONY)*100,
                 delcol.error=sum(DELETE_COLONY)/length(DELETE_COLONY)*100,
                 od.error=sum(OLDDEAD_CORRECT)/length(OLDDEAD_CORRECT)*100,
                 sp.error=sum(SPCODE_CORRECT)/length(SPCODE_CORRECT)*100,
                 rdc.error=sum(RDCAUSE_CORRECT)/length(RDCAUSE_CORRECT)*100,
                 con.error=sum(CONDITION_CORRECT)/length(CONDITION_CORRECT)*100,
                 size.error=sum(SIZE_CORRECT)/length(SIZE_CORRECT)*100)


review.error.mean<-ddply(review.error,.(SEGAREA,REVIEW_ANALYST),
                      summarize,
                      global=mean(glob.error),
                      lump_split=mean(ls.error),
                      misscol=mean(misscol.error),
                      delcol=mean(delcol.error),
                      od=mean(od.error),
                      spcode=mean(sp.error),
                      rdcause=mean(rdc.error),
                      condition=mean(con.error),
                      size=mean(size.error))

review.error.mean
