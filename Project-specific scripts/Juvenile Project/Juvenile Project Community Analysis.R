#https://chrischizinski.github.io/rstats/vegan-ggplot2/

rm(list=ls())

#Load libraries
library(vegan)
library(ggplot2)
library(reshape2)
library(dplyr)
library(plyr)

setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project")

#Read in strata-level data
stw13<-read.csv("T:/Benthic/Projects/Juvenile Project/JuvProject_temporal_STRATA.csv")#includes MHI 2013 data
st<-read.csv("T:/Benthic/Projects/Juvenile Project/JuvProject_STRATAno2013.csv")#excludes MHI 2013 data
st$DB_RZ<-st$DB_RZ_2

# Generate regional plots--------
GenerateMDS_Region<-function(df,region_field,region_name){

  #Subset Region of interest
  df<-subset(df,REGION==region_field)

    #Remove genera that weren't observed in any regions or years
  tmp<-ddply(df,.(GENUS_CODE),summarize,n=sum(JuvColDen))
  nocol<-subset(tmp,n==0)
  df<-df[!df$GENUS_CODE %in% nocol$GENUS_CODE,]
  
  #Convert to wide format
  df.wide<-dcast(df, formula=METHOD+ REGION + ISLAND +SEC_NAME+ANALYSIS_YEAR+STRATANAME+DB_RZ~ GENUS_CODE, value.var="JuvColDen",fill=0)
  df.wide<-subset(df.wide,SSSS!=0)#remove strata that do not have any colonies
  df.wide<-subset(df.wide,select= -c(SSSS)) #remove total hard coral column
  head(df.wide)
  
  #Extract community matrix and meta data
  gen.matrix<-df.wide[8:ncol(df.wide)] #extract community matrix
  head(gen.matrix)
  meta <-df.wide[c(2,5)] #create a dataframe with just site and year
  meta$ANALYSIS_YEAR<-as.factor(meta$ANALYSIS_YEAR)
  
  #calculate distance for NMDS
  mds <- metaMDS(gen.matrix)
  stress.value<-round(mds$stress,3) #extract stress value to plot later
  stress.value<-paste("Stress = ",stress.value)
    
  data.scores <- as.data.frame(scores(mds))  #Using the scores function from vegan to extract the site scores and convert to a data.fram
  data.scores$Stratum <- rownames(data.scores)  # create a column of Stratum names, from the rownames of data.scores
  data.scores<-cbind(data.scores,meta)
  data.scores$region_year<-paste(data.scores$REGION,data.scores$ANALYSIS_YEAR,sep="-")
  
  species.scores <- as.data.frame(scores(mds, "species"))  #Using the scores function from vegan to extract the genus scores and convert to a data.fram
  species.scores$GENUS_CODE <- rownames(species.scores)  # create a column of genus, from the rownames of species.scores


#Generate Hulls that you will plot around each group
  hull.data<-NULL
  R_YR=unique(data.scores$region_year)
  for(i in 1:length(R_YR)){
    chull.df1<-data.scores[data.scores$region_year==R_YR[i], ][chull(data.scores[data.scores$region_year==R_YR[i], c("NMDS1", "NMDS2")]), ]
    hull.data<-rbind(hull.data,chull.df1)
  }

#Plot strata and Genera by year with hull
p<-ggplot() + 
  geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,fill=ANALYSIS_YEAR,group=ANALYSIS_YEAR),alpha=0.30) + # add the convex hulls
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,shape=ANALYSIS_YEAR,colour=ANALYSIS_YEAR),size=2.5) + # add the point markers
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=GENUS_CODE),size=2,alpha=0.5) +  # add the species labels
  annotate(geom = 'text', label = stress.value, x = -Inf, y = Inf, hjust = 0, vjust = 1)+
  coord_equal() +
  theme_bw() + 
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_blank(), # remove x-axis labels
        axis.title.y = element_blank(), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank(),
        legend.title = element_blank(),
        aspect.ratio=1)+
  labs(x=NULL)+
  ggtitle(region_name)
return(p)
}
#set aspect ratio of plots in theme

nwhi<-GenerateMDS_Region(st,"NWHI","Northwestern Hawaiian Islands")
mhi<-GenerateMDS_Region(st,"MHI","Main Hawaiian Islands")
phoneix<-GenerateMDS_Region(st,"PHOENIX","Phoenix Islands")
line<-GenerateMDS_Region(st,"LINE","Line Islands")
samoa<-GenerateMDS_Region(st,"SAMOA","American Samoa")
wake<-GenerateMDS_Region(st,"WAKE","Wake")
nmar<-GenerateMDS_Region(st,"NMARIAN","Northern Marianas")
smar<-GenerateMDS_Region(st,"SMARIAN","Southern Marianas")

allplots<-grid.arrange(nwhi,mhi,phoneix,line,wake,samoa,nmar,smar,nrow=3,ncol=3)
ggsave(plot=allplots,file="T:/Benthic/Projects/Juvenile Project/Figures/Juv_Region_nMDS.pdf",width=10,height=7)



# Generate regional with depth bin separated out plots--------
GenerateMDS_Region_DB<-function(df,region_field,region_name){
  
  #Subset Region of interest
  df<-subset(df,REGION==region_field)
  
  #Remove genera that weren't observed in any regions or years
  tmp<-ddply(df,.(GENUS_CODE),summarize,n=sum(JuvColDen))
  nocol<-subset(tmp,n==0)
  df<-df[!df$GENUS_CODE %in% nocol$GENUS_CODE,]
  
  #Convert to wide format
  df.wide<-dcast(df, formula=METHOD+ REGION + ISLAND +SEC_NAME+ANALYSIS_YEAR+STRATANAME+DEPTH_BIN~ GENUS_CODE, value.var="JuvColDen",fill=0)
  df.wide<-subset(df.wide,SSSS!=0)#remove strata that do not have any colonies
  df.wide<-subset(df.wide,select= -c(SSSS)) #remove total hard coral column
  head(df.wide)
  
  #Extract community matrix and meta data
  gen.matrix<-df.wide[8:ncol(df.wide)] #extract community matrix
  head(gen.matrix)
  meta <-df.wide[c(2,5,7)] #create a dataframe with just site and year
  meta$ANALYSIS_YEAR<-as.factor(meta$ANALYSIS_YEAR)
  
  #calculate distance for NMDS
  mds <- metaMDS(gen.matrix)
  stress.value<-round(mds$stress,3) #extract stress value to plot later
  stress.value<-paste("Stress = ",stress.value)
  
  data.scores <- as.data.frame(scores(mds))  #Using the scores function from vegan to extract the site scores and convert to a data.fram
  data.scores$Stratum <- rownames(data.scores)  # create a column of Stratum names, from the rownames of data.scores
  data.scores<-cbind(data.scores,meta)
  data.scores$region_year<-paste(data.scores$REGION,data.scores$ANALYSIS_YEAR,sep="-")
  data.scores$region_year_depth<-paste(data.scores$REGION,data.scores$ANALYSIS_YEAR,data.scores$DEPTH_BIN,sep="-")
  
  species.scores <- as.data.frame(scores(mds, "species"))  #Using the scores function from vegan to extract the genus scores and convert to a data.fram
  species.scores$GENUS_CODE <- rownames(species.scores)  # create a column of genus, from the rownames of species.scores
  
  
  #Generate Hulls that you will plot around each group
  hull.data<-NULL
  R_YR=unique(data.scores$region_year_depth)
  for(i in 1:length(R_YR)){
    chull.df1<-data.scores[data.scores$region_year_depth==R_YR[i], ][chull(data.scores[data.scores$region_year_depth==R_YR[i], c("NMDS1", "NMDS2")]), ]
    hull.data<-rbind(hull.data,chull.df1)
  }
  
  #Plot strata and Genera by year with hull
  p<-ggplot() + 
    geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,fill=ANALYSIS_YEAR,group=ANALYSIS_YEAR),alpha=0.30) + # add the convex hulls
    geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,colour=ANALYSIS_YEAR),size=2.5) + # add the point markers
    geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=GENUS_CODE),size=2,alpha=0.5) +  # add the species labels
    facet_wrap(~DEPTH_BIN)+
    annotate(geom = 'text', label = stress.value, x = -Inf, y = Inf, hjust = 0, vjust = 1)+
    coord_equal() +
    theme_bw() + 
    theme(panel.background = element_blank(), 
          panel.grid.major = element_blank(),  #remove major-grid labels
          panel.grid.minor = element_blank(),  #remove minor-grid labels
          plot.background = element_blank(),
          legend.title = element_blank(),
          aspect.ratio=1)+
    labs(x=NULL)+
    ggtitle(region_name)
  return(p)
}
#set aspect ratio of plots in theme

nwhi<-GenerateMDS_Region(st,"NWHI","Northwestern Hawaiian Islands")
mhi<-GenerateMDS_Region(st,"MHI","Main Hawaiian Islands")
phoneix<-GenerateMDS_Region(st,"PHOENIX","Phoenix Islands")
line<-GenerateMDS_Region(st,"LINE","Line Islands")
samoa<-GenerateMDS_Region(st,"SAMOA","American Samoa")
wake<-GenerateMDS_Region(st,"WAKE","Wake")
nmar<-GenerateMDS_Region(st,"NMARIAN","Northern Marianas")
smar<-GenerateMDS_Region(st,"SMARIAN","Southern Marianas")

allplots<-grid.arrange(nwhi,mhi,phoneix,line,wake,samoa,nmar,smar,nrow=3,ncol=3)
ggsave(plot=allplots,file="T:/Benthic/Projects/Juvenile Project/Figures/Juv_Region_nMDS.pdf",width=10,height=7)


# Generate separate regional plots with each region faceted by island --------

#I debated about whether to generate separate island-level MDSs, but after speaking with Tom, we thought it was more 
#useful to be able to have all the islands for a given region on the same scale so we can compare between islands within a region. 
#Faceting allows you to see differences betwen years for a given island more easily than plotting all islands and years on same plot
GenerateMDS_Island<-function(df,region_field,region_name){
  
  #Subset Region of interest
  df<-subset(df,REGION==region_field)
    
  #Remove genera that weren't observed in any regions or years
  tmp<-ddply(df,.(GENUS_CODE),summarize,n=sum(JuvColDen))
  nocol<-subset(tmp,n==0)
  df<-df[!df$GENUS_CODE %in% nocol$GENUS_CODE,]
  
  #Convert to wide format
  df.wide<-dcast(df, formula=METHOD+ REGION + ISLAND +SEC_NAME+ANALYSIS_YEAR+STRATANAME+DB_RZ~ GENUS_CODE, value.var="JuvColDen",fill=0)
  df.wide<-subset(df.wide,SSSS!=0)#remove strata that do not have any colonies
  df.wide<-subset(df.wide,select= -c(SSSS)) #remove total hard coral column
  head(df.wide)
  
  #Extract community matrix and meta data
  gen.matrix<-df.wide[8:ncol(df.wide)] #extract community matrix
  head(gen.matrix)
  meta <-df.wide[c(2,3,5)] #create a dataframe with just site and year
  meta$ANALYSIS_YEAR<-as.factor(meta$ANALYSIS_YEAR)
  
  #calculate distance for NMDS
  mds <- metaMDS(gen.matrix)
  stress.value<-round(mds$stress,3) #extract stress value to add to plot later
  stress.value<-paste("(Stress = ",stress.value,")")
  
  data.scores<- as.data.frame(scores(mds))  #Using the scores function from vegan to extract the site scores and convert to a data.fram
  data.scores$Stratum <- rownames(data.scores)  # create a column of Stratum names, from the rownames of data.scores
  data.scores<-cbind(data.scores,meta)
  data.scores$island_year<-paste(data.scores$ISLAND,data.scores$ANALYSIS_YEAR,sep="-")

  species.scores <- as.data.frame(scores(mds, "species"))  #Using the scores function from vegan to extract the genus scores and convert to a data.fram
  species.scores$GENUS_CODE <- rownames(species.scores)  # create a column of genus, from the rownames of species.scores

  
  hull.data<-NULL
  IS_YR=unique(data.scores$island_year)
  for(i in 1:length(IS_YR)){
    chull.df1<-data.scores[data.scores$island_year==IS_YR[i], ][chull(data.scores[data.scores$island_year==IS_YR[i], c("NMDS1", "NMDS2")]), ]
    hull.data<-rbind(hull.data,chull.df1)
  }
  title<-paste(region_name,stress.value,sep = " ")
  #Plot strata and Genera by year with hull
  p<-ggplot() + 
    geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,fill=ANALYSIS_YEAR,group=ANALYSIS_YEAR),alpha=0.30) + # add the convex hulls
    geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=GENUS_CODE),size=3, alpha=0.5) +  # add the species labels
    geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,shape=ANALYSIS_YEAR,colour=ANALYSIS_YEAR),size=2.5) + # add the point markers
    facet_wrap(~ISLAND)+
    #annotate(geom = 'text', label = stress.value, x = -Inf, y = Inf, hjust = 0, vjust = 1)+
    coord_equal() +
    theme_bw() + 
    theme(axis.text.x = element_blank(),  # remove x-axis text
          axis.text.y = element_blank(), # remove y-axis text
          axis.ticks = element_blank(),  # remove axis ticks
          panel.background = element_blank(), 
          panel.grid.major = element_blank(),  #remove major-grid labels
          panel.grid.minor = element_blank(),  #remove minor-grid labels
          plot.background = element_blank(),
          legend.title = element_blank())+
    ggtitle(title)
  
  ggsave(plot=p,path="T:/Benthic/Projects/Juvenile Project/Figures",width=8,height=6,filename=paste0("Juv_Island_nMDS", sep="_", region_name, ".png"))
  return(p)
}

#remove islands that don't have at least 3 strata in each time point
st.sub<-st[!(st$ISLAND %in% c("French Frigate","Niihau","Swains","Alamagan","Sarigan","Aguijan")),]

nwhi<-GenerateMDS_Island(st.sub,"NWHI","Northwestern Hawaiian Islands")
mhi<-GenerateMDS_Island(st.sub,"MHI","Main Hawaiian Islands")
phoneix<-GenerateMDS_Island(st.sub,"PHOENIX","Phoenix Islands")
line<-GenerateMDS_Island(st.sub,"LINE","Line Islands")
samoa<-GenerateMDS_Island(st.sub,"SAMOA","American Samoa")
wake<-GenerateMDS_Island(st.sub,"WAKE","Wake")
nmar<-GenerateMDS_Island(st.sub,"NMARIAN","Northern Marianas")
smar<-GenerateMDS_Island(st.sub,"SMARIAN","Southern Marianas")


#Plot boxplots of the most abundant taxa between years for each year. 
meanden<-ddply(subset(st.sub,GENUS_CODE=="SSSS"),.(REGION),
        summarize,
        regionMean=mean(JuvColDen),
        per10=regionMean*0.15)
taxarank<-ddply(st.sub,.(REGION,GENUS_CODE),
                summarize,
                meanJuv=mean(JuvColDen),maxJuv=max(JuvColDen))
df.tax<-left_join(taxarank,meanden)
df.tax2<-subset(df.tax,maxJuv>per10& GENUS_CODE!="SSSS")
df.tax2$R_gen<-paste(df.tax2$REGION,df.tax2$GENUS_CODE,sep = "_")

st.sub$R_gen<-paste(st.sub$REGION,st.sub$GENUS_CODE,sep = "_")

GenerateBoxPlot_Island<-function(df,region_field,region_name){
  
  #Subset Region of interest
  df<-subset(df,REGION==region_field)
  
  s<-dplyr::filter(df, R_gen %in% df.tax2$R_gen)

  #Plot strata and Genera by year with hull
  s$ANALYSIS_YEAR<-as.factor(s$ANALYSIS_YEAR)
  p<-ggplot(data=s,aes(x=GENUS_CODE,y=JuvColDen,fill=ANALYSIS_YEAR)) + 
    geom_boxplot() + 
    facet_wrap(~ISLAND,scales ="free")+
    theme_bw() + 
    theme(
      axis.text.x = element_text(angle = 90)
      ,plot.background = element_blank()
      ,panel.grid.major = element_blank()
      ,panel.grid.minor = element_blank()
      ,axis.title.x = element_text( vjust = -.0001))+
  ggtitle(region_name)
  
  ggsave(plot=p,path="T:/Benthic/Projects/Juvenile Project/Figures",width=8,height=6,filename=paste0("Juv_Island_boxplots", sep="_", region_name, ".png"))
  return(p)
}

nwhi<-GenerateBoxPlot_Island(st.sub,"NWHI","Northwestern Hawaiian Islands")
mhi<-GenerateBoxPlot_Island(st.sub,"MHI","Main Hawaiian Islands")
phoneix<-GenerateBoxPlot_Island(st.sub,"PHOENIX","Phoenix Islands")
line<-GenerateBoxPlot_Island(st.sub,"LINE","Line Islands")
samoa<-GenerateBoxPlot_Island(st.sub,"SAMOA","American Samoa")
wake<-GenerateBoxPlot_Island(st.sub,"WAKE","Wake")
nmar<-GenerateBoxPlot_Island(st.sub,"NMARIAN","Northern Marianas")
smar<-GenerateBoxPlot_Island(st.sub,"SMARIAN","Southern Marianas")
