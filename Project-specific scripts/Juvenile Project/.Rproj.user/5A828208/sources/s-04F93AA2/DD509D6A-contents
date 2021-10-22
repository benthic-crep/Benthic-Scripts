#https://chrischizinski.github.io/rstats/vegan-ggplot2/
#See the following look up table for code definitions T:\Benthic\Data\Lookup Tables\2013-20_Taxa_MASTER.csv

rm(list=ls())

#Load libraries
library(vegan)
library(ggplot2)
library(reshape2)
library(dplyr)
library(plyr)
library(gridExtra)

setwd("C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project")

#Read in strata-level data
stw13<-read.csv("T:/Benthic/Projects/Juvenile Project/JuvProject_STRATA_WITHOUT_MHI2013.csv")#includes MHI 2013 data
st<-read.csv("T:/Benthic/Projects/Juvenile Project/JuvProject_STRATA_WITH_MHI2013.csv")#excludes MHI 2013 data



#remove Unknown and tubastrea
st<-st[!st$GENUS_CODE %in% c("UNKN","TUBA"),]

#Remove FASP from MHI dataset- this was an error and only recorded at 1 site in 2016
st$JuvColDen<-ifelse(st$GENUS_CODE=="FASP"&st$REGION=="MHI",0,st$JuvColDen)

View(st)


#I still need to play with different data transformations (e.g. sqrt or 4th root) and removing rare taxa

#Create community matrix 
Generate_Community_matrix<-function(df){
  
#Remove genera that weren't observed in a given region or year
tmp<-ddply(df,.(GENUS_CODE),summarize,n=sum(JuvColDen))
nocol<-subset(tmp,n==0)
df<-df[!df$GENUS_CODE %in% nocol$GENUS_CODE,]

#Convert to wide format
df.wide<-dcast(df, formula=REGION + ISLAND +SEC_NAME+ANALYSIS_YEAR+STRATANAME+DEPTH_BIN~ GENUS_CODE, value.var="JuvColDen",fill=0)
df.wide<-subset(df.wide,SSSS!=0)#remove strata that do not have any colonies
df.wide<-subset(df.wide,select= -c(SSSS)) #remove total hard coral column
head(df.wide)

#Extract community matrix and meta data
gen.matrix<-df.wide[7:ncol(df.wide)] #extract community matrix

return(gen.matrix)
}

#Create metadata dataframe
Generate_metadata<-function(df){
  
  #Remove genera that weren't observed in a given region or year
  tmp<-ddply(df,.(GENUS_CODE),summarize,n=sum(JuvColDen))
  nocol<-subset(tmp,n==0)
  df<-df[!df$GENUS_CODE %in% nocol$GENUS_CODE,]
  
  #Convert to wide format
  df.wide<-dcast(df, formula=REGION + ISLAND +SEC_NAME+ANALYSIS_YEAR+STRATANAME+DEPTH_BIN~ GENUS_CODE, value.var="JuvColDen",fill=0)
  df.wide<-subset(df.wide,SSSS!=0)#remove strata that do not have any colonies
  df.wide<-subset(df.wide,select= -c(SSSS)) #remove total hard coral column
  head(df.wide)
  
  head(gen.matrix)
  meta <-df.wide[c(1:6)] #create a dataframe with env data
  meta$ANALYSIS_YEAR<-as.factor(meta$ANALYSIS_YEAR)
  
  return(meta)
}

Permanova_Year<-function(df,region_field){
  
  #Subset Region of interest
  df<-subset(df,REGION==region_field)
  
  #generate the community matrix and metadata df
  gen.matrix<-Generate_Community_matrix(df)
  meta<-Generate_metadata(df)
  
  ### ------------ Distance matrix ----------------------------------------------
  ### Compute distance matrix using Bray-Curtis on raw density
  range(gen.matrix) #the range looks good- no need to transform
  
  ### ------------ PERMANOVA -----------------------------------------------------
  pmv1 <- adonis(gen.matrix ~ ANALYSIS_YEAR, data = meta,permutations = 999,method = "bray")
  
  return(pmv1)
}
Permanova_Year_Depth<-function(df,region_field){

#Subset Region of interest
df<-subset(df,REGION==region_field)

#generate the community matrix and metadata df
gen.matrix<-Generate_Community_matrix(df)
meta<-Generate_metadata(df)

### ------------ Distance matrix ----------------------------------------------
### Compute distance matrix using Bray-Curtis on raw density
range(gen.matrix) #the range looks good- no need to transform

### ------------ PERMANOVA -----------------------------------------------------
pmv1 <- adonis(gen.matrix ~ ANALYSIS_YEAR*DEPTH_BIN, data = meta,permutations = 999,method = "bray")

return(pmv1)
}
Permanova_Year_Island<-function(df,region_field){
  
  #Create community matrix
  #Subset Region of interest
  df<-subset(df,REGION==region_field)
  
  #generate the community matrix and metadata df
  gen.matrix<-Generate_Community_matrix(df)
  meta<-Generate_metadata(df)
  
  ### ------------ Distance matrix ----------------------------------------------
  ### Compute distance matrix using Bray-Curtis on raw density
  range(gen.matrix) #the range looks good- no need to transform
  
  dist_den <- vegdist(gen.matrix, method = "bray")
  
  ### ------------ PERMANOVA -----------------------------------------------------
  pmv1 <- adonis(gen.matrix ~ ANALYSIS_YEAR*ISLAND, data = meta,permutations = 999,method = "bray")
  
  return(pmv1)
}
Simper_Year<-function(df,region_field){
  
  #Create community matrix
  #Subset Region of interest
  df<-subset(df,REGION==region_field)
  
  #generate the community matrix and metadata df
  gen.matrix<-Generate_Community_matrix(df)
  meta<-Generate_metadata(df)
  
  sim <- simper(gen.matrix, group = meta$ANALYSIS_YEAR)
  sim
  summary(sim) 
  return(summary(sim))
}

#check PERMANOVA assumption of equal dispersion among groups for a given region
#No signficant diff in dispersion for YEAR, but box plots show mild dispersion in Phoenix islands
#No signficant diff in dispersion for ISLAND, but box plots show mild dispersion in LINE, MHI AND NMARIAN

df2<-subset(st,REGION=="LINE")
gen.matrix<-Generate_Community_matrix(df2)
meta<-Generate_metadata(df2)

gen.matrix_disp <- vegdist((gen.matrix), method = "bray")

#Analysis year
bd <-  betadisper(gen.matrix_disp, meta$ANALYSIS_YEAR)
boxplot(bd)
anova(bd) 
#TukeyHSD(bd, ordered = FALSE, conf.level = 0.95)

# Island
bd <-  betadisper(gen.matrix_disp, meta$ISLAND)
boxplot(bd)
anova(bd) 
#TukeyHSD(bd, ordered = FALSE, conf.level = 0.95)

#Run PERMANOVAs for each region
Permanova_Year_Island(st,"MHI")
Permanova_Year_Depth(st,"MHI")
Simper_Year(st,"MHI")

Permanova_Year_Island(st,"NWHI")
Permanova_Year_Depth(st,"NWHI")
Simper_Year(st,"NWHI")

Permanova_Year(st,"LINE")
Permanova_Year_Island(st,"LINE")
Permanova_Year_Depth(st,"LINE")
Simper_Year(st,"LINE")

Permanova_Year_Island(st,"PHOENIX")
Permanova_Year_Depth(st,"PHOENIX")
Simper_Year(st,"PHOENIX")

Permanova_Year_Island(st,"SAMOA")
Permanova_Year_Depth(st,"SAMOA")
Simper_Year(st,"SAMOA")

Permanova_Year_Island(st,"NMARIAN")
Permanova_Year_Depth(st,"NMARIAN")
Simper_Year(st,"NMARIAN")

Permanova_Year_Island(st,"SMARIAN")
Permanova_Year_Depth(st,"SMARIAN")
Simper_Year(st,"SMARIAN")


# Generate regional plots--------
GenerateMDS_Region<-function(df,region_field,region_name){

  #Subset Region of interest
  df<-subset(df,REGION==region_field)

  #generate the community matrix and metadata df
  gen.matrix<-Generate_Community_matrix(df)
  meta<-Generate_metadata(df)
  
  #calculate distance for NMDS
  mds <- metaMDS(gen.matrix, distance = "bray", k=2,trymax = 1000)
  stress.value<-round(mds$stress,3) #extract stress value to plot later
  stress.value<-paste("Stress = ",stress.value)
    
  data.scores <- as.data.frame(scores(mds))  #Using the scores function from vegan to extract the site scores and convert to a data.fram
  data.scores$Stratum <- rownames(data.scores)  # create a column of Stratum names, from the rownames of data.scores
  data.scores<-cbind(data.scores,meta)
  data.scores$region_year<-paste(data.scores$REGION,data.scores$ANALYSIS_YEAR,sep="-")
  
  species.scores <- as.data.frame(scores(mds, display = "species"))  #Using the scores function from vegan to extract the genus scores and convert to a data.fram
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
ggsave(plot=allplots,file="T:/Benthic/Projects/Juvenile Project/Figures/Juv_Region_nMDS.png",width=12,height=9)



# Generate regional with depth bin separated out plots--------
GenerateMDS_Region_DB<-function(df,region_field,region_name){
  
  #Subset Region of interest
  df<-subset(df,REGION==region_field)
  
  #generate the community matrix and metadata df
  gen.matrix<-Generate_Community_matrix(df)
  meta<-Generate_metadata(df)
  
  #calculate distance for NMDS
  mds <- metaMDS(gen.matrix)
  stress.value<-round(mds$stress,3) #extract stress value to plot later
  stress.value<-paste("(Stress = ",stress.value,")")
  
  data.scores <- as.data.frame(scores(mds))  #Using the scores function from vegan to extract the site scores and convert to a data.fram
  data.scores$Stratum <- rownames(data.scores)  # create a column of Stratum names, from the rownames of data.scores
  data.scores<-cbind(data.scores,meta)
  data.scores$region_year<-paste(data.scores$REGION,data.scores$ANALYSIS_YEAR,sep="-")
  data.scores$region_year_depth<-paste(data.scores$REGION,data.scores$ANALYSIS_YEAR,data.scores$DEPTH_BIN,sep="-")
  
  species.scores <- as.data.frame(scores(mds, "species"))  #Using the scores function from vegan to extract the genus scores and convert to a data.fram
  species.scores$GENUS_CODE <- rownames(species.scores)  # create a column of genus, from the rownames of species.scores
  
  
  #Generate the hull/polygon data that will be used to group strata by depth and year in the plot
  hull.data<-NULL
  R_YR=unique(data.scores$region_year_depth)
  for(i in 1:length(R_YR)){
    chull.df1<-data.scores[data.scores$region_year_depth==R_YR[i], ][chull(data.scores[data.scores$region_year_depth==R_YR[i], c("NMDS1", "NMDS2")]), ]
    hull.data<-rbind(hull.data,chull.df1)
  }
  title<-paste(region_name,stress.value,sep = " ")
  #Plot strata and Genera by year with hull
  p<-ggplot() + 
    geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,fill=ANALYSIS_YEAR,group=ANALYSIS_YEAR),alpha=0.30) + # add the convex hulls
    geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,colour=ANALYSIS_YEAR),size=2.5) + # add the point markers
    geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=GENUS_CODE),size=2,alpha=0.5) +  # add the species labels
    facet_wrap(~DEPTH_BIN)+
    coord_equal() +
    theme_bw() + 
    theme(axis.text.x = element_blank(),  # remove x-axis text
          axis.text.y = element_blank(), # remove y-axis text
          axis.ticks = element_blank(),  # remove axis ticks
          axis.title.x = element_blank(), # remove x-axis labels
          axis.title.y = element_blank(),
          panel.background = element_blank(), 
          panel.grid.major = element_blank(),  #remove major-grid labels
          panel.grid.minor = element_blank(),  #remove minor-grid labels
          plot.background = element_blank(),
          legend.title = element_blank(),
          aspect.ratio=1)+
    ggtitle(title)
  return(p)
}

#set aspect ratio of plots in theme

#nwhi<-GenerateMDS_Region_DB(st,"NWHI","Northwestern Hawaiian Islands")
mhi<-GenerateMDS_Region_DB(st,"MHI","Main Hawaiian Islands")
#phoneix<-GenerateMDS_Region_DB(st,"PHOENIX","Phoenix Islands")
line<-GenerateMDS_Region_DB(st,"LINE","Line Islands")
samoa<-GenerateMDS_Region_DB(st,"SAMOA","American Samoa")
nmar<-GenerateMDS_Region_DB(st,"NMARIAN","Northern Marianas")
smar<-GenerateMDS_Region_DB(st,"SMARIAN","Southern Marianas")

allplots<-grid.arrange(mhi,line,samoa,nmar,smar,nrow=5,ncol=1)
ggsave(plot=allplots,file="T:/Benthic/Projects/Juvenile Project/Figures/Juv_Region_Depth_nMDS.png",width=10,height=18)


# Generate separate regional plots with each region faceted by island --------

#I debated about whether to generate separate island-level MDSs, but after speaking with Tom, we thought it was more 
#useful to be able to have all the islands for a given region on the same scale so we can compare between islands within a region. 
#Faceting allows you to see differences betwen years for a given island more easily than plotting all islands and years on same plot
GenerateMDS_Island<-function(df,region_field,region_name){
  
  #Subset Region of interest
  df<-subset(df,REGION==region_field)
    
  #generate the community matrix and metadata df
  gen.matrix<-Generate_Community_matrix(df)
  meta<-Generate_metadata(df)
  
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

  #Generate the hull/polygon data that will be used to group strata by island and year in the plot
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
    geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=GENUS_CODE),size=2, alpha=0.5) +  # add the species labels
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


# Generate separate regional plots with each region faceted by island- Spider plot --------
GenerateMDS_Island_spider<-function(df,region_field,region_name){
  
  #Subset Region of interest
  df<-subset(df,REGION==region_field)
  
  #generate the community matrix and metadata df
  gen.matrix<-Generate_Community_matrix(df)
  meta<-Generate_metadata(df)
  
  #calculate distance for NMDS
  mds <- metaMDS(gen.matrix)
  stress.value<-round(mds$stress,3) #extract stress value to add to plot later
  stress.value<-paste("(Stress = ",stress.value,")")
  
  data.scores<- as.data.frame(scores(mds))  #Using the scores function from vegan to extract the site scores and convert to a data.fram
  data.scores$Stratum <- rownames(data.scores)  # create a column of Stratum names, from the rownames of data.scores
  data.scores<-cbind(data.scores,meta)
  data.scores$island_year<-paste(data.scores$ISLAND,data.scores$ANALYSIS_YEAR,sep="-")
  
  species.scores <- as.data.frame(scores(mds, "species"))  #Using the scores function from vegan to extract the genus scores and convert to a data.frame
  species.scores$GENUS_CODE <- rownames(species.scores)  # create a column of genus, from the rownames of species.scores
  
  #Calculate the group centroids
  cent<-ddply(data.scores,.(REGION,ISLAND),summarize,
              cNMDS1=mean(NMDS1),
              cNMDS2=mean(NMDS2))
  
  segs<-left_join(data.scores,cent)
  
  title<-paste(region_name,stress.value,sep = " ")
  
  #Plot strata-level data in spider plot 
  p<-ggplot(data.scores,aes(x=NMDS1,y=NMDS2)) + 
    geom_segment(data=segs,mapping = aes(xend=cNMDS1,yend=cNMDS2,color=ISLAND)) + # add segments
    geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=GENUS_CODE),size=2, alpha=0.5) +  # add the species labels
    geom_point(aes(color=ISLAND,shape=ANALYSIS_YEAR),size=2.5) + # add the point markers
    geom_point(data=cent,aes(x=cNMDS1,y=cNMDS2,colour=ISLAND),size=1) + # add centroids
    guides(colour=FALSE)+ #remove colors from legend
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
  
  ggsave(plot=p,path="T:/Benthic/Projects/Juvenile Project/Figures",width=8,height=6,filename=paste0("Juv_Island_nMDS_spider", sep="_", region_name, ".png"))
  return(p)
}

#remove islands that don't have at least 3 strata in each time point
st.sub<-st[!(st$ISLAND %in% c("French Frigate","Niihau","Swains","Alamagan","Sarigan","Aguijan")),]

nwhi<-GenerateMDS_Island_spider(st.sub,"NWHI","Northwestern Hawaiian Islands")
mhi<-GenerateMDS_Island_spider(st.sub,"MHI","Main Hawaiian Islands")
phoneix<-GenerateMDS_Island_spider(st.sub,"PHOENIX","Phoenix Islands")
line<-GenerateMDS_Island_spider(st.sub,"LINE","Line Islands")
samoa<-GenerateMDS_Island_spider(st.sub,"SAMOA","American Samoa")
wake<-GenerateMDS_Island_spider(st.sub,"WAKE","Wake")
nmar<-GenerateMDS_Island_spider(st.sub,"NMARIAN","Northern Marianas")
smar<-GenerateMDS_Island_spider(st.sub,"SMARIAN","Southern Marianas")



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


#Plot nMDS trajectories (segments with arrows)
GenerateMDS_Trajectory<-function(df,region_field,region_name){
  
  #Subset Region of interest
  df<-subset(df,REGION==region_field)
  
  #generate the community matrix and metadata df
  gen.matrix<-Generate_Community_matrix(df)
  meta<-Generate_metadata(df)
  
  #calculate distance for NMDS
  mds <- metaMDS(gen.matrix)
  stress.value<-round(mds$stress,3) #extract stress value to add to plot later
  stress.value<-paste("(Stress = ",stress.value,")")
  
  data.scores<- as.data.frame(scores(mds))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
  data.scores<-cbind(data.scores,meta)

  species.scores <- as.data.frame(scores(mds, "species"))  #Using the scores function from vegan to extract the genus scores and convert to a data.frame ("species" = genus, we are forced to choose species or site)
  species.scores$GENUS_CODE <- rownames(species.scores)  # create a column of genus, from the rownames of species.scores
  
  #Create dataframes of NMDS scores for time point 1 and time point 2 
  scoreT1<-subset(data.scores,ANALYSIS_YEAR<=2016 & REGION!="NWHI")
  scoreT1nwhi<-subset(data.scores,ANALYSIS_YEAR==2014 & REGION=="NWHI") #because NWHI has 3 time points
  scoreT2<-subset(data.scores,ANALYSIS_YEAR>2016 & REGION!="NWHI")
  scoreT2nwhi<-subset(data.scores,ANALYSIS_YEAR==2016 & REGION=="NWHI")
  
  #combine T1 dfs for all regions & rename columns
  scoreT1<-rbind(scoreT1,scoreT1nwhi) 
  scoreT1<-subset(scoreT1,select = -c(ANALYSIS_YEAR))
  names(scoreT1)[names(scoreT1) == "NMDS1"] <- "T1_NMDS1";names(scoreT1)[names(scoreT1) == "NMDS2"] <- "T1_NMDS2"
  scoreT2<-rbind(scoreT2,scoreT2nwhi)
  scoreT2<-subset(scoreT2,select = -c(ANALYSIS_YEAR))
  names(scoreT2)[names(scoreT2) == "NMDS1"] <- "T2_NMDS1";names(scoreT2)[names(scoreT2) == "NMDS2"] <- "T2_NMDS2"
  
  new.score<-left_join(scoreT1,scoreT2);head(new.score) #combine T1 and T2 dfs
  
  #Convert year to factor
  data.scores$ANALYSIS_YEAR<-as.factor(data.scores$ANALYSIS_YEAR)
  
  title<-paste(region_name,stress.value,sep = " ") #Create plot title with region name and NMDS stress value
  
  #Plot strata-level data with arrows for the trajectory of each stratum between time points
    p<-ggplot(data.scores,aes(x=NMDS1,y=NMDS2)) + 
    geom_segment(data=new.score,mapping = aes(x=T1_NMDS1,xend=T2_NMDS1,y=T1_NMDS2,yend=T2_NMDS2,color=DEPTH_BIN)
                 ,arrow=arrow(length=unit(0.3,"cm"),type="closed")) + # add arrows
    geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=GENUS_CODE),size=2, alpha=0.5) +  # add the species labels
    coord_equal() + #standardizes plot dimensions
    theme_bw() + 
    theme(axis.text.x = element_blank(),  # remove x-axis text
          axis.text.y = element_blank(), # remove y-axis text
          axis.ticks = element_blank(),  # remove axis ticks
          panel.background = element_blank(), #remove background
          panel.grid.major = element_blank(),  #remove major-grid labels
          panel.grid.minor = element_blank(),  #remove minor-grid labels
          plot.background = element_blank(),
          legend.title = element_blank())+ #remove legend title
    ggtitle(title) #add plot title
  
  return(p)
}

nwhi<-GenerateMDS_Trajectory(st,"NWHI","Northwestern Hawaiian Islands")
mhi<-GenerateMDS_Trajectory(st,"MHI","Main Hawaiian Islands")
phoneix<-GenerateMDS_Trajectory(st,"PHOENIX","Phoenix Islands")
line<-GenerateMDS_Trajectory(st,"LINE","Line Islands")
samoa<-GenerateMDS_Trajectory(st,"SAMOA","American Samoa")
wake<-GenerateMDS_Trajectory(st,"WAKE","Wake")
nmar<-GenerateMDS_Trajectory(st,"NMARIAN","Northern Marianas")
smar<-GenerateMDS_Trajectory(st,"SMARIAN","Southern Marianas")

allplots<-grid.arrange(nwhi,mhi,phoneix,line,wake,samoa,nmar,smar,nrow=3,ncol=3)
ggsave(plot=allplots,file="T:/Benthic/Projects/Juvenile Project/Figures/Juv_Trajectory_DB_nMDS.png",width=12,height=9)

