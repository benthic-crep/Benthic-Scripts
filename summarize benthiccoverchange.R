df<-read.csv("T:/Benthic/Data/REA Coral Demography & Cover/Summary Data/Island/BenthicCover_2010-2019_Tier1_ISLAND_forTimesSeries.csv")



ggplot(data = subset(df,REGION=="SAMOA"), aes(x = ANALYSIS_YEAR, y = Mean.CORAL,color=ISLAND,group=ISLAND)) +
  geom_point(size = 2.5, shape = 19) +
  geom_path(size=1)+
  theme_bw()+
  ggtitle("% Coral Cover")
  

ggplot(data = subset(df,REGION=="SAMOA"), aes(x = ANALYSIS_YEAR, y = Mean.CCA,color=ISLAND,group=ISLAND)) +
  geom_point(size = 2.5, shape = 17) +
  geom_path(size=1, linetype="dashed")+
  theme_bw()+
  
  ggtitle("% CCA Cover")


ggplot(data = subset(df,REGION=="PRIAs"), aes(x = ANALYSIS_YEAR, y = Mean.CORAL,color=ISLAND,group=ISLAND)) +
  geom_point(size = 2.5, shape = 19) +
  geom_path(size=1)+
  theme_bw()+
  ggtitle("% Coral Cover")


ggplot(data = subset(df,REGION=="PRIAs"), aes(x = ANALYSIS_YEAR, y = Mean.CCA,color=ISLAND,group=ISLAND)) +
  geom_point(size = 2.5, shape = 17) +
  geom_path(size=1, linetype="dashed")+
  ggtitle("% CCA Cover")
