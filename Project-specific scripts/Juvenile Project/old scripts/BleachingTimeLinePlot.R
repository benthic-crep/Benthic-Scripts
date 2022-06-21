#https://stackoverflow.com/questions/7492274/draw-a-chronological-timeline-with-ggplot2

library(ggplot2)
library(dplyr)
library(ggalt)
library(cowplot)
library(tibble)
library(lubridate)
library(ggrepel)

#Create data to plot
data<-tribble( ~peak_date, ~REGION, ~displ,~type,
               ymd(20130915), "MARIAN", 0.7,"Bleaching Event",
               ymd(20140825), "MARIAN", 0.5,"Bleaching Event",
               ymd(20160830), "MARIAN", 0.5,"Bleaching Event",
               ymd(20170830), "MARIAN", 0.9,"Bleaching Event",
               ymd(20141015), "MHI", 0.3,"Bleaching Event",
               ymd(20151020), "MHI", 0.9,"Bleaching Event",
               ymd(20141001), "NWHI", 0.7,"Bleaching Event",
               ymd(20151030), "LINE", 0.8,"Bleaching Event",
               ymd(20150315), "SAMOA", 0.6,"Bleaching Event",
               ymd(20130818), "MHI", 0.01,"RAMP Survey",
               ymd(20150416), "LINE",-0.01,"RAMP Survey",
               ymd(20150216), "PHOENIX",0.03,"RAMP Survey",
               ymd(20150817), "NWHI", 0.01,"RAMP Survey",
               ymd(20150315), "SAMOA", 0.01,"RAMP Survey",
               ymd(20160811), "MHI", 0.01,"RAMP Survey",
               ymd(20160920), "NWHI", 0.01,"RAMP Survey",
               ymd(20170609), "MARIAN", 0.01,"RAMP Survey",
               ymd(20180803), "LINE", -0.01,"RAMP Survey",
               ymd(20180603), "PHOENIX", 0.03,"RAMP Survey",
               ymd(20180708), "SAMOA", 0.01,"RAMP Survey")
head(data)

#Function to shift x-axis to 0 
shift_axis <- function(p, xmin, xmax, y=0){
  g <- ggplotGrob(p)
  dummy <- data.frame(y=y)
  ax <- g[["grobs"]][g$layout$name == "axis-b"][[1]]
  p + annotation_custom(grid::grobTree(ax, vp = grid::viewport(y=1, height=sum(ax$height))), 
                        ymax=y, ymin=y) +
    annotate("segment", y = 0, yend = 0, x = xmin, xend = xmax, 
             arrow = arrow(length = unit(0.1, "inches"))) +
    theme(axis.text.x = element_blank(), 
          axis.ticks.x=element_blank())
  
}



#Conditionally set whether text will be above or below the point
vjust = ifelse(data$displ > 0, -1, 1.5)

#plot
p1 <- data %>% 
  ggplot(aes(peak_date, displ,color=REGION,group=type)) +
  geom_lollipop(aes(shape=type),point.size = 4) +
  # geom_text(aes(x = peak_date, y = displ, label = REGION), data = data,
  #           hjust = 0, vjust = vjust, size = 2.5) +
  geom_text_repel(aes(x = peak_date, #this function prevents overlap of labels
                      y = displ, 
                      label = REGION),
                      data=data,vjust = vjust,size = 4,segment.color = NA)+
  theme_bw() +
  #ggtitle("2013-2017 Bleaching Event and Survey Timeline")+
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 12,face="bold"),
        legend.position = c(0.9,0.6),
        legend.text=element_text(size=12),
        legend.title = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(color="black", size=14, face="bold.italic"))+
  scale_shape_manual(values=c(16,4))+
  guides(color = FALSE,shape = guide_legend(override.aes = list(size = 4)))+ #remove region from legend &and increase size of symbols
  expand_limits(x = c(ymd(20130701), ymd(20190101)), y = 1.2) +
  scale_x_date(date_labels = "%b-%Y",breaks = scales::pretty_breaks(n = 9))

p1

#and run the function from above
timeline <- shift_axis(p1, ymd(20130701), ymd(20190101))
timeline

ggsave(plot=timeline,file="C:/Users/Courtney.S.Couch/Documents/Courtney's Files/R Files/ESD/Juvenile Project/Figures/BleachingTimeline.jpg",width=10,height=7)


dhw<-read.csv("T:/Benthic/Projects/Juvenile Project/Juvenile_PeakDHW.csv")

df<-read.csv("T:/Benthic/Projects/Juvenile Project/JuvDen_Pred_SITE_AllYears.csv")#Combined juvenile delta density and all predictors

jmeta<-df[,c("REGION","ISLAND","SITE")]

jmeta$REGION<-ifelse(jmeta$ISLAND %in% c("Maug", "Asuncion", "Alamagan", "Pagan", "Agrihan", "Guguan", "Sarigan","Farallon_de_Pajaros")
                              ,"NMI", as.character(jmeta$REGION))
jmeta$REGION<-ifelse(jmeta$ISLAND %in% c("Saipan", "Tinian", "Aguijan", "Rota", "Guam")
                              ,"SMI", as.character(jmeta$REGION))

dhw<-left_join(dhw,jmeta)

dhw <-dhw %>%
  filter(!is.na(REGION))

dhw$PeakDate<-lubridate::ymd(dhw$PeakDate)

test<-dhw %>%
  group_by(OBS_YEAR,ISLAND) %>%
  slice(which.max(PeakDHW))

Peak.DHW<-dhw %>%
  group_by(REGION,OBS_YEAR) %>%
  summarize(PeakDate=mean(PeakDate,na.rm=T),
            PeakDHW=mean(PeakDHW,na.rm=T))

Peak.DHW$R_Y<-paste(Peak.DHW$REGION,Peak.DHW$OBS_YEAR,sep = "_")

targetRY<-c("LINE_2015","PHOENIX_2015","MHI_2015","NMI_2013","NMI_2014","NMI_2016","NMI_2017",
            "sMI_2013","SMI_2014","SMI_2016","SMI_2017","WAKE_2014","WAKE_2015","SAMOA_2015","SAMOA_2017","NWHI_2014")

Peak.DHW<-filter(Peak.DHW,R_Y %in% targetRY)
Peak.DHW$type<-"Heat Stress Event"

Peak.DHW<-subset(Peak.DHW,select = c(PeakDate,REGION,PeakDHW,type))

#Create data to plot
data<-tribble( ~PeakDate, ~REGION, ~PeakDHW,~type,
               ymd(20130818), "MHI", 0.1,"Survey",
               ymd(20150416), "LINE",-0.1,"Survey",
               ymd(20150216), "PHOENIX",0.3,"Survey",
               ymd(20150817), "NWHI", 0.1,"Survey",
               ymd(20150315), "SAMOA", 0.1,"Survey",
               ymd(20160811), "MHI", 0.1,"Survey",
               ymd(20160920), "NWHI", 0.1,"Survey",
               ymd(20170609), "MARIAN", 0.1,"Survey",
               ymd(20180803), "LINE", -0.1,"Survey",
               ymd(20180603), "PHOENIX", 0.3,"Survey",
               ymd(20180708), "SAMOA", 0.1,"Survey")
head(data)

all.data<-rbind(Peak.DHW,data)

#Function to shift x-axis to 0 
shift_axis <- function(p, xmin, xmax, y=0){
  g <- ggplotGrob(p)
  dummy <- data.frame(y=y)
  ax <- g[["grobs"]][g$layout$name == "axis-b"][[1]]
  p + annotation_custom(grid::grobTree(ax, vp = grid::viewport(y=1, height=sum(ax$height))), 
                        ymax=y, ymin=y) +
    annotate("segment", y = 0, yend = 0, x = xmin, xend = xmax, 
             arrow = arrow(length = unit(0.1, "inches"))) +
    theme(axis.text.x = element_blank(), 
          axis.ticks.x=element_blank())
  
}



#Conditionally set whether text will be above or below the point
vjust = ifelse(all.data$PeakDHW > 0, -1, 15)

#plot
p1 <- all.data %>% 
  ggplot(aes(PeakDate, PeakDHW,color=REGION,group=type)) +
  geom_lollipop(aes(shape=type),point.size = 4) +
  # geom_text(aes(x = peak_date, y = displ, label = REGION), data = data,
  #           hjust = 0, vjust = vjust, size = 2.5) +
  geom_text_repel(aes(x = PeakDate, #this function prevents overlap of labels
                      y = PeakDHW, 
                      label = REGION),
                  data=all.data,vjust = vjust,size = 4,segment.color = NA)+
  theme_bw() +
  #ggtitle("2013-2017 Bleaching Event and Survey Timeline")+
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 12,face="bold"),
        legend.position = c(0.9,0.6),
        legend.text=element_text(size=12),
        legend.title = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(color="black", size=14, face="bold.italic"))+
  scale_shape_manual(values=c(16,4))+
  guides(color = FALSE,shape = guide_legend(override.aes = list(size = 4)))+ #remove region from legend &and increase size of symbols
  expand_limits(x = c(ymd(20130101), ymd(20191201)), y = 1.2) +
  scale_x_date(date_labels = "%b-%Y",breaks = scales::pretty_breaks(n = 9))

p1

#and run the function from above
timeline <- shift_axis(p1, ymd(20130701), ymd(20190101))
timeline

