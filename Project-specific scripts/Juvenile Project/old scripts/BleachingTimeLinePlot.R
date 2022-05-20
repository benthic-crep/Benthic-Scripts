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

