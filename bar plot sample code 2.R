


hi19<-subset(seg.data,REGION=="MHI"& OBS_YEAR=="2019")

data.cols<-c("ISLAND","DIVER","alg_prev","ble_prev")
df.new<-subset(hi19,select = c("ISLAND","DIVER","alg_prev","ble_prev"))



df.sum<-ddply(hi19,.(ISLAND,DIVER),
              summarize,
              Mean.RD=mean(Ave.rd),
              SE.RD=std.error(Ave.rd))

plot<-ggplot(df.sum, aes(x=DIVER, y=Mean.RD, fill=ISLAND)) + 
  geom_bar(position="dodge",stat="identity") +
  geom_errorbar(aes(ymin=MAE_sc.mn-MAE_sc.se, ymax=MAE_sc.mn+MAE_sc.se),width=.15, position=position_dodge(.9)) + 
  facet_wrap(~ ISLAND) +
  ylab("Midpoint Scaled Mean Absolute Error")+
  ylim(c(0,.5))+
  xlab("Error Comparison")+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 0)
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,axis.ticks.x = element_blank() # no x axis ticks
    ,axis.title.x = element_text( vjust = -.0001) # adjust x axis to lower the same amount as the genus labels
    ,legend.position="none")+
  theme(axis.title.y = element_text(face="bold")
        ,axis.title.x = element_text(face="bold"))