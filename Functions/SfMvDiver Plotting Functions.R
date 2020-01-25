#install.packages(c("cowplot", "hydroGOF", "reshape")) #for when packages don't load
library(gdata)             # needed for drop_levels()
library(reshape)           # reshape library inclues the cast() function used below
library(grid)
library(ggplot2)
#library(ggthemes)
library(plyr)
library(scales)
library(RColorBrewer)
library(cowplot)
library(hydroGOF)

#Create figure based on pearson correlation coefficient for linear relationship between two variables (i.e. diver1 vs diver 2)
#should we remove rows with NAs?
Plot1to1<-function(d,response_variable,predictor_variable){
	#sub<-d[d$taxon,]
	d$X<-d[,response_variable]
	d$Y<-d[,predictor_variable]
	mx_val<-max(d$Y, d$X, na.rm = TRUE)

	corr<-cor.test(d$X, d$Y, method="pearson")
	rmse<-rmse(d$Y, d$X,na.rm=TRUE)
	r_text<-paste("RMSE = ", round(rmse,digits = 2),"\n r = ", round((corr$estimate),2), sep="")
	
	p1<-ggplot(d, aes(x=X, y=Y)) + 
 		geom_point(size=1) + 	geom_abline(slope=1, intercept=0) +
 		geom_smooth(method="lm", color="red", linetype="dashed", se=F) +
		geom_text(aes((mx_val/5), (mx_val * 0.9), label=r_text), nudge_y=-0.1, nudge_x=0.1,size=4, color="red") +
	  theme_bw()+
	  scale_x_continuous(limits=c(0,mx_val)) +
	  scale_y_continuous(limits=c(0,mx_val)) +
 		
		ylab("") +  xlab("")     
	return(p1)
} # 

PlotBlaAlt<-function(d,response_variable,predictor_variable, Y_EXP=0){
# d<-ad[ad$SCHEME=="ORIGINAL",]
# benth_field="CORAL"
  d$X<-d[,response_variable]
  d$Y<-d[,predictor_variable]
  d<-subset(d,!(is.na(d["X"]) | is.na(d["Y"]))) #remove rows with NAs
  
	d$AVE<-rowMeans(d[,c("X", "Y")])
	max_x<-max(d$AVE)
#	d<-subset(d, d$AVE>0.01)
	d$PROP_DIFF<-(d$Y-d$X)/d$AVE
	d$ABS_DIFF<-d$Y-d$X
	m_diff=mean(d$ABS_DIFF)
	sd_diff=sd(d$ABS_DIFF)
	SD_SCALER<-1  #Could set this to 1.96 to convert SD to 95% CI
	diff_text<-paste("Diff = ", round((100*m_diff),1), "% ± ", round((100*sd_diff*SD_SCALER),1), "%", sep="")
	
	p1<-ggplot(d, aes(x=AVE, y=ABS_DIFF)) + 
 		geom_point(size=1) + geom_hline(yintercept=0) + 
# 		geom_smooth(method="lm") +
 		geom_hline(yintercept=m_diff, color="red") +
 		geom_hline(yintercept=m_diff+(SD_SCALER*sd_diff), color="blue", linetype = "dashed") +
 		geom_hline(yintercept=m_diff-(SD_SCALER*sd_diff), color="blue", linetype = "dashed") +
		geom_text(aes((max_x/2.5), 0.2, label=diff_text), size=4, color="red") +
 		expand_limits(y=c(-Y_EXP, Y_EXP)) +theme_bw()+
 		# scale_x_continuous(labels=scales::percent_format(accuracy=1)) + 
 		# scale_y_continuous(labels=scales::percent_format(accuracy=1), breaks=pretty_breaks(5)) +
		ylab("") + xlab("")      
	return(p1)
} #PlotBlaAlt


#Plot histogram 
PlotHIST<-function(d,response_variable,predictor_variable){
  Nbins=30
  d$X<-d[,response_variable]
  d$Y<-d[,predictor_variable]
  d<-subset(d,!(is.na(d["X"]) | is.na(d["Y"]))) #remove rows with NAs
  d$DIFF<-d$Y-d$X
  m=mean(d$DIFF,na.rm=T);SE=1.28*sd(d$DIFF,na.rm=T)/sqrt(nrow(d)) #why 1.28?
  
  p=ggplot(d,aes(DIFF))+geom_histogram(bins=Nbins,fill="skyblue",color="black")+
    annotate("rect", xmin = m-SE, xmax = m+SE, ymin = -Inf, ymax = Inf, fill = "yellow", alpha = 0.5) +
    theme_bw()+
    geom_vline(xintercept=m,color="Red",lty=2)+geom_vline(xintercept=0,color="black")+xlab("")+ylab("")
  return(p)
}#End Plot hist


#Create viewing panel for histogram and correlation figures
PlotPair<-function(d,response_variable,predictor_variable){
  p1<-Plot1to1(d,response_variable,predictor_variable)
  p2<-PlotHIST(d,response_variable,predictor_variable)
  pX<-plot_grid(p1, p2)
  pOUT<-ggdraw() +
    draw_plot(pX) 
  return(pOUT)
} #PlotPair


#Add specifications to the viewing panel
PlotAll<-function(d, response_variable,predictor_variable, Y1="title",Y2="title",X1="title",X2="title"){
  pO<-PlotPair(d[d$GENUS_CODE=="SSSS",], response_variable,predictor_variable)
  pS<-PlotPair(d[d$GENUS_CODE=="POSP",], response_variable,predictor_variable)
  pB<-PlotPair(d[d$GENUS_CODE=="MOSP",], response_variable,predictor_variable)
  pH<-PlotPair(d[d$GENUS_CODE=="POCS",], response_variable,predictor_variable)
  pOUT<-plot_grid(pO, pS, pB, pH, ncol=1, labels=c("SSSS", "POSP", "MOSP", "POCS"), label_x=c(0.02,0.02,0.02,0.02), align="v")
  pFIN<-ggdraw() +
    draw_plot(pOUT, x=0.01, y=0.01, 0.99, 0.99) +
    draw_label(Y1, x=0.02, y=.55, size = 16, angle=90, fontface="bold", colour="black") +
    draw_label(Y2, x=0.515, y=.55, size = 14, angle=90, fontface="bold", colour="black") + 
    draw_label(X1, x=0.30, y=.02, size = 14, fontface="bold", colour="black") + 
    draw_label(X2, x=0.80, y=.02, size = 14, fontface="bold", colour="black") 
  
  #ggsave(plot=pFIN,path=(outpath),width=12,height=12,filename=paste0(Y1, sep="v", X1, ".png"))
  
  return(pFIN)
} #end PlotALL




#I also played around with bland altman plots- don't really like them for these data, went with histograms

# 
# PlotPairwBA<-function(d,response_variable,predictor_variable, y_exp=0){
#   p1<-Plot1to1(d,response_variable,predictor_variable)
#   p2<-PlotBlaAlt(d,response_variable,predictor_variable, y_exp)
#   pX<-plot_grid(p1, p2)
#   pOUT<-ggdraw() +
#     draw_plot(pX) 
#   return(pOUT)
# } #PlotPair
# 
# PlotAllwBA<-function(d, response_variable,predictor_variable, Y1="title",Y2="title",X1="title",X2="title",y_exp=0){
#   pO<-PlotPair(d[d$GENUS_CODE=="SSSS",], response_variable,predictor_variable, y_exp)
#   pS<-PlotPair(d[d$GENUS_CODE=="POSP",], response_variable,predictor_variable, y_exp)
#   pB<-PlotPair(d[d$GENUS_CODE=="MOSP",], response_variable,predictor_variable, y_exp)
#   pH<-PlotPair(d[d$GENUS_CODE=="POCS",], response_variable,predictor_variable, y_exp)
#   pOUT<-plot_grid(pO, pS, pB, pH, ncol=1, labels=c("SSSS", "POSP", "MOSP", "POCS"), label_x=c(0.02,0.02,0.02,0.02), align="v")
#   pFIN<-ggdraw() +
#     draw_plot(pOUT, x=0.01, y=0.01, 0.99, 0.99) +
#     draw_label(Y1, x=0.02, y=.55, size = 16, angle=90, fontface="bold", colour="black") +
#     draw_label(Y2, x=0.515, y=.55, size = 14, angle=90, fontface="bold", colour="black") + 
#     draw_label(X1, x=0.30, y=.02, size = 14, fontface="bold", colour="black") + 
#     draw_label(X2, x=0.80, y=.02, size = 14, fontface="bold", colour="black") 
#   
#   ggsave(plot=pFIN,file=paste0(outpath,Y1,sep=" v ",X1,".png"),width=12,height=12)
#   
#   return(pFIN)
# } #end PlotALL
# 
