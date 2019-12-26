#rm(list=ls())
#setwd("/Users/ivor.williams/Documents/CRED/Fish Team/Papers/Papers In Prep/CoralNet/Samoa/Data Workings")

# # #library(gdata)             # needed for drop_levels()
# library(reshape)           # reshape library inclues the cast() function used below
# #library(grid)
# library(ggplot2)
# #library(ggthemes)
# #library(plyr)
# #library(scales)
# #library(RColorBrewer)
# library(cowplot)


# # ### Plotting Functions
# percentIvor=function (x, accuracy = NULL, scale = 100, prefix = "", suffix = "%", 
    # big.mark = " ", decimal.mark = ".", trim = TRUE, ...) 
# {
    # number(x = x, accuracy = accuracy, scale = scale, prefix = prefix, 
        # suffix = suffix, big.mark = big.mark, decimal.mark = decimal.mark, 
        # trim = trim, ...)
# }

#Calculate Root Mean Square Error
RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}

Plot1to1<-function(d, benth_field="AdColDen"){
	
	d$DIVER<-d[,benth_field]
	d$SFM<-d[,paste("a", benth_field, sep="")]
	mx_val<-max(d$DIVER, d$SFM, na.rm = T)
	
	# corr<-cor.test(d$DIVER, d$SFM, method="pearson")
	# r_text<-paste("r = ", round((corr$estimate),2), sep="")
	
	rmse<-RMSE(d$SFM, d$DIVER)
	r_text<-paste("RMSE = ", round(rmse), sep="")
	
	p1<-ggplot(d, aes(x=DIVER, y=SFM)) + 
 		geom_point(size=1) + 	geom_abline(slope=1, intercept=0) +
 		geom_smooth(method="lm", color="red", linetype="dashed", se=F) +
		geom_text(aes((mx_val/6), (mx_val * 0.9), label=r_text), size=4, color="red") +
 		# scale_x_continuous(labels=scales::percent_format(accuracy=1), limits=c(0,mx_val)) +
 		# scale_y_continuous(labels=scales::percent_format(accuracy=1),limits=c(0,mx_val)) +
		ylab("") +  xlab("")     
	return(p1)
} #Plot1to1

PlotBlaAlt<-function(d, benth_field="AdColDen", Y_EXP=0){
# d<-ad[ad$SCHEME=="ORIGINAL",]
# benth_field="CORAL"
	d$DIVER<-d[,benth_field]
	d$SFM<-d[,paste("a", benth_field, sep="")]

	d$AVE<-rowMeans(d[,c("DIVER", "SFM")])
	max_x<-max(d$AVE)
#	d<-subset(d, d$AVE>0.01)
	d$PROP_DIFF<-(d$SFM-d$DIVER)/d$AVE
	d$ABS_DIFF<-d$SFM-d$DIVER
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
 		expand_limits(y=c(-Y_EXP, Y_EXP)) +
 		# scale_x_continuous(labels=scales::percent_format(accuracy=1)) + 
 		# scale_y_continuous(labels=scales::percent_format(accuracy=1), breaks=pretty_breaks(5)) +
		ylab("") + xlab("")      
	return(p1)
} #PlotBlaAlt

PlotPair<-function(d, benth_field="AdColDen", y_exp=0){
	p1<-Plot1to1(d, benth_field)
	p2<-PlotBlaAlt(d, benth_field, y_exp)
	pX<-plot_grid(p1, p2)
	pOUT<-ggdraw() +
		draw_plot(pX) 
	return(pOUT)
} #PlotPair

#Plot histogram of diver vs. SfM
PDhist<-function(data,benthic_field="AdColDen"){
m=mean(data$P1P2d,na.rm=T);g21ci80r=1.28*sd(SiteComp$P1P2d,na.rm=T)/sqrt(nrow(SiteComp))
g21=ggplot(SiteComp,aes(P1P2d))+geom_histogram(bins=Nbins,fill="skyblue",color="black")+
  ggtitle(paste(TargetTaxa[i_t],": Raw Difference In Cover"))+xlim(-dmax,dmax)+
  annotate("rect", xmin = g21mn-g21ci80r, xmax = g21mn+g21ci80r, ymin = -Inf, ymax = Inf, fill = "yellow", alpha = 0.5) +
  geom_vline(xintercept=g21mn,color="blue",lty=2)+geom_vline(xintercept=0,color="black")+xlab("P1 Cover - P2 Cover")



