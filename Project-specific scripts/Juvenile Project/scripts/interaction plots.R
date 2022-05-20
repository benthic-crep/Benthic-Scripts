new.df$YScat<-ifelse(new.df$YearSinceDHW4<5,"<5 Years Since Heat Stress Event",">5 Years Since Heat Stress Event")


#r <- subset(new.df,YearSinceDHW4<5)
newdata<-new.df
newdata$TRANSECTAREA_j <- 1 #Need to keep survey area constant
newdata$scaled_CORAL <- mean(newdata$scaled_CORAL)
newdata$scaled_CoralSec_A <- mean(newdata$scaled_CoralSec_A)
newdata$scaled_SAND_RUB <- mean(newdata$scaled_SAND_RUB)
newdata$scaled_Depth_Median<- mean(newdata$scaled_Depth_Median)
newdata$scaled_logHumanDen <- mean(newdata$scaled_logHumanDen)
newdata$scaled_CVsst <- mean(newdata$scaled_CVsst)
newdata$scaled_MeanDHW10<-seq(min(newdata$scaled_MeanDHW10),max(newdata$scaled_MeanDHW10),
                              by=round(rg(newdata$scaled_MeanDHW10),5)/nrow(newdata))
newdata$scaled_YearSinceDHW4<-mean(newdata$scaled_YearSinceDHW4)

PredictplotI(best.mod,newdata,"YScat","MeanDHW10","scaled_MeanDHW10","Heat Stress", 2)

o <- subset(new.df,YearSinceDHW4>=5)
newdata<-o
newdata$TRANSECTAREA_j <- 1 #Need to keep survey area constant
newdata$scaled_CORAL <- mean(o$scaled_CORAL)
newdata$scaled_CoralSec_A <- mean(o$scaled_CoralSec_A)
newdata$scaled_SAND_RUB <- mean(o$scaled_SAND_RUB)
newdata$scaled_Depth_Median<- mean(o$scaled_Depth_Median)
newdata$scaled_logHumanDen <- mean(o$scaled_logHumanDen)
newdata$scaled_CVsst <- mean(o$scaled_CVsst)
newdata$scaled_MeanDHW10<-seq(min(o$scaled_MeanDHW10),max(o$scaled_MeanDHW10),
                              by=round(rg(o$scaled_MeanDHW10),5)/nrow(o))
newdata$scaled_YearSinceDHW4<-mean(o$scaled_YearSinceDHW4)

olddhw.plot<-Predictplot(best.mod,newdata,"MeanDHW10","scaled_MeanDHW10","Heat Stress","#009E73", 0.5)+
  ggtitle(">5 Years Since Heat Stress Event")
olddhw.plot



# function to predict bleaching and create a plot based on variable of interest
PredictplotI <- function(mod, dat,Icat="YScat",us_pred="CORAL",predictor="scaled_CORAL", predictor_name,bks=2){
  dat$s_X<-dat[,predictor]
  
  p <- predict(mod, newdata = dat, type = "response",se.fit=TRUE)
  p<-as.data.frame(p)
  colnames(p)<-c("Predicted_Juv","SE_Juv")
  dat<-cbind(dat,p)
  dat$Predict.lwr <- dat$Predicted_Juv - 1.96 * dat$SE_Juv # confidence interval upper bound
  dat$Predict.upr <- dat$Predicted_Juv + 1.96 * dat$SE_Juv # confidence interval lower bound
  head(dat)
  
  dat$X<-dat[,us_pred]
  mx_val<-max(dat$X, na.rm = TRUE)
  
  
  #Unscaling predictor to plot on x axis
  att <- attributes(scale(dat$X))
  mylabels <- seq(0,mx_val,bks)
  mybreaks <- scale(mylabels, att$`scaled:center`, att$`scaled:scale`)[,1]
  
  # set color for line
  #pcol<-ifelse(sum.co$Sig == "p>0.05","black","#3399FF")
  
  
  #Try mapping geom_rug(dat2,aes(x=s_X)) - dat2= new.df
  
  #Plot
  plot1<-ggplot(dat, aes(x = s_X, y = Predicted_Juv,color=Icat)) + 
    geom_line(size=1) +
    geom_ribbon(data = dat,
                aes(ymin = Predict.lwr, ymax = Predict.upr,fill=Icat),
                alpha = 0.1)
    #geom_rug() +
    # theme_bw() +
    # theme(
    #   axis.title.y = element_blank(),
    #   axis.title = element_text(face = "bold"),
    #   text = element_text(size = 12),
    #   panel.grid = element_blank()
    # ) +
    # ylab("Predicted Juvenile Abudance") +
    # xlab(predictor_name)  +
    # scale_x_continuous(labels = mylabels,breaks=mybreaks) #This isn't working
  
  return(plot1)
  
}
