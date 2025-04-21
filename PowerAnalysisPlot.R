library(tidyverse)
pow=expand.grid(between.var=100*c(.001,.002,.005,.01,.02,.05,.1,.2),within.var=c(10,20,30))
pow=expand.grid(between.var=10^seq(0,1.3,.05),within.var=c(10,20,30))


for(i in 1:nrow(pow)){
pow$N[i]=power.anova.test(groups = 2,within.var=pow$within.var[i],between.var=pow$between.var[i],power=.8)$n
}

powplot=pow %>% ggplot(aes(x=N,y=between.var,color=factor(within.var),group=factor(within.var)))+
  geom_point()+geom_line()+
  xlab("Number of Replicates")+
  ylab("Dectectable Between-Group Effect Size")+
  scale_color_discrete(name="Variation Within Groups")+theme_bw()+
  ggtitle("How Many Replicates? Sampling Power Curves\n Assuming 80% chance of detecting real difference (at p= 0.05)")+xlim(c(0,250))+theme(legend.position = "bottom")
powplot
sc=.8
ggsave(filename = "C:/Users/Thomas.Oliver/Downloads/PowerAnalysis.png",plot = powplot,device = "png",width=sc*8,height=sc*6)
ggsave(filename = "C:/Users/Thomas.Oliver/Downloads/PowerAnalysis.pdf",plot = powplot,device = "pdf",width=sc*8,height=sc*6)
ggsave(filename = "C:/Users/Thomas.Oliver/Downloads/PowerAnalysis.eps",plot = powplot,device = "eps",width=sc*8,height=sc*6)
