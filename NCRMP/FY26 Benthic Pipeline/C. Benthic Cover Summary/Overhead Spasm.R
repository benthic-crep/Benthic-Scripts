MaxRate=.99
OH=data.frame(Overhead=seq(0,MaxRate,length.out=MaxRate*100+1),DirectCost=100)
OH=OH %>% mutate(Total_Indirect=DirectCost+DirectCost*Overhead,
                 Total_PctTotal=DirectCost/(1-Overhead),
                 Indirect=Total_Indirect-DirectCost,
                 PctTotal=Total_PctTotal-DirectCost) %>% pivot_longer(cols=c(Indirect,PctTotal),names_to = "Scenario",values_to = "Indirect_Cost")


cols <- c("Percent Of Total" = "red", "Indirect Rate" = "blue")
ggplot(OH,aes(x=100*Overhead))+
  geom_vline(xintercept = 100*c(.16,.42),lty=3,size=2)+
  geom_line(aes(y=Indirect_Cost,color=Scenario),size=3)+
  ylab("'Overhead' Cost, for Direct Cost of $100K, Under Two Distinct Scenarios (1000s $)")+
  xlab("Nominal 'Overhead' Rate, Under Two Indirect Cost Caluclation Scenarios")+theme_bw()+
  scale_color_discrete(labels=c("Indirect Cost","Percent of Total"))+ggtitle("Scale of 'Overhead' Cost Differences, Indirect vs Percent of Total\nFor 16% Task 1 vs 42% Federal")

OH %>% filter(Overhead==.16)
OH %>% filter(Overhead==.42)
