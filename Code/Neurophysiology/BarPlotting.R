IaCanVar_Logistic$Cancer<-as.factor(IaCanVar_Logistic$Cancer)
IaCanVar_Logistic$treatment<-as.factor(IaCanVar_Logistic$treatment)
IaCanVar_Logistic$LD1

model <- glm(IaCanVar_Logistic$LD3 ~ Cancer*treatment, data=IaCanVar_Logistic)
summary(model)
12 pt
class(IaCanVar_Logistic$Cancer)


library(scales)

DataIaRaw$treatment<-as.factor(DataIaRaw$treatment)
DataIaRawSummary <- summarySE(DataIaRaw, measurevar="Dyn.pfr", groupvars=c("treatment"))



png(file="/Users/nickhousley/Desktop/Papers/Housley et al 2018 Nature Medicine/Neurophysiology/Ia Neurons/Dyn.pfr.png",res = 600, width = 4000, height =  6000, bg = "transparent")

ggplot(DataIaRawSummary, aes(x=treatment, y=Dyn.pfr, fill=treatment))+
  geom_bar(position = position_dodge(), stat = "identity", colour="black")+
  scale_y_continuous(limits = c(100,250),oob = rescale_none) +
  geom_errorbar(aes(ymin=Dyn.pfr-se, ymax=Dyn.pfr+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  scale_fill_manual(values=c(paletteNature2018),"Treatment")+
theme_transparent()+
  
  theme(axis.text.x = element_text(face="bold", color="black", 
                                   size=15, angle=0),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(face="bold", color="black", 
                                   size=15, angle=0),
        axis.title.y = element_blank(),
        plot.title = element_text(face="bold", color="black", 
                                  size=15, angle=0),
        axis.title = element_text(face="bold", color="black", 
                                  size=15, angle=0),
        legend.position="none")

  
dev.off()
  


       
