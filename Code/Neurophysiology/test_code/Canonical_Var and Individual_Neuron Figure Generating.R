png(file="/Users/nickhousley/Desktop/Papers/Housley et al 2018 Nature Medicine/Neurophysiology/CanonicaltreatmentgroupsIaThr.L.png",res = 600, width = 8000, height =  2000, bg = "transparent")


treatmentgroups<-ggplot(data = DataIIStandardized, 
                        aes(x=as.factor(treatment), 
                            y= Dyn.spkNum3, 
                            #colour=interaction(autoClass,as.factor(treatment))))+ 
                            fill=as.factor(treatment)))+
  geom_boxplot(outlier.shape = NA, position = "dodge",varwidth = TRUE)+
  geom_point(alpha = 0.3, size = 3, position = position_jitter(w = 0.1, h = 0))+
  scale_fill_manual(values=c("#79AA55","#5586AA","#BA575F","#8655AA"),"Treatment")+
  
  theme(legend.justification=c(1,1), legend.position=c(1,1), legend.box.just = c("top"), legend.background = element_rect(fill=alpha(0.4)), legend.direction = "horizontal",legend.box="horizontal")+
  xlab("Treatment")+
  ggtitle("Dyn.spkNum3")+
  #  ylim(-2,4)+
  #theme(panel.background = element_blank())+
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
        legend.position="none")+
 # stat_compare_means(method = "anova", label.y = 10)+ # Add pairwise comparisons p-value
  #  stat_compare_means(label.y = 3)+     # Add global p-value
 stat_compare_means(comparisons = my_comparisons, label = "p.signif", method = "t.test",
                    ref.group = ".all.") 

afferentBYtreatment<-ggplot(data = DataIIStandardized, 
                            aes(x=as.factor(autoClass), 
                                y= Thr.L, 
                                #colour=interaction(autoClass,as.factor(treatment))))+ 
                                fill=as.factor(treatment)))+
  geom_boxplot(outlier.shape = NA, position = "dodge",varwidth = TRUE)+
  #geom_boxplot(outlier.shape = NA, varwidth = TRUE)+
  geom_point(alpha = 0.3, size = 3, position = position_jitter(w = 0.3, h = 0))+
  scale_fill_manual(values=c("#79AA55","#5586AA","#BA575F","#8655AA"),"Treatment")+
  
  theme(legend.justification=c(1,1), legend.position=c(1,1), legend.box.just = c("top"), legend.background = element_rect(fill=alpha(0.4)), legend.direction = "horizontal",legend.box="horizontal")+
  xlab("Neuron Clas")+
  ggtitle("Thr.L")+
  #  ylim(-2,4)+
  #theme(panel.background = element_blank())+
  theme_transparent()+
  
  theme(axis.text.x = element_text(face="bold", color="black", 
                                   size=15, angle=0),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(), #element_text(face="bold", color="black", 
                                   #size=15, angle=0),
        axis.title.y = element_blank(),
        plot.title = element_text(face="bold", color="black", 
                                  size=15, angle=0),
        axis.title = element_text(face="bold", color="black", 
                                  size=15, angle=0),
        legend.position="none")



multiplot(treatmentgroups,afferentBYtreatment, cols=2)
dev.off()