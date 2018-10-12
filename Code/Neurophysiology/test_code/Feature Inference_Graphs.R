png(file="/Users/nickhousley/Desktop/Papers/Housley et al 2018 Nature Medicine/Neurophysiology/All Neurons/FeatureGraphsDyn.png",res = 600, width = 8000, height =  8000, bg = "transparent")


Dyn.pfr<-ggplot(data = DataAllRaw, 
                        aes(x=as.factor(treatment), 
                            y= Dyn.pfr, 
                            #colour=interaction(autoClass,as.factor(treatment))))+ 
                            fill=as.factor(treatment)))+
  geom_boxplot(outlier.shape = NA, position = "dodge",varwidth = TRUE)+
  geom_point(alpha = 0.3, size = 3 )+
  scale_fill_manual(values=c(paletteNature2018),"Treatment")+
  
  theme(legend.justification=c(1,1), legend.position=c(1,1), legend.box.just = c("top"), legend.background = element_rect(fill=alpha(0.4)), legend.direction = "horizontal",legend.box="horizontal")+
  xlab("Treatment")+
  ggtitle("Dyn.pfr")+
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
   stat_compare_means(comparisons = my_comparisons, label = "p.signif", method = "t.test",
   ref.group = ".all.") 


Dyn.IB<-ggplot(data = DataAllRaw, 
                 aes(x=as.factor(treatment), 
                     y= Dyn.IB, 
                     #colour=interaction(autoClass,as.factor(treatment))))+ 
                     fill=as.factor(treatment)))+
  geom_boxplot(outlier.shape = NA, position = "dodge",varwidth = TRUE)+
  geom_point(alpha = 0.3, size = 3 )+
  scale_fill_manual(values=c(paletteNature2018),"Treatment")+
  
  theme(legend.justification=c(1,1), legend.position=c(1,1), legend.box.just = c("top"), legend.background = element_rect(fill=alpha(0.4)), legend.direction = "horizontal",legend.box="horizontal")+
  xlab("Treatment")+
  ggtitle("Dyn.IB")+
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
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", method = "t.test",
                     ref.group = ".all.") 




Dyn.spkNum<-ggplot(data = DataAllRaw, 
                 aes(x=as.factor(treatment), 
                     y= Dyn.spkNum, 
                     #colour=interaction(autoClass,as.factor(treatment))))+ 
                     fill=as.factor(treatment)))+
  geom_boxplot(outlier.shape = NA, position = "dodge",varwidth = TRUE)+
  geom_point(alpha = 0.3, size = 3 )+
  scale_fill_manual(values=c(paletteNature2018),"Treatment")+
  
  theme(legend.justification=c(1,1), legend.position=c(1,1), legend.box.just = c("top"), legend.background = element_rect(fill=alpha(0.4)), legend.direction = "horizontal",legend.box="horizontal")+
  xlab("Treatment")+
  ggtitle("Dyn.spkNum")+
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
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", method = "t.test",
                     ref.group = ".all.") 




Dyn.DI<-ggplot(data = DataAllRaw, 
                 aes(x=as.factor(treatment), 
                     y= Dyn.DI, 
                     #colour=interaction(autoClass,as.factor(treatment))))+ 
                     fill=as.factor(treatment)))+
  geom_boxplot(outlier.shape = NA, position = "dodge",varwidth = TRUE)+
  geom_point(alpha = 0.3, size = 3 )+
  scale_fill_manual(values=c(paletteNature2018),"Treatment")+
  
  theme(legend.justification=c(1,1), legend.position=c(1,1), legend.box.just = c("top"), legend.background = element_rect(fill=alpha(0.4)), legend.direction = "horizontal",legend.box="horizontal")+
  xlab("Treatment")+
  ggtitle("Dyn.DI")+
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
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", method = "t.test",
                     ref.group = ".all.") 



Dyn.slp<-ggplot(data = DataAllRaw, 
                 aes(x=as.factor(treatment), 
                     y= Dyn.slp, 
                     #colour=interaction(autoClass,as.factor(treatment))))+ 
                     fill=as.factor(treatment)))+
  geom_boxplot(outlier.shape = NA, position = "dodge",varwidth = TRUE)+
  geom_point(alpha = 0.3, size = 3 )+
  scale_fill_manual(values=c(paletteNature2018),"Treatment")+
  
  theme(legend.justification=c(1,1), legend.position=c(1,1), legend.box.just = c("top"), legend.background = element_rect(fill=alpha(0.4)), legend.direction = "horizontal",legend.box="horizontal")+
  xlab("Treatment")+
  ggtitle("Dyn.slp")+
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
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", method = "t.test",
                     ref.group = ".all.") 


Dyn.F<-ggplot(data = DataAllRaw, 
                 aes(x=as.factor(treatment), 
                     y= Dyn.F, 
                     #colour=interaction(autoClass,as.factor(treatment))))+ 
                     fill=as.factor(treatment)))+
  geom_boxplot(outlier.shape = NA, position = "dodge",varwidth = TRUE)+
  geom_point(alpha = 0.3, size = 3 )+
  scale_fill_manual(values=c(paletteNature2018),"Treatment")+
  
  theme(legend.justification=c(1,1), legend.position=c(1,1), legend.box.just = c("top"), legend.background = element_rect(fill=alpha(0.4)), legend.direction = "horizontal",legend.box="horizontal")+
  xlab("Treatment")+
  ggtitle("Dyn.F")+
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
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", method = "t.test",
                     ref.group = ".all.") 


Dyn.pfr1<-ggplot(data = DataAllRaw, 
                 aes(x=as.factor(treatment), 
                     y= Dyn.pfr1, 
                     #colour=interaction(autoClass,as.factor(treatment))))+ 
                     fill=as.factor(treatment)))+
  geom_boxplot(outlier.shape = NA, position = "dodge",varwidth = TRUE)+
  geom_point(alpha = 0.3, size = 3 )+
  scale_fill_manual(values=c(paletteNature2018),"Treatment")+
  
  theme(legend.justification=c(1,1), legend.position=c(1,1), legend.box.just = c("top"), legend.background = element_rect(fill=alpha(0.4)), legend.direction = "horizontal",legend.box="horizontal")+
  xlab("Treatment")+
  ggtitle("Dyn.pfr1")+
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
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", method = "t.test",
                     ref.group = ".all.") 


Dyn.pfr3<-ggplot(data = DataAllRaw, 
                 aes(x=as.factor(treatment), 
                     y= Dyn.pfr3, 
                     #colour=interaction(autoClass,as.factor(treatment))))+ 
                     fill=as.factor(treatment)))+
  geom_boxplot(outlier.shape = NA, position = "dodge",varwidth = TRUE)+
  geom_point(alpha = 0.3, size = 3 )+
  scale_fill_manual(values=c(paletteNature2018),"Treatment")+
  
  theme(legend.justification=c(1,1), legend.position=c(1,1), legend.box.just = c("top"), legend.background = element_rect(fill=alpha(0.4)), legend.direction = "horizontal",legend.box="horizontal")+
  xlab("Treatment")+
  ggtitle("Dyn.pfr3")+
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
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", method = "t.test",
                     ref.group = ".all.") 



Dyn.slp1<-ggplot(data = DataAllRaw, 
                 aes(x=as.factor(treatment), 
                     y= Dyn.slp1, 
                     #colour=interaction(autoClass,as.factor(treatment))))+ 
                     fill=as.factor(treatment)))+
  geom_boxplot(outlier.shape = NA, position = "dodge",varwidth = TRUE)+
  geom_point(alpha = 0.3, size = 3 )+
  scale_fill_manual(values=c(paletteNature2018),"Treatment")+
  
  theme(legend.justification=c(1,1), legend.position=c(1,1), legend.box.just = c("top"), legend.background = element_rect(fill=alpha(0.4)), legend.direction = "horizontal",legend.box="horizontal")+
  xlab("Treatment")+
  ggtitle("Dyn.slp1")+
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
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", method = "t.test",
                     ref.group = ".all.") 


Dyn.slp3<-ggplot(data = DataAllRaw, 
                 aes(x=as.factor(treatment), 
                     y= Dyn.slp3, 
                     #colour=interaction(autoClass,as.factor(treatment))))+ 
                     fill=as.factor(treatment)))+
  geom_boxplot(outlier.shape = NA, position = "dodge",varwidth = TRUE)+
  geom_point(alpha = 0.3, size = 3 )+
  scale_fill_manual(values=c(paletteNature2018),"Treatment")+
  
  theme(legend.justification=c(1,1), legend.position=c(1,1), legend.box.just = c("top"), legend.background = element_rect(fill=alpha(0.4)), legend.direction = "horizontal",legend.box="horizontal")+
  xlab("Treatment")+
  ggtitle("Dyn.slp3")+
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
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", method = "t.test",
                     ref.group = ".all.") 


Dyn.spkNum1<-ggplot(data = DataAllRaw, 
                 aes(x=as.factor(treatment), 
                     y= Dyn.spkNum1, 
                     #colour=interaction(autoClass,as.factor(treatment))))+ 
                     fill=as.factor(treatment)))+
  geom_boxplot(outlier.shape = NA, position = "dodge",varwidth = TRUE)+
  geom_point(alpha = 0.3, size = 3 )+
  scale_fill_manual(values=c(paletteNature2018),"Treatment")+
  
  theme(legend.justification=c(1,1), legend.position=c(1,1), legend.box.just = c("top"), legend.background = element_rect(fill=alpha(0.4)), legend.direction = "horizontal",legend.box="horizontal")+
  xlab("Treatment")+
  ggtitle("Dyn.spkNum1")+
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
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", method = "t.test",
                     ref.group = ".all.") 


Dyn.spkNum3<-ggplot(data = DataAllRaw, 
                 aes(x=as.factor(treatment), 
                     y= Dyn.spkNum3, 
                     #colour=interaction(autoClass,as.factor(treatment))))+ 
                     fill=as.factor(treatment)))+
  geom_boxplot(outlier.shape = NA, position = "dodge",varwidth = TRUE)+
  geom_point(alpha = 0.3, size = 3 )+
  scale_fill_manual(values=c(paletteNature2018),"Treatment")+
  
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
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", method = "t.test",
                     ref.group = ".all.") 


Dyn.RDR<-ggplot(data = DataAllRaw, 
                 aes(x=as.factor(treatment), 
                     y= Dyn.RDR, 
                     #colour=interaction(autoClass,as.factor(treatment))))+ 
                     fill=as.factor(treatment)))+
  geom_boxplot(outlier.shape = NA, position = "dodge",varwidth = TRUE)+
  geom_point(alpha = 0.3, size = 3 )+
  scale_fill_manual(values=c(paletteNature2018),"Treatment")+
  
  theme(legend.justification=c(1,1), legend.position=c(1,1), legend.box.just = c("top"), legend.background = element_rect(fill=alpha(0.4)), legend.direction = "horizontal",legend.box="horizontal")+
  xlab("Treatment")+
  ggtitle("Dyn.RDR")+
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
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", method = "t.test",
                     ref.group = ".all.") 


Dyn.IFRdrop<-ggplot(data = DataAllRaw, 
                 aes(x=as.factor(treatment), 
                     y= Dyn.IFRdrop, 
                     #colour=interaction(autoClass,as.factor(treatment))))+ 
                     fill=as.factor(treatment)))+
  geom_boxplot(outlier.shape = NA, position = "dodge",varwidth = TRUE)+
  geom_point(alpha = 0.3, size = 3 )+
  scale_fill_manual(values=c(paletteNature2018),"Treatment")+
  
  theme(legend.justification=c(1,1), legend.position=c(1,1), legend.box.just = c("top"), legend.background = element_rect(fill=alpha(0.4)), legend.direction = "horizontal",legend.box="horizontal")+
  xlab("Treatment")+
  ggtitle("Dyn.IFRdrop")+
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
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", method = "t.test",
                     ref.group = ".all.") 


multiplot(Dyn.pfr,	Dyn.IB,		Dyn.spkNum,		Dyn.DI,	Dyn.slp,	 Dyn.F,	Dyn.pfr1,	Dyn.pfr3,	Dyn.slp1,	Dyn.slp3,	Dyn.spkNum1,	Dyn.spkNum3,	Dyn.RDR	,Dyn.IFRdrop, cols=4)
dev.off()





png(file="/Users/nickhousley/Desktop/Papers/Housley et al 2018 Nature Medicine/Neurophysiology/All Neurons/FeatureGraphsStat.png",res = 600, width = 8000, height =  8000, bg = "transparent")

Stat.mSfr<-ggplot(data = DataAllRaw, 
                  aes(x=as.factor(treatment), 
                      y= Stat.mSfr, 
                      #colour=interaction(autoClass,as.factor(treatment))))+ 
                      fill=as.factor(treatment)))+
  geom_boxplot(outlier.shape = NA, position = "dodge",varwidth = TRUE)+
  geom_point(alpha = 0.3, size = 3 )+
  scale_fill_manual(values=c(paletteNature2018),"Treatment")+
  
  theme(legend.justification=c(1,1), legend.position=c(1,1), legend.box.just = c("top"), legend.background = element_rect(fill=alpha(0.4)), legend.direction = "horizontal",legend.box="horizontal")+
  xlab("Treatment")+
  ggtitle("Stat.mSfr")+
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
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", method = "t.test",
                     ref.group = ".all.") 


Stat.afr<-ggplot(data = DataAllRaw, 
                 aes(x=as.factor(treatment), 
                     y= Stat.afr, 
                     #colour=interaction(autoClass,as.factor(treatment))))+ 
                     fill=as.factor(treatment)))+
  geom_boxplot(outlier.shape = NA, position = "dodge",varwidth = TRUE)+
  geom_point(alpha = 0.3, size = 3 )+
  scale_fill_manual(values=c(paletteNature2018),"Treatment")+
  
  theme(legend.justification=c(1,1), legend.position=c(1,1), legend.box.just = c("top"), legend.background = element_rect(fill=alpha(0.4)), legend.direction = "horizontal",legend.box="horizontal")+
  xlab("Treatment")+
  ggtitle("Stat.afr")+
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
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", method = "t.test",
                     ref.group = ".all.") 


Stat.HzSTD<-ggplot(data = DataAllRaw, 
                   aes(x=as.factor(treatment), 
                       y= Stat.HzSTD, 
                       #colour=interaction(autoClass,as.factor(treatment))))+ 
                       fill=as.factor(treatment)))+
  geom_boxplot(outlier.shape = NA, position = "dodge",varwidth = TRUE)+
  geom_point(alpha = 0.3, size = 3 )+
  scale_fill_manual(values=c(paletteNature2018),"Treatment")+
  
  theme(legend.justification=c(1,1), legend.position=c(1,1), legend.box.just = c("top"), legend.background = element_rect(fill=alpha(0.4)), legend.direction = "horizontal",legend.box="horizontal")+
  xlab("Treatment")+
  ggtitle("Stat.HzSTD")+
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
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", method = "t.test",
                     ref.group = ".all.") 




Stat.HzEnd<-ggplot(data = DataAllRaw, 
                   aes(x=as.factor(treatment), 
                       y= Stat.HzEnd, 
                       #colour=interaction(autoClass,as.factor(treatment))))+ 
                       fill=as.factor(treatment)))+
  geom_boxplot(outlier.shape = NA, position = "dodge",varwidth = TRUE)+
  geom_point(alpha = 0.3, size = 3 )+
  scale_fill_manual(values=c(paletteNature2018),"Treatment")+
  
  theme(legend.justification=c(1,1), legend.position=c(1,1), legend.box.just = c("top"), legend.background = element_rect(fill=alpha(0.4)), legend.direction = "horizontal",legend.box="horizontal")+
  xlab("Treatment")+
  ggtitle("Stat.HzEnd")+
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
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", method = "t.test",
                     ref.group = ".all.") 



Stat.LSpkT<-ggplot(data = DataAllRaw, 
                   aes(x=as.factor(treatment), 
                       y= Stat.LSpkT, 
                       #colour=interaction(autoClass,as.factor(treatment))))+ 
                       fill=as.factor(treatment)))+
  geom_boxplot(outlier.shape = NA, position = "dodge",varwidth = TRUE)+
  geom_point(alpha = 0.3, size = 3 )+
  scale_fill_manual(values=c(paletteNature2018),"Treatment")+
  
  theme(legend.justification=c(1,1), legend.position=c(1,1), legend.box.just = c("top"), legend.background = element_rect(fill=alpha(0.4)), legend.direction = "horizontal",legend.box="horizontal")+
  xlab("Treatment")+
  ggtitle("Stat.LSpkT")+
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
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", method = "t.test",
                     ref.group = ".all.") 





Stat.LSpkF<-ggplot(data = DataAllRaw, 
                   aes(x=as.factor(treatment), 
                       y= Stat.LSpkF, 
                       #colour=interaction(autoClass,as.factor(treatment))))+ 
                       fill=as.factor(treatment)))+
  geom_boxplot(outlier.shape = NA, position = "dodge",varwidth = TRUE)+
  geom_point(alpha = 0.3, size = 3 )+
  scale_fill_manual(values=c(paletteNature2018),"Treatment")+
  
  theme(legend.justification=c(1,1), legend.position=c(1,1), legend.box.just = c("top"), legend.background = element_rect(fill=alpha(0.4)), legend.direction = "horizontal",legend.box="horizontal")+
  xlab("Treatment")+
  ggtitle("Stat.LSpkF")+
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
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", method = "t.test",
                     ref.group = ".all.") 



Stat.slp<-ggplot(data = DataAllRaw, 
                 aes(x=as.factor(treatment), 
                     y= Stat.slp, 
                     #colour=interaction(autoClass,as.factor(treatment))))+ 
                     fill=as.factor(treatment)))+
  geom_boxplot(outlier.shape = NA, position = "dodge",varwidth = TRUE)+
  geom_point(alpha = 0.3, size = 3 )+
  scale_fill_manual(values=c(paletteNature2018),"Treatment")+
  
  theme(legend.justification=c(1,1), legend.position=c(1,1), legend.box.just = c("top"), legend.background = element_rect(fill=alpha(0.4)), legend.direction = "horizontal",legend.box="horizontal")+
  xlab("Treatment")+
  ggtitle("Stat.slp")+
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
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", method = "t.test",
                     ref.group = ".all.") 





Stat.spkNum<-ggplot(data = DataAllRaw, 
                    aes(x=as.factor(treatment), 
                        y= Stat.spkNum, 
                        #colour=interaction(autoClass,as.factor(treatment))))+ 
                        fill=as.factor(treatment)))+
  geom_boxplot(outlier.shape = NA, position = "dodge",varwidth = TRUE)+
  geom_point(alpha = 0.3, size = 3 )+
  scale_fill_manual(values=c(paletteNature2018),"Treatment")+
  
  theme(legend.justification=c(1,1), legend.position=c(1,1), legend.box.just = c("top"), legend.background = element_rect(fill=alpha(0.4)), legend.direction = "horizontal",legend.box="horizontal")+
  xlab("Treatment")+
  ggtitle("Stat.spkNum")+
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
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", method = "t.test",
                     ref.group = ".all.") 

multiplot(	Stat.mSfr,	Stat.afr,	Stat.HzSTD,	Stat.HzEnd,	Stat.LSpkT,	Stat.LSpkF,	Stat.spkNum,		Stat.slp,  cols=4)
dev.off()







png(file="/Users/nickhousley/Desktop/Papers/Housley et al 2018 Nature Medicine/Neurophysiology/All Neurons/FeatureGraphsThr.png",res = 600, width = 8000, height =  6000, bg = "transparent")


Thr.T<-ggplot(data = DataAllRaw, 
              aes(x=as.factor(treatment), 
                  y= Thr.T, 
                  #colour=interaction(autoClass,as.factor(treatment))))+ 
                  fill=as.factor(treatment)))+
  geom_boxplot(outlier.shape = NA, position = "dodge",varwidth = TRUE)+
  geom_point(alpha = 0.3, size = 3 )+
  scale_fill_manual(values=c(paletteNature2018),"Treatment")+
  
  theme(legend.justification=c(1,1), legend.position=c(1,1), legend.box.just = c("top"), legend.background = element_rect(fill=alpha(0.4)), legend.direction = "horizontal",legend.box="horizontal")+
  xlab("Treatment")+
  ggtitle("Thr.T")+
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
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", method = "t.test",
                     ref.group = ".all.") 




Thr.F<-ggplot(data = DataAllRaw, 
              aes(x=as.factor(treatment), 
                  y= Thr.F, 
                  #colour=interaction(autoClass,as.factor(treatment))))+ 
                  fill=as.factor(treatment)))+
  geom_boxplot(outlier.shape = NA, position = "dodge",varwidth = TRUE)+
  geom_point(alpha = 0.3, size = 3 )+
  scale_fill_manual(values=c(paletteNature2018),"Treatment")+
  
  theme(legend.justification=c(1,1), legend.position=c(1,1), legend.box.just = c("top"), legend.background = element_rect(fill=alpha(0.4)), legend.direction = "horizontal",legend.box="horizontal")+
  xlab("Treatment")+
  ggtitle("Thr.F")+
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
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", method = "t.test",
                     ref.group = ".all.") 



Thr.L<-ggplot(data = DataAllRaw, 
              aes(x=as.factor(treatment), 
                  y= Thr.L, 
                  #colour=interaction(autoClass,as.factor(treatment))))+ 
                  fill=as.factor(treatment)))+
  geom_boxplot(outlier.shape = NA, position = "dodge",varwidth = TRUE)+
  geom_point(alpha = 0.3, size = 3 )+
  scale_fill_manual(values=c(paletteNature2018),"Treatment")+
  
  theme(legend.justification=c(1,1), legend.position=c(1,1), legend.box.just = c("top"), legend.background = element_rect(fill=alpha(0.4)), legend.direction = "horizontal",legend.box="horizontal")+
  xlab("Treatment")+
  ggtitle("Thr.L")+
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
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", method = "t.test",
                     ref.group = ".all.") 




Thr.T1<-ggplot(data = DataAllRaw, 
               aes(x=as.factor(treatment), 
                   y= Thr.T1, 
                   #colour=interaction(autoClass,as.factor(treatment))))+ 
                   fill=as.factor(treatment)))+
  geom_boxplot(outlier.shape = NA, position = "dodge",varwidth = TRUE)+
  geom_point(alpha = 0.3, size = 3 )+
  scale_fill_manual(values=c(paletteNature2018),"Treatment")+
  
  theme(legend.justification=c(1,1), legend.position=c(1,1), legend.box.just = c("top"), legend.background = element_rect(fill=alpha(0.4)), legend.direction = "horizontal",legend.box="horizontal")+
  xlab("Treatment")+
  ggtitle("Thr.T1")+
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
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", method = "t.test",
                     ref.group = ".all.") 



Thr.F1<-ggplot(data = DataAllRaw, 
               aes(x=as.factor(treatment), 
                   y= Thr.F1, 
                   #colour=interaction(autoClass,as.factor(treatment))))+ 
                   fill=as.factor(treatment)))+
  geom_boxplot(outlier.shape = NA, position = "dodge",varwidth = TRUE)+
  geom_point(alpha = 0.3, size = 3 )+
  scale_fill_manual(values=c(paletteNature2018),"Treatment")+
  
  theme(legend.justification=c(1,1), legend.position=c(1,1), legend.box.just = c("top"), legend.background = element_rect(fill=alpha(0.4)), legend.direction = "horizontal",legend.box="horizontal")+
  xlab("Treatment")+
  ggtitle("Thr.F1")+
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
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", method = "t.test",
                     ref.group = ".all.") 



Thr.L1<-ggplot(data = DataAllRaw, 
               aes(x=as.factor(treatment), 
                   y= Thr.L1, 
                   #colour=interaction(autoClass,as.factor(treatment))))+ 
                   fill=as.factor(treatment)))+
  geom_boxplot(outlier.shape = NA, position = "dodge",varwidth = TRUE)+
  geom_point(alpha = 0.3, size = 3 )+
  scale_fill_manual(values=c(paletteNature2018),"Treatment")+
  
  theme(legend.justification=c(1,1), legend.position=c(1,1), legend.box.just = c("top"), legend.background = element_rect(fill=alpha(0.4)), legend.direction = "horizontal",legend.box="horizontal")+
  xlab("Treatment")+
  ggtitle("Thr.L1")+
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
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", method = "t.test",
                     ref.group = ".all.") 


Thr.T3<-ggplot(data = DataAllRaw, 
               aes(x=as.factor(treatment), 
                   y= Thr.T3, 
                   #colour=interaction(autoClass,as.factor(treatment))))+ 
                   fill=as.factor(treatment)))+
  geom_boxplot(outlier.shape = NA, position = "dodge",varwidth = TRUE)+
  geom_point(alpha = 0.3, size = 3 )+
  scale_fill_manual(values=c(paletteNature2018),"Treatment")+
  
  theme(legend.justification=c(1,1), legend.position=c(1,1), legend.box.just = c("top"), legend.background = element_rect(fill=alpha(0.4)), legend.direction = "horizontal",legend.box="horizontal")+
  xlab("Treatment")+
  ggtitle("Thr.T3")+
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
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", method = "t.test",
                     ref.group = ".all.") 


Thr.F3<-ggplot(data = DataAllRaw, 
               aes(x=as.factor(treatment), 
                   y= Thr.F3, 
                   #colour=interaction(autoClass,as.factor(treatment))))+ 
                   fill=as.factor(treatment)))+
  geom_boxplot(outlier.shape = NA, position = "dodge",varwidth = TRUE)+
  geom_point(alpha = 0.3, size = 3 )+
  scale_fill_manual(values=c(paletteNature2018),"Treatment")+
  
  theme(legend.justification=c(1,1), legend.position=c(1,1), legend.box.just = c("top"), legend.background = element_rect(fill=alpha(0.4)), legend.direction = "horizontal",legend.box="horizontal")+
  xlab("Treatment")+
  ggtitle("Thr.F3")+
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
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", method = "t.test",
                     ref.group = ".all.") 


Thr.L3<-ggplot(data = DataAllRaw, 
               aes(x=as.factor(treatment), 
                   y= Thr.L3, 
                   #colour=interaction(autoClass,as.factor(treatment))))+ 
                   fill=as.factor(treatment)))+
  geom_boxplot(outlier.shape = NA, position = "dodge",varwidth = TRUE)+
  geom_point(alpha = 0.3, size = 3 )+
  scale_fill_manual(values=c(paletteNature2018),"Treatment")+
  
  theme(legend.justification=c(1,1), legend.position=c(1,1), legend.box.just = c("top"), legend.background = element_rect(fill=alpha(0.4)), legend.direction = "horizontal",legend.box="horizontal")+
  xlab("Treatment")+
  ggtitle("Thr.L3")+
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
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", method = "t.test",
                     ref.group = ".all.") 


multiplot(Thr.T,	Thr.F,	Thr.L,	Thr.T1,	Thr.F1,	Thr.L1,	Thr.T3,	Thr.F3,	Thr.L3, cols=5)
dev.off()