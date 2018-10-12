library(FactoMineR)
library(factoextra)
library(missMDA)
library(corrplot)
library(MASS)
library(scatterplot3d)
library(devtools)
library(ggplot2)
library(pca3d)
library(ggord)
library(parallel)
library(devtools)
library(ggpubr)

########################    All Neurons       ##################################

##load in data
allData<-read.csv(file="/Users/nickhousley/Desktop/Papers/Housley et al 2018 Nature Medicine/Neurophysiology/Data/All Afferents/231_afferents_MasterSheet.csv", header=TRUE, sep=",", check.names = FALSE) # read in data and filter
head(allData)
# DataRampsStandardized<-allData[,c(2,5,47:62)]
#allDataFiltered<-allDataFiltered[,c(-10)]
# head(DataRampsStandardized)
# dataRampAll.lda<- lda(treatment ~ Dyn.pfr + 
#                   Stat.mSfr + 
#                   Stat.afr + 
#                   Stat.HzSTD + 
#                   Stat.HzEnd + 
#                   Dyn.IB+ 
#                   Thr.T + 
#                   Thr.F + 
#                   Thr.L + 
#                   Stat.LSpkT + 
#                   Stat.LSpkF + 
#                   Dyn.spkNum + 
#                   Stat.spkNum + 
#                   Dyn.DI + 
#                   Dyn.slp + 
#                   Stat.slp,
#                 data = DataRampsStandardized)
# 
# ggord(dataRampAll.lda, as.factor(DataRampsStandardized$treatment))


DataAllStandardized<-allData[,c(2,5,47:62,66:80)]
DataAllStandardized<-allData[,c(2,5,10:25,29:43)]


dataRampTriAll.lda<- lda(treatment ~ Dyn.pfr + 
                  Stat.mSfr + 
                  Stat.afr + 
                  Stat.HzSTD + 
                  Stat.HzEnd + 
                  Dyn.IB+ 
                  Thr.T + 
                  Thr.F + 
                  Thr.L + 
                  Stat.LSpkT + 
                  Stat.LSpkF + 
                  Dyn.spkNum + 
                  Stat.spkNum + 
                  Dyn.DI + 
                  Dyn.slp + 
                  Stat.slp +
                  Dyn.F +
                  Dyn.pfr1 +
                  Dyn.pfr3 +
                  Thr.T1 +
                  Thr.F1 +
                  Thr.L1 +
                  Thr.T3 +
                  Thr.F3 +
                  Thr.L3 +
                  Dyn.slp1 +
                  Dyn.slp3 +
                  Dyn.spkNum1 +
                  Dyn.spkNum3 +
                  Dyn.RDR +
                  Dyn.IFRdrop, 
                data = DataAllStandardized)

### prediction check on all data
training_sample <- sample(c(TRUE, FALSE), nrow(DataAllStandardized), replace = T, prob = c(0.6,0.4))
train <- DataAllStandardized[training_sample, ]
test <- DataAllStandardized[!training_sample, ]

lda.test <- predict(dataRampTriAll.lda,test)
test$lda <- lda.test$class
table(test$lda,test$autoClass)

pred.train <- predict(dataRampTriAll.lda,train)$class
pred.test <- predict(dataRampTriAll.lda,test)$class
#accuracy on training data
mean(pred.train == train$treatment)
mean(pred.test == test$treatment)

#Plot the results
png(file="/Users/nickhousley/Desktop/Papers/Housley et al 2018 Nature Medicine/Neurophysiology/dataRampTriAll.lda.png",res = 600, width = 6000, height =  4800 ,bg = "transparent")
ggord(dataRampTriAll.lda, as.factor(DataAllStandardized$treatment), ellipse_pro =0.68, cols = c("#79AA55","#5586AA","#BA575F","#8655AA"), repel = TRUE, size=3, arrow=0.4, alpha_el=0.8)+
theme(legend.justification=c(1,1), legend.position=c(1,1), legend.box.just = c("top"), legend.background = element_rect(fill=alpha(0.9)), legend.direction = "horizontal",legend.box="horizontal")+
    ggtitle("All Neurons")+
  labs(x="Canonical Variable 1", y= "Canonical Variable 2")+
  #  ylim(-2,4)+
  #theme(panel.background = element_blank())+
  theme_bw()+
  theme(axis.text.x = element_text(face="bold", color="black", 
                                   size=15, angle=0),
        #axis.title.x = element_blank(),
        axis.text.y = element_text(face="bold", color="black", 
                                   size=15, angle=0),
      # axis.title.y = element_blank(),
        plot.title = element_text(face="bold", color="black", 
                                  size=15, angle=0),
        axis.title = element_text(face="bold", color="black", 
                                  size=15, angle=0),
        legend.position="none")
 
dev.off()

CanonicalVariables<-as.matrix(DataAllStandardized[,c(3:33)]) %*% as.matrix(dataRampTriAll.lda$scaling)
CanonicalVariables<-as.data.frame(CanonicalVariables)
CanonicalVariables$treatment<-DataAllStandardized$treatment


# Visualize: Specify the comparisons you want
my_comparisons <- list( c("1", "2"), c("1", "3"), c("1", "4"),c("2", "3"),c("2", "4"),c("3", "4") )

png(file="/Users/nickhousley/Desktop/Papers/Housley et al 2018 Nature Medicine/Neurophysiology/CanonicaltreatmentgroupsAllCanVar3.png",res = 600, width = 3000, height =  6000, bg = "transparent")

#CanonicaltreatmentgroupsAll<-

  ggplot(data = CanonicalVariables, 
                        aes(x=as.factor(treatment), 
                            y= LD3, 
                            #colour=interaction(autoClass,as.factor(treatment))))+ 
                            fill=as.factor(treatment)))+
  geom_boxplot(outlier.shape = NA, position = "dodge",varwidth = TRUE)+
  geom_point(alpha = 0.3, size = 3)+
  scale_fill_manual(values=c("#79AA55","#5586AA","#BA575F","#8655AA"),"Treatment")+

theme(legend.justification=c(1,1), legend.position=c(1,1), legend.box.just = c("top"), legend.background = element_rect(fill=alpha(0.4)), legend.direction = "horizontal",legend.box="horizontal")+
  xlab("Treatment")+

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
        legend.position="none")
  #stat_compare_means(method = "anova", label.y = 10)+ # Add pairwise comparisons p-value
  #  stat_compare_means(label.y = 3)     # Add global p-value
  #stat_compare_means(comparisons = my_comparisons, label = "p.signif", method = "t.test",
                   # ref.group = ".all.") 

dev.off()


####################### control classifier check
ControlDataCheck<-DataAllStandardized[DataAllStandardized$treatment == '1',] # subset only control
ControlDataCheck<-ControlDataCheck[,c(-1)] #remove treatment column


### LDA
ControlDataCheck_lda<- lda(autoClass ~ Dyn.pfr + 
                           Stat.mSfr + 
                           Stat.afr + 
                           Stat.HzSTD + 
                           Stat.HzEnd + 
                           Dyn.IB+ 
                           Thr.T + 
                           Thr.F + 
                           Thr.L + 
                           Stat.LSpkT + 
                           Stat.LSpkF + 
                           Dyn.spkNum + 
                           Stat.spkNum + 
                           Dyn.DI + 
                           Dyn.slp + 
                           Stat.slp +
                           Dyn.F +
                           Dyn.pfr1 +
                           Dyn.pfr3 +
                           Thr.T1 +
                           Thr.F1 +
                           Thr.L1 +
                           Thr.T3 +
                           Thr.F3 +
                           Thr.L3 +
                           Dyn.slp1 +
                           Dyn.slp3 +
                           Dyn.spkNum1 +
                           Dyn.spkNum3 +
                           Dyn.RDR +
                           Dyn.IFRdrop, 
                         data = ControlDataCheck)

training_sample <- sample(c(TRUE, FALSE), nrow(ControlDataCheck), replace = T, prob = c(0.6,0.4))
train <- ControlDataCheck[training_sample, ]
test <- ControlDataCheck[!training_sample, ]

lda.test <- predict(ControlDataCheck_lda,test)
test$lda <- lda.test$class
table(test$lda,test$autoClass)

pred.train <- predict(ControlDataCheck_lda,train)$class
pred.test <- predict(ControlDataCheck_lda,test)$class
#accuracy on training data
mean(pred.train == train$autoClass)
mean(pred.test == test$autoClass)








########################    Ia Neuron       ##################################

IaData<-read.csv(file="/Users/nickhousley/Desktop/Papers/Housley et al 2018 Nature Medicine/Neurophysiology/Data/Ia/Ia_Final_a1.csv", header=TRUE, sep=",", check.names = FALSE) # read in data and filter
# head(IaData)
# DataIaStandardizedRamps<-IaData[,c(2,5,47:62)]
# 
# dataRampIa.lda<- lda(treatment ~ Dyn.pfr +
#                   Stat.mSfr +
#                   Stat.afr +
#                   Stat.HzSTD +
#                   Stat.HzEnd +
#                   Dyn.IB+
#                   Thr.T +
#                   Thr.F +
#                   Thr.L +
#                   Stat.LSpkT +
#                   Stat.LSpkF +
#                   Dyn.spkNum +
#                   Stat.spkNum +
#                   Dyn.DI +
#                   Dyn.slp +
#                   Stat.slp,
#                 data = DataIaStandardizedRamps)
# 
# ggord(dataRampAll.lda, as.factor(DataRampsStandardized$treatment))


DataIaStandardized<-IaData[,c(2,5,47:62,66:80)]
DataIaStandardized<-IaData[,c(2,5,10:25,29:43)]

DataIaStandardized<-IaData[,c(2,5,83:113)]

head(DataIaStandardized,20)
dataRampTriIa.lda<- lda(treatment ~ Dyn.pfr + 
                           Stat.mSfr + 
                           Stat.afr + 
                           Stat.HzSTD + 
                           Stat.HzEnd + 
                           Dyn.IB+ 
                           Thr.T + 
                           Thr.F + 
                           Thr.L + 
                           Stat.LSpkT + 
                           Stat.LSpkF + 
                           Dyn.spkNum + 
                           Stat.spkNum + 
                           Dyn.DI + 
                           Dyn.slp + 
                           Stat.slp +
                           Dyn.F +
                           Dyn.pfr1 +
                           Dyn.pfr3 +
                           Thr.T1 +
                           Thr.F1 +
                           Thr.L1 +
                           Thr.T3 +
                           Thr.F3 +
                           Thr.L3 +
                           Dyn.slp1 +
                           Dyn.slp3 +
                           Dyn.spkNum1 +
                           Dyn.spkNum3 +
                           Dyn.RDR +
                           Dyn.IFRdrop, 
                         data = DataIaStandardized)


### prediction check on all data
training_sample <- sample(c(TRUE, FALSE), nrow(DataIaStandardized), replace = T, prob = c(0.6,0.4))
train <- DataIaStandardized[training_sample, ]
test <- DataIaStandardized[!training_sample, ]

lda.test <- predict(dataRampTriIa.lda,test)
test$lda <- lda.test$class
table(test$lda,test$treatment)

pred.train <- predict(dataRampTriIa.lda,train)$class
pred.test <- predict(dataRampTriIa.lda,test)$class
#accuracy on training data
mean(pred.train == train$treatment)
mean(pred.test == test$treatment)


#Plot the results
png(file="/Users/nickhousley/Desktop/Papers/Housley et al 2018 Nature Medicine/Neurophysiology/dataRampTriIa.lda.png",res = 600, width = 6000, height =  4800 ,bg = "transparent")

ggord(dataRampTriIa.lda, as.factor(DataIaStandardized$treatment), ellipse_pro =0.68, cols = c("#79AA55","#5586AA","#BA575F","#8655AA"), repel=TRUE)+
theme(legend.justification=c(1,1), legend.position=c(1,1), legend.box.just = c("top"), legend.background = element_rect(fill=alpha(0.9)), legend.direction = "horizontal",legend.box="horizontal")+
  ggtitle("Ia Neuron")+
  labs(x="Canonical Variable 1", y= "Canonical Variable 2")+
  #  ylim(-2,4)+
  #theme(panel.background = element_blank())+
  theme_transparent()+
  theme(axis.text.x = element_text(face="bold", color="black", 
                                   size=15, angle=0),
        axis.title.x = element_blank(),
        axis.text.y = element_text(face="bold", color="black", 
                                   size=15, angle=0),
        # axis.title.y = element_blank(),
        plot.title = element_text(face="bold", color="black", 
                                  size=15, angle=0),
        axis.title = element_text(face="bold", color="black", 
                                  size=15, angle=0),
        legend.position="none")

dev.off()


CanonicalVariablesIa<-as.matrix(DataIaStandardized[,c(3:33)]) %*% as.matrix(dataRampTriIa.lda$scaling)
CanonicalVariablesIa<-as.data.frame(CanonicalVariablesIa)
CanonicalVariablesIa$treatment<-DataIaStandardized$treatment


# Visualize: Specify the comparisons you want
my_comparisons <- list( c("1", "2"), c("1", "3"), c("1", "4"),c("2", "3"),c("2", "4"),c("3", "4") )

png(file="/Users/nickhousley/Desktop/Papers/Housley et al 2018 Nature Medicine/Neurophysiology/CanonicaltreatmentgroupsIaCanVar3.png",res = 600, width = 3000, height =  6000, bg = "transparent")

#CanonicaltreatmentgroupsIa<-
  ggplot(data = CanonicalVariablesIa, 
                                 aes(x=as.factor(treatment), 
                                     y= LD3, 
                                     #colour=interaction(autoClass,as.factor(treatment))))+ 
                                     fill=as.factor(treatment)))+
  geom_boxplot(outlier.shape = NA, position = "dodge",varwidth = TRUE)+
  geom_point(alpha = 0.3)+coord_cartesian()+
  scale_fill_manual(values=c("#79AA55","#5586AA","#BA575F","#8655AA"),"Treatment")+
theme(legend.justification=c(1,1), legend.position=c(1,1), legend.box.just = c("top"), legend.background = element_rect(fill=alpha(0.4)), legend.direction = "horizontal",legend.box="horizontal")+
  xlab("Treatment")+
  
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
        legend.position="none")
#stat_compare_means(method = "anova", label.y = 10)+ # Add pairwise comparisons p-value
#  stat_compare_means(label.y = 3)     # Add global p-value
#stat_compare_means(comparisons = my_comparisons, label = "p.signif", method = "t.test",
# ref.group = ".all.") 

dev.off()

########################    1.5 Neuron       ##################################

Data1.5<-read.csv(file="/Users/nickhousley/Desktop/Papers/Housley et al 2018 Nature Medicine/Neurophysiology/Data/1.5/1.5_final.csv", header=TRUE, sep=",", check.names = FALSE) # read in data and filter
# head(IaData)
# DataIaStandardizedRamps<-IaData[,c(2,5,47:62)]
# 
# dataRampIa.lda<- lda(treatment ~ Dyn.pfr +
#                   Stat.mSfr +
#                   Stat.afr +
#                   Stat.HzSTD +
#                   Stat.HzEnd +
#                   Dyn.IB+
#                   Thr.T +
#                   Thr.F +
#                   Thr.L +
#                   Stat.LSpkT +
#                   Stat.LSpkF +
#                   Dyn.spkNum +
#                   Stat.spkNum +
#                   Dyn.DI +
#                   Dyn.slp +
#                   Stat.slp,
#                 data = DataIaStandardizedRamps)
# 
# ggord(dataRampAll.lda, as.factor(DataRampsStandardized$treatment))


Data1.5Standardized<-Data1.5[,c(2,5,47:51, 53:62, 66:80)]

head(Data1.5Standardized)
Data1.5Standardized.lda<- lda(treatment ~ Dyn.pfr + 
                          Stat.mSfr + 
                          Stat.afr + 
                          Stat.HzSTD + 
                          Stat.HzEnd + 
                          #Dyn.IB+ 
                          Thr.T + 
                          Thr.F + 
                          Thr.L + 
                          Stat.LSpkT + 
                          Stat.LSpkF + 
                          Dyn.spkNum + 
                          Stat.spkNum + 
                          Dyn.DI + 
                          Dyn.slp + 
                          Stat.slp +
                          Dyn.F +
                          Dyn.pfr1 +
                          Dyn.pfr3 +
                          Thr.T1 +
                          Thr.F1 +
                          Thr.L1 +
                          Thr.T3 +
                          Thr.F3 +
                          Thr.L3 +
                          Dyn.slp1 +
                          Dyn.slp3 +
                          Dyn.spkNum1 +
                          Dyn.spkNum3 +
                          Dyn.RDR +
                          Dyn.IFRdrop, 
                        data = Data1.5Standardized)


### prediction check on all data
training_sample <- sample(c(TRUE, FALSE), nrow(Data1.5Standardized), replace = T, prob = c(0.6,0.4))
train <- Data1.5Standardized[training_sample, ]
test <- Data1.5Standardized[!training_sample, ]

lda.test <- predict(Data1.5Standardized.lda,test)
test$lda <- lda.test$class
table(test$lda,test$treatment)

pred.train <- predict(Data1.5Standardized.lda,train)$class
pred.test <- predict(Data1.5Standardized.lda,test)$class
#accuracy on training data
mean(pred.train == train$treatment)
mean(pred.test == test$treatment)


#Plot the results
ggord(Data1.5Standardized.lda, as.factor(Data1.5Standardized$treatment), ellipse_pro =0.68, cols = c("#79AA55","#5586AA","#BA575F","#8655AA"))

CanonicalVariables1.5<-as.matrix(Data1.5Standardized[,c(3:32)]) %*% as.matrix(Data1.5Standardized.lda$scaling)
CanonicalVariables1.5<-as.data.frame(CanonicalVariables1.5)
CanonicalVariables1.5$treatment<-Data1.5Standardized$treatment


# Visualize: Specify the comparisons you want
my_comparisons <- list( c("1", "2"), c("1", "3"), c("1", "4"),c("2", "3"),c("2", "4"),c("3", "4") )
Canonicaltreatmentgroups1.5<-ggplot(data = CanonicalVariables1.5, 
                                   aes(x=as.factor(treatment), 
                                       y= LD1, 
                                       #colour=interaction(autoClass,as.factor(treatment))))+ 
                                       fill=as.factor(treatment)))+
  geom_boxplot(outlier.shape = NA, position = "dodge",varwidth = TRUE)+
  geom_point(alpha = 0.3)+
  scale_fill_manual(values=c("#79AA55","#5586AA","#BA575F","#8655AA"),"Treatment")+
  theme(legend.justification=c(1,1), legend.position=c(1,1), legend.box.just = c("top"), legend.background = element_rect(fill=alpha(0.4)), legend.direction = "horizontal",legend.box="horizontal")+
  xlab("Treatment")+
  #  ylim(-2,4)+
  #theme(panel.background = element_blank())+
  theme_bw()+
  theme(axis.text.x = element_text(face="bold", color="black", 
                                   size=15, angle=0),
        axis.text.y = element_text(face="bold", color="black", 
                                   size=15, angle=0))+
  theme(axis.title = element_text(face="bold", color="black", 
                                  size=15, angle=0))+
  stat_compare_means(method = "anova", label.y = 10)+ # Add pairwise comparisons p-value
  #  stat_compare_means(label.y = 3)     # Add global p-value
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", method = "t.test",
                     ref.group = ".all.") 



########################    Ib Neuron       ##################################

DataIb<-read.csv(file="/Users/nickhousley/Desktop/Papers/Housley et al 2018 Nature Medicine/Neurophysiology/Data/Ib/Ib_final.csv", header=TRUE, sep=",", check.names = FALSE) # read in data and filter
# head(IaData)
# DataIaStandardizedRamps<-IaData[,c(2,5,47:62)]
# 
# dataRampIa.lda<- lda(treatment ~ Dyn.pfr +
#                   Stat.mSfr +
#                   Stat.afr +
#                   Stat.HzSTD +
#                   Stat.HzEnd +
#                   Dyn.IB+
#                   Thr.T +
#                   Thr.F +
#                   Thr.L +
#                   Stat.LSpkT +
#                   Stat.LSpkF +
#                   Dyn.spkNum +
#                   Stat.spkNum +
#                   Dyn.DI +
#                   Dyn.slp +
#                   Stat.slp,
#                 data = DataIaStandardizedRamps)
# 
# ggord(dataRampAll.lda, as.factor(DataRampsStandardized$treatment))


DataIbStandardized<-DataIb[,c(2,5,47:51, 53:62, 66:80)]
DataIbStandardized<-DataIb[,c(2,5,10:25,29:43)]


head(DataIbStandardized)
DataIbStandardized.lda<- lda(treatment ~ Dyn.pfr + 
                                Stat.mSfr + 
                                Stat.afr + 
                                Stat.HzSTD + 
                                Stat.HzEnd + 
                                #Dyn.IB+ 
                                Thr.T + 
                                Thr.F + 
                                Thr.L + 
                                Stat.LSpkT + 
                                Stat.LSpkF + 
                                Dyn.spkNum + 
                                Stat.spkNum + 
                                Dyn.DI + 
                                Dyn.slp + 
                                Stat.slp +
                                Dyn.F +
                                Dyn.pfr1 +
                                Dyn.pfr3 +
                                Thr.T1 +
                                Thr.F1 +
                                Thr.L1 +
                                Thr.T3 +
                                Thr.F3 +
                                Thr.L3 +
                                Dyn.slp1 +
                                Dyn.slp3 +
                                Dyn.spkNum1 +
                                Dyn.spkNum3 +
                                Dyn.RDR +
                                Dyn.IFRdrop, 
                              data = DataIbStandardized)


### prediction check on all data
training_sample <- sample(c(TRUE, FALSE), nrow(DataIbStandardized), replace = T, prob = c(0.6,0.4))
train <- DataIbStandardized[training_sample, ]
test <- DataIbStandardized[!training_sample, ]

lda.test <- predict(DataIbStandardized.lda,test)
test$lda <- lda.test$class
table(test$lda,test$treatment)

pred.train <- predict(DataIbStandardized.lda,train)$class
pred.test <- predict(DataIbStandardized.lda,test)$class
#accuracy on training data
mean(pred.train == train$treatment)
mean(pred.test == test$treatment)


#Plot the results
ggord(DataIbStandardized.lda, as.factor(DataIbStandardized$treatment),  cols = c("#79AA55","#5586AA","#BA575F","#8655AA"))

CanonicalVariablesIb<-as.matrix(DataIbStandardized[,c(3:32)]) %*% as.matrix(DataIbStandardized.lda$scaling)
CanonicalVariablesIb<-as.data.frame(CanonicalVariablesIb)
CanonicalVariablesIb$treatment<-DataIbStandardized$treatment


# Visualize: Specify the comparisons you want
my_comparisons <- list( c("1", "2"), c("1", "3"), c("1", "4"),c("2", "3"),c("2", "4"),c("3", "4") )

png(file="/Users/nickhousley/Desktop/Papers/Housley et al 2018 Nature Medicine/Neurophysiology/CanonicaltreatmentgroupsIbCanVar3.png",res = 600, width = 3000, height =  6000, bg = "transparent")

#CanonicaltreatmentgroupsIb<-
  ggplot(data = CanonicalVariablesIb, 
                                    aes(x=as.factor(treatment), 
                                        y= LD3, 
                                        #colour=interaction(autoClass,as.factor(treatment))))+ 
                                        fill=as.factor(treatment)))+
    geom_boxplot(outlier.shape = NA, position = "dodge",varwidth = TRUE)+
    geom_point(alpha = 0.3)+coord_cartesian()+
    scale_fill_manual(values=c("#79AA55","#5586AA","#BA575F","#8655AA"),"Treatment")+
    theme(legend.justification=c(1,1), legend.position=c(1,1), legend.box.just = c("top"), legend.background = element_rect(fill=alpha(0.4)), legend.direction = "horizontal",legend.box="horizontal")+
    xlab("Treatment")+
    
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
          legend.position="none")
  #stat_compare_means(method = "anova", label.y = 10)+ # Add pairwise comparisons p-value
  #  stat_compare_means(label.y = 3)     # Add global p-value
  #stat_compare_means(comparisons = my_comparisons, label = "p.signif", method = "t.test",
  # ref.group = ".all.") 
  
  dev.off()


########################    II Neuron       ##################################

DataII<-read.csv(file="/Users/nickhousley/Desktop/Papers/Housley et al 2018 Nature Medicine/Neurophysiology/Data/II/II_final.csv", header=TRUE, sep=",", check.names = FALSE) # read in data and filter
# head(IaData)
# DataIaStandardizedRamps<-IaData[,c(2,5,47:62)]
# 
# dataRampIa.lda<- lda(treatment ~ Dyn.pfr +  
#                   Stat.mSfr +
#                   Stat.afr +
#                   Stat.HzSTD +
#                   Stat.HzEnd +
#                   Dyn.IB+
#                   Thr.T +
#                   Thr.F +
#                   Thr.L +
#                   Stat.LSpkT +
#                   Stat.LSpkF +
#                   Dyn.spkNum +
#                   Stat.spkNum +
#                   Dyn.DI +
#                   Dyn.slp +
#                   Stat.slp,
#                 data = DataIaStandardizedRamps)
# 
# ggord(dataRampAll.lda, as.factor(DataRampsStandardized$treatment))


DataIIStandardized<-DataII[,c(2,5,47:51, 53:62, 66:80)]
DataIIStandardized<-DataII[,c(2,5,10:25,29:43)]

head(DataIIStandardized)
dim(DataIIStandardized)
DataIIStandardized.lda<- lda(treatment ~ Dyn.pfr + 
                               Stat.mSfr + 
                               Stat.afr + 
                               Stat.HzSTD + 
                               Stat.HzEnd + 
                               #Dyn.IB+ 
                               Thr.T + 
                               Thr.F + 
                               Thr.L + 
                               Stat.LSpkT + 
                               Stat.LSpkF + 
                               Dyn.spkNum + 
                               Stat.spkNum + 
                               Dyn.DI + 
                               Dyn.slp + 
                               Stat.slp +
                               Dyn.F +
                               Dyn.pfr1 +
                               Dyn.pfr3 +
                               Thr.T1 +
                               Thr.F1 +
                               Thr.L1 +
                               Thr.T3 +
                               Thr.F3 +
                               Thr.L3 +
                               Dyn.slp1 +
                               Dyn.slp3 +
                               Dyn.spkNum1 +
                               Dyn.spkNum3 +
                               Dyn.RDR +
                               Dyn.IFRdrop, 
                             data = DataIIStandardized)


### prediction check on all data
training_sample <- sample(c(TRUE, FALSE), nrow(DataIIStandardized), replace = T, prob = c(0.6,0.4))
train <- DataIIStandardized[training_sample, ]
test <- DataIIStandardized[!training_sample, ]

lda.test <- predict(DataIIStandardized.lda,test)
test$lda <- lda.test$class
table(test$lda,test$treatment)

pred.train <- predict(DataIIStandardized.lda,train)$class
pred.test <- predict(DataIIStandardized.lda,test)$class
#accuracy on training data
mean(pred.train == train$treatment)
mean(pred.test == test$treatment)


#Plot the results
ggord(DataIIStandardized.lda, as.factor(DataIIStandardized$treatment), ellipse_pro =0.68, cols = c("#79AA55","#5586AA","#BA575F","#8655AA"))

CanonicalVariablesII<-as.matrix(DataIIStandardized[,c(3:32)]) %*% as.matrix(DataIIStandardized.lda$scaling)
CanonicalVariablesII<-as.data.frame(CanonicalVariablesII)
CanonicalVariablesII$treatment<-DataIIStandardized$treatment


# Visualize: Specify the comparisons you want
my_comparisons <- list( c("1", "2"), c("1", "3"), c("1", "4"),c("2", "3"),c("2", "4"),c("3", "4") )
CanonicaltreatmentgroupsII<-ggplot(data = CanonicalVariablesII, 
                                   aes(x=as.factor(treatment), 
                                       y= LD1, 
                                       #colour=interaction(autoClass,as.factor(treatment))))+ 
                                       fill=as.factor(treatment)))+
  geom_boxplot(outlier.shape = NA, position = "dodge",varwidth = TRUE)+
  geom_point(alpha = 0.6, size = 2)+
  scale_fill_manual(values=c("#79AA55","#5586AA","#BA575F","#8655AA"),"Treatment")+
  theme(legend.justification=c(1,1), legend.position=c(1,1), legend.box.just = c("top"), legend.background = element_rect(fill=alpha(0.4)), legend.direction = "horizontal",legend.box="horizontal")+
  xlab("Treatment")+
  ggtitle("Group II")+
  #  ylim(-2,4)+
  #theme(panel.background = element_blank())+
  theme_bw()+
  theme(axis.text.x = element_text(face="bold", color="black", 
                                   size=15, angle=0),
        axis.title.x = element_blank(),
        axis.text.y = element_text(face="bold", color="black", 
                                   size=15, angle=0),
        axis.title.y = element_blank(),
        plot.title = element_text(face="bold", color="black", 
                                  size=15, angle=0),
        axis.title = element_text(face="bold", color="black", 
                                  size=15, angle=0),
        legend.position="none")+
  #stat_compare_means(method = "anova", label.y = 10)+ # Add pairwise comparisons p-value
  #  stat_compare_means(label.y = 3)     # Add global p-value
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", method = "t.test",
                     ref.group = ".all.") 




###########################
multiplot(CanonicaltreatmentgroupsIa,Canonicaltreatmentgroups1.5,CanonicaltreatmentgroupsIb,CanonicaltreatmentgroupsII, cols = 2)
