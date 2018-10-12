
rm(list=ls()) 

## @knitr All_Afferents_Treatments_Nature_2018_PCA_Data
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
library(knitr)

paletteNature2018 = c("#808080","#3e5ce0","#db0415","#933bd3")


allData<-read.csv(file="/Users/nickhousley/Desktop/Papers/Housley et al 2018 Nature Medicine/Neurophysiology/Data/All Afferents/231_afferents_MasterSheet.csv", header=TRUE, sep=",", check.names = FALSE) # read in data and filter
DataAllStandardized<-allData[,c(2,5,47:62,66:80)] # filter 
DataAllStandardized_pca<-DataAllStandardized[,c(-2)] #removes the autoClass column


DataAllStandardized.pca<-PCA(DataAllStandardized_pca[,-1],graph=T) #PCA

eig.val <- get_eigenvalue(DataAllStandardized.pca)

fviz_eig(DataAllStandardized.pca, addlabels = TRUE, ylim = c(0, 50)) # generate scree plot
var <- get_pca_var(DataAllStandardized.pca)

# Coordinates
head(var$coord)
# Cos2: quality on the factore map
head(var$cos2)
# Contributions to the principal components
head(var$contrib)

# Coordinates of variables
head(var$coord, 4)

#variable correlation plots
#Interpretation (following)
#-1 Positively correlated variables are grouped together.
#-2 Negatively correlated variables are positioned on opposite sides of the plot origin (opposed quadrants).
#-3 The distance between variables and the origin measures the quality of the variables on the factor map. Variables that are away from the origin are well represented on the factor map.
fviz_pca_var(DataAllStandardized.pca, col.var = "black")


#Quality of representation
head(var$cos2, 4)
corrplot(var$contrib, is.corr=FALSE)


# Total cos2 of variables on Dim.1 and Dim.2 (BarPlot)
fviz_cos2(DataAllStandardized.pca, choice = "var", axes = 1)


corrplot(var$contrib, is.corr=FALSE) 

# Contributions of variables to PC1
fviz_contrib(DataAllStandardized.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(DataAllStandardized.pca, choice = "var", axes = 2, top = 10)   

#Dimension description
res.desc <- dimdesc(DataAllStandardized.pca, axes = c(1,2), proba = 0.05)
# Description of dimension 1
res.desc$Dim.1
res.desc$Dim.2



## @knitr All_Afferents_Treatments_Nature_2018_PCA_Graph_1
fviz_pca_ind(DataAllStandardized.pca, axes=c(1,2),
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = as.factor(DataAllStandardized_pca$treatment), # color by groups
             palette = paletteNature2018,
             addEllipses = TRUE, ellipse.type = "norm", ellipse.level = 0.68,# Concentration ellipses
             legend.title = "Groups")

## @knitr All_Afferents_Treatments_Nature_2018_PCA_Graph_2
ind.p <- fviz_pca_ind(DataAllStandardized.pca, geom = "point", col.ind = as.factor(DataAllStandardized_pca$treatment))
ggpubr::ggpar(ind.p,
              title = "Principal Component Analysis",
              subtitle = "WT,OX,Pirc,POX",
              caption = "Source: factoextra",
              xlab = "PC1", ylab = "PC2",
              legend.title = "treatment", legend.position = "top",
              ggtheme = theme_gray(), palette = "jco")

## @knitr All_Afferents_Treatments_Nature_2018_PCA_Graph_3
fviz_pca_biplot(DataAllStandardized.pca, 
                col.ind = as.factor(DataAllStandardized_pca$treatment), palette = "jco", 
                addEllipses = TRUE, label = "var", ellipse.type="euclid",
                col.var = "black", repel = TRUE,
                legend.title = "treatment") 

## @knitr All_Afferents_Treatments_Nature_2018_PCA_Graph_clustering
fviz_pca_biplot(DataAllStandardized.pca, 
                # Fill individuals by groups
                geom.ind = "point",
                pointshape = 21,
                pointsize = 1.5,
                palette = paletteNature2018,
                alpha.var = 1,
                alpha.ind = 1,
                addEllipses = T, ellipse.type = "norm", ellipse.level = 0.68, ellipse.alpha=.5,
                fill.ind = as.factor(DataAllStandardized$treatment),
                col.ind = "black",
                col.var = factor(c("Dynamic","Static","Static","Static","Static",
                                   "Dynamic","Threshold","Threshold","Threshold",
                                   "Static","Static","Dynamic","Static",
                                   "Dynamic","Dynamic","Static","Dynamic","Dynamic","Dynamic","Threshold","Threshold","Threshold","Threshold","Threshold","Threshold","Dynamic","Dynamic","Dynamic","Dynamic","Dynamic","Dynamic")),
                legend.title = list(fill = "treatment", color = "Clusters"),
                repel = TRUE        # Avoid label overplotting
)+
#  ggpubr::fill_palette("paletteNature2018")+      # Indiviual fill color
  ggpubr::color_palette("npg")      # Variable colors


## @knitr All_Afferents_Treatments_Nature_2018_PCA_Graph_contribution_weighting
fviz_pca_biplot(DataAllStandardized.pca, 
                # Individuals
                geom.ind = "point",
                fill.ind = as.factor(DataAllStandardized$treatment),
                col.ind = "black",
                pointshape = 21, 
                pointsize = 2,
                palette = paletteNature2018,
                alpha.var = 1,
                alpha.ind = 1,
                addEllipses = T, ellipse.type = "norm", ellipse.level = 0.68, ellipse.alpha=.5,                # Variables
                alpha.var ="contrib", col.var = "contrib",
                gradient.cols = "RdYlBu",
                
                legend.title = list(fill = "treatment", color = "Contrib",
                                    alpha = "Contrib"))






### control PCA check to see if we can seperate similar to Vincent et al 2017
ControlDataCheck<-DataAllStandardized[DataAllStandardized$treatment == '1',]
ControlDataCheck<-ControlDataCheck[,c(-1)]
ControlDataCheck_pca<-PCA(ControlDataCheck[,-1],graph=T)


### plot PCA space  
fviz_pca_biplot(ControlDataCheck_pca, 
                # Fill individuals by groups
                geom.ind = "point",
                pointshape = 21,
                pointsize = 2.5,
                
                addEllipses = T, ellipse.type = "norm",ellipse.alpha = 0.5,
                fill.ind = as.factor(ControlDataCheck$autoClass),ellipse.level = 0.68,
                col.ind = "black",
                col.var = factor(c("Dynamic","Static","Static","Static","Static",
                                   "Dynamic","Threshold","Threshold","Threshold",
                                   "Static","Static","Dynamic","Static",
                                   "Dynamic","Dynamic","Static","Dynamic","Dynamic","Dynamic","Threshold","Threshold","Threshold","Threshold","Threshold","Threshold","Dynamic","Dynamic","Dynamic","Dynamic","Dynamic","Dynamic")),
                legend.title = list(fill = "autoClass", color = "Clusters"),
                repel = TRUE        # Avoid label overplotting
)+
  ggpubr::fill_palette("jco")+      # Indiviual fill color
  ggpubr::color_palette("npg")      # Variable colors

### control classifier check with 1.5 neuron class removed
ControlDataCheck_less1.5<-ControlDataCheck[!ControlDataCheck$autoClass == '1.5',]
ControlDataCheck<-ControlDataCheck[,c(-1)]
ControlDataCheck_less1.5_pca<-PCA(ControlDataCheck_less1.5[,-1],graph=T)


### plot PCA space  
fviz_pca_biplot(ControlDataCheck_less1.5_pca, 
                # Fill individuals by groups
                geom.ind = "point",
                pointshape = 21,
                pointsize = 2.5,
                addEllipses = T, ellipse.type = "norm",
                fill.ind = as.factor(ControlDataCheck_less1.5$autoClass),ellipse.level = 0.68,
                col.ind = "black",
                col.var = factor(c("Dynamic","Static","Static","Static","Static",
                                   "Dynamic","Threshold","Threshold","Threshold",
                                   "Static","Static","Dynamic","Static",
                                   "Dynamic","Dynamic","Static","Dynamic","Dynamic","Dynamic","Threshold","Threshold","Threshold","Threshold","Threshold","Threshold","Dynamic","Dynamic","Dynamic","Dynamic","Dynamic","Dynamic")),
                legend.title = list(fill = "autoClass", color = "Clusters"),
                repel = TRUE        # Avoid label overplotting
)+
  ggpubr::fill_palette("jco")+      # Indiviual fill color
  ggpubr::color_palette("npg")      # Variable colors

